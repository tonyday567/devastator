{-# LANGUAGE OverloadedStrings #-}

-- | NSE fiber: integrate the spectral system and emit a Post/JSONL tape.
--
-- Each integration step is one stamped post threading its predecessor, so the
-- tape encodes the causal chain exactly as the scalar toy does. The body
-- carries the full spectral state, which is what makes the fiber replayable:
-- a swapped box can be re-integrated from the seed and diffed.
module Devastator.Fiber
  ( FiberBody (..),
    bodyField,
    integrateFiber,
    frameFiber,
    readFiber,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing
  ( Jsonl (..),
    Stamped (..),
    These (..),
    frameStored,
    uncons,
  )
import Circuit.Parser.Json (decodeJson, encodeJson)
import Circuit.Parser.Json.Value (Json (..))
import Data.Complex
import Data.List (unfoldr)
import Data.Map.Strict qualified as Map
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import Devastator.Spectral (BilinearBox, SpectralField (..), fieldFromList, stepField)

-- | Body carried by each fiber post: time plus the full spectral state.
data FiberBody = FiberBody
  { fbTime :: Double,
    fbModes :: [((Int, Int), (Double, Double))]
  }
  deriving (Eq, Show)

-- | Reconstruct the spectral field from a body.
bodyField :: FiberBody -> SpectralField
bodyField = fieldFromList . map (fmap (uncurry (:+))) . fbModes

jdouble :: Double -> Json
jdouble = JNumber . fromFloatDigits

jint :: Int -> Json
jint = JNumber . fromInteger . toInteger

lookupDouble :: Text -> [(Text, Json)] -> Maybe Double
lookupDouble k o = case lookup k o of
  Just (JNumber s) -> Just (toRealFloat s)
  _ -> Nothing

encodeBody :: FiberBody -> Text
encodeBody b =
  decodeUtf8 $
    encodeJson $
      JObject
        [ ("time", jdouble (fbTime b)),
          ( "modes",
            JArray $
              V.fromList
                [ JArray (V.fromList [jint kx, jint ky, jdouble re, jdouble im])
                  | ((kx, ky), (re, im)) <- fbModes b
                ]
          )
        ]

decodeBody :: Text -> Maybe FiberBody
decodeBody t = do
  JObject o <- either (const Nothing) Just (decodeJson (encodeUtf8 t))
  time <- lookupDouble "time" o
  JArray ms <- lookup "modes" o
  modes <- traverse decodeMode (V.toList ms)
  pure (FiberBody time modes)
  where
    decodeMode (JArray v) = case V.toList v of
      [JNumber kx, JNumber ky, JNumber re, JNumber im] ->
        Just
          ( (round (toRealFloat kx :: Double), round (toRealFloat ky :: Double)),
            (toRealFloat re, toRealFloat im)
          )
      _ -> Nothing
    decodeMode _ = Nothing

-- | Integrate the spectral system, emitting one stamped post per step.
--
-- The seed post carries the initial field; step posts thread their immediate
-- predecessor. Explicit Euler, same schedule discipline as the scalar toy.
integrateFiber ::
  -- | truncation n
  Int ->
  BilinearBox ->
  -- | viscosity ν
  Double ->
  -- | dt
  Double ->
  -- | tEnd
  Double ->
  SpectralField ->
  [Stamped (Post FiberBody)]
integrateFiber nTrunc box nu dt tEnd seed = go 0 0.0 seed [seedPost]
  where
    fieldList = [((kx, ky), (realPart w, imagPart w)) | ((kx, ky), w) <- fieldToList seed]
    fieldToList (SpectralField m) = Map.toList m
    seedPost =
      Stamped
        0
        ""
        ( Post
            { from = "fiber-seed",
              to = ["fiber"],
              thread = [],
              body = FiberBody 0.0 fieldList
            }
        )
    go :: Int -> Double -> SpectralField -> [Stamped (Post FiberBody)] -> [Stamped (Post FiberBody)]
    go n t f acc
      | t >= tEnd - 1e-15 = reverse acc
      | otherwise =
          let t' = t + dt
              f' = stepField nTrunc dt box nu f
              postId = fromIntegral (n + 1)
              p =
                Post
                  { from = "fiber-step",
                    to = ["fiber"],
                    thread = [fromIntegral n],
                    body = FiberBody t' [((kx, ky), (realPart w, imagPart w)) | ((kx, ky), w) <- fieldToList f']
                  }
           in go (n + 1) t' f' (Stamped postId "" p : acc)

-- | Frame a fiber tape as JSON Lines.
frameFiber :: [Stamped (Post FiberBody)] -> Jsonl
frameFiber = Jsonl . map (frameStored . fmap encodePostBody)
  where
    encodePostBody p = p {body = encodeBody (body p)}

-- | Read a fiber tape back.
readFiber :: Jsonl -> [Stamped (Post FiberBody)]
readFiber = unfoldr unconsOne
  where
    unconsOne j = case uncons j of
      These sp rest -> Just (fmap decodePostBody sp, rest)
      This sp -> Just (fmap decodePostBody sp, Jsonl [])
      That _ -> Nothing
    decodePostBody p = case decodeBody (body p) of
      Just b -> p {body = b}
      Nothing -> error "readFiber: malformed FiberBody"

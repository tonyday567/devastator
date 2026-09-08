{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | NSE fiber: integrate the spectral system and emit a Post/JSONL tape.
--
-- Each integration step is one stamped post threading its predecessor, so the
-- tape encodes the causal chain exactly as the scalar toy does. The body
-- carries the full spectral state, which is what makes the fiber replayable:
-- a swapped box can be re-integrated from the seed and diffed.
module Devastator.Fiber
  ( FiberBody (..),
    bodyField,
    fiberRun,
    integrateFiber,
    frameFiber,
    readFiber,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, These (..), frameStored, pattern Stamped)
import Circuit.Parser.Json (decodeJson, encodeJson)
import Circuit.Parser.Json.Value (Json (..))
import Data.Complex
import Data.List (unfoldr)
import Data.Map.Strict qualified as Map
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import Devastator.Framing (Jsonl (..), epoch, uncons)
import Devastator.Run (Framed (..), Run (..), RunCfg (..), runTape)
import Devastator.Spectral (BilinearBox, SpectralField (..), fieldFromList, stepField)
import Devastator.Tape (frameTape, readTape)

-- | Body carried by each fiber post: time plus the full spectral state.
data FiberBody = FiberBody
  { fbTime :: Double,
    fbModes :: [((Int, Int), (Double, Double))]
  }
  deriving (Eq, Show)

-- | Reconstruct the spectral field from a body.
bodyField :: FiberBody -> SpectralField
bodyField = fieldFromList . map (fmap (uncurry (:+))) . fbModes

-- | The fiber as a closed cell: state @(t, field)@, observation the full
-- spectral state (which is what makes the fiber replayable — a swapped
-- box re-integrates from the same seed), tick one explicit Euler step.
fiberRun ::
  -- | truncation n
  Int ->
  BilinearBox ->
  -- | viscosity ν
  Double ->
  -- | dt
  Double ->
  Run (Double, SpectralField) FiberBody
fiberRun nTrunc box nu dt =
  Run
    (\(t, f) -> FiberBody t (fieldList f))
    (\_ (t, f) -> (t + dt, stepField nTrunc dt box nu f))
  where
    fieldList (SpectralField m) = [((kx, ky), (realPart w, imagPart w)) | ((kx, ky), w) <- Map.toList m]

fiberCfg :: RunCfg
fiberCfg = RunCfg "fiber-seed" "fiber-step" ["fiber"] ["fiber"]

-- | Integrate the spectral system, emitting one stamped post per step.
--
-- The seed post carries the initial field; step posts thread their
-- immediate predecessor. Explicit Euler, same schedule discipline as the
-- scalar toy.
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
  [Stamped FiberBody]
integrateFiber nTrunc box nu dt tEnd seed =
  runTape (\(t, _) -> t >= tEnd - 1e-15) fiberCfg (fiberRun nTrunc box nu dt) (0, seed)

-- * Codec

instance Framed FiberBody where
  frameBody = encodeBody
  unframeBody = decodeBody

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

-- | Frame a fiber tape as JSON Lines.
frameFiber :: [Stamped FiberBody] -> Jsonl
frameFiber = frameTape

-- | Read a fiber tape back.
readFiber :: Jsonl -> [Stamped FiberBody]
readFiber = readTape

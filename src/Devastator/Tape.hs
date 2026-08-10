{-# LANGUAGE OverloadedStrings #-}

-- | Framing for the toy devastator meeting.
--
-- The tape is JSON Lines of stamped 'Post's, exactly as 'Circuit.Agent.Framing'
-- defines it. The only extra work is serialising the 'ToyBody' record to and
-- from the body 'Text'.
module Devastator.Tape
  ( frameMeeting,
    readMeeting,
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
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.List (unfoldr)
import Devastator.Toy (ToyBody (..))

jdouble :: Double -> Json
jdouble = JNumber . fromFloatDigits

lookupDouble :: Text -> [(Text, Json)] -> Maybe Double
lookupDouble k o = case lookup k o of
  Just (JNumber s) -> Just (toRealFloat s)
  _ -> Nothing

encodeBody :: ToyBody -> Text
encodeBody b =
  decodeUtf8 $
    encodeJson $
      JObject
        [ ("time", jdouble (tbTime b)),
          ("value", jdouble (tbValue b)),
          ("flux", jdouble (tbFlux b))
        ]

decodeBody :: Text -> Maybe ToyBody
decodeBody t = do
  JObject o <- either (const Nothing) Just (decodeJson (encodeUtf8 t))
  time <- lookupDouble "time" o
  value <- lookupDouble "value" o
  flux <- lookupDouble "flux" o
  pure (ToyBody time value flux)

-- | Convert a toy meeting into a JSONL tape.
frameMeeting :: [Stamped (Post ToyBody)] -> Jsonl
frameMeeting =
  Jsonl . map (frameStored . fmap encodePostBody)
  where
    encodePostBody p = p {body = encodeBody (body p)}

-- | Read a JSONL tape back into a toy meeting.
readMeeting :: Jsonl -> [Stamped (Post ToyBody)]
readMeeting = unfoldr unconsOne
  where
    unconsOne j = case uncons j of
      These sp rest -> Just (fmap decodePostBody sp, rest)
      This sp -> Just (fmap decodePostBody sp, Jsonl [])
      That _ -> Nothing
    decodePostBody p = case decodeBody (body p) of
      Just b -> p {body = b}
      Nothing -> error "readMeeting: malformed ToyBody"

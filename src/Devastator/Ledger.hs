{-# LANGUAGE OverloadedStrings #-}

-- | Append-only verdict ledger for the devastator observatory.
--
-- The ledger is JSON Lines of verdict entries. Each entry cites the tape logs
-- it was computed from and the previous ledger entries it depends on, so the
-- whole ledger is replayable: given the ledger and the tape files it names, a
-- third party can re-run the certificate, recompute the significance, and
-- check that the recorded verdict is reproduced.
module Devastator.Ledger
  ( VerdictEntry (..),
    Ledger,
    emptyLedger,
    appendEntry,
    frameLedger,
    readLedger,
    frameEntry,
    parseEntry,
    recomputeVerdict,
  )
where

import Circuit.Agent (PostId)
import Circuit.Parser.Json (decodeJson, encodeJson)
import Circuit.Parser.Json.Value (Json (..))
import Data.List (unfoldr)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as Text.IO
import Data.Vector qualified as V
import Devastator.Cert (Certificate, maxValueCert, trivialCert, valueAtCert)
import Devastator.Framing (Jsonl (..))
import Devastator.Replay (separationSignificance)
import Devastator.Tape (readMeeting)
import Devastator.Verdict (Verdict (..))

-- | One row in the verdict ledger.
data VerdictEntry = VerdictEntry
  { veId :: PostId,
    veVerdict :: Verdict,
    veCertificate :: Text,
    veTrueOp :: Text,
    veNullOp :: Text,
    veTrueLog :: Text,
    veNullLog :: Text,
    veSignificance :: Double,
    veTolerance :: Double,
    veEpsilon :: Double,
    veThread :: [PostId]
  }
  deriving (Eq, Show)

-- | The ledger is an ordered list of entries, oldest first.
type Ledger = [VerdictEntry]

emptyLedger :: Ledger
emptyLedger = []

appendEntry :: VerdictEntry -> Ledger -> Ledger
appendEntry e ledger = ledger ++ [e]

-- | Render a ledger as JSON Lines, oldest entry first.
frameLedger :: Ledger -> Jsonl
frameLedger = Jsonl . map (decodeUtf8 . encodeJson . frameEntry)

-- | Read a ledger from JSON Lines.
readLedger :: Jsonl -> Ledger
readLedger (Jsonl lines') = unfoldr step lines'
  where
    step [] = Nothing
    step (l : ls) = case parseEntry l of
      Just e -> Just (e, ls)
      Nothing -> step ls

jdouble :: Double -> Json
jdouble = JNumber . fromFloatDigits

lookupText :: Text -> [(Text, Json)] -> Maybe Text
lookupText k o = case lookup k o of
  Just (JString t) -> Just t
  _ -> Nothing

lookupDouble :: Text -> [(Text, Json)] -> Maybe Double
lookupDouble k o = case lookup k o of
  Just (JNumber s) -> Just (toRealFloat s)
  _ -> Nothing

lookupPostIds :: Text -> [(Text, Json)] -> Maybe [PostId]
lookupPostIds k o = case lookup k o of
  Just (JArray vs) ->
    let sciToPostId (JNumber s) = Just (round (toRealFloat s :: Double) :: PostId)
        sciToPostId _ = Nothing
     in traverse sciToPostId (V.toList vs)
  _ -> Nothing

lookupVerdict :: Text -> [(Text, Json)] -> Maybe Verdict
lookupVerdict k o = lookupText k o >>= parseVerdict
  where
    parseVerdict "SKELETON" = Just Skeleton
    parseVerdict "SEPARATING" = Just Separating
    parseVerdict "UNDECIDED" = Just Undecided
    parseVerdict _ = Nothing

frameVerdict :: Verdict -> Json
frameVerdict = JString . verdictText
  where
    verdictText Skeleton = "SKELETON"
    verdictText Separating = "SEPARATING"
    verdictText Undecided = "UNDECIDED"

framePostIds :: [PostId] -> Json
framePostIds = JArray . V.fromList . map (JNumber . fromInteger . toInteger)

-- | Render one verdict entry as a JSON object.
frameEntry :: VerdictEntry -> Json
frameEntry e =
  JObject
    [ ("id", JNumber (fromInteger (toInteger (veId e)))),
      ("verdict", frameVerdict (veVerdict e)),
      ("certificate", JString (veCertificate e)),
      ("trueOp", JString (veTrueOp e)),
      ("nullOp", JString (veNullOp e)),
      ("trueLog", JString (veTrueLog e)),
      ("nullLog", JString (veNullLog e)),
      ("significance", jdouble (veSignificance e)),
      ("tolerance", jdouble (veTolerance e)),
      ("epsilon", jdouble (veEpsilon e)),
      ("thread", framePostIds (veThread e))
    ]

-- | Parse one verdict entry from JSON text.
parseEntry :: Text -> Maybe VerdictEntry
parseEntry t = do
  JObject o <- either (const Nothing) Just (decodeJson (encodeUtf8 t))
  id' <- lookupDouble "id" o
  verdict <- lookupVerdict "verdict" o
  cert <- lookupText "certificate" o
  trueOp' <- lookupText "trueOp" o
  nullOp' <- lookupText "nullOp" o
  trueLog <- lookupText "trueLog" o
  nullLog <- lookupText "nullLog" o
  sig <- lookupDouble "significance" o
  tol <- lookupDouble "tolerance" o
  eps <- lookupDouble "epsilon" o
  thread <- lookupPostIds "thread" o
  pure
    VerdictEntry
      { veId = round id',
        veVerdict = verdict,
        veCertificate = cert,
        veTrueOp = trueOp',
        veNullOp = nullOp',
        veTrueLog = trueLog,
        veNullLog = nullLog,
        veSignificance = sig,
        veTolerance = tol,
        veEpsilon = eps,
        veThread = thread
      }

-- | Recompute the verdict for an entry from its cited tape files.
--
-- Returns the recomputed verdict and significance. This is the replayability
-- oracle: the ledger is trustworthy only if the entries reproduce from the
-- named tapes.
recomputeVerdict :: VerdictEntry -> IO (Verdict, Double)
recomputeVerdict e = do
  trueJsonl <- readTapeFile (veTrueLog e)
  nullJsonl <- readTapeFile (veNullLog e)
  let trueMeeting = readMeeting trueJsonl
      nullMeeting = readMeeting nullJsonl
      cert = resolveCert (veCertificate e)
      sig = separationSignificance cert (veEpsilon e) trueMeeting nullMeeting
      v = classify (veTolerance e) (cert trueMeeting) (cert nullMeeting) sig
  pure (v, sig)
  where
    readTapeFile path =
      Jsonl . Text.lines <$> Text.IO.readFile (Text.unpack path)
    resolveCert "trivialCert" = trivialCert
    resolveCert "maxValueCert" = maxValueCert
    resolveCert name
      | "valueAtCert@" `Text.isPrefixOf` name =
          let t0 = read (Text.unpack (Text.drop 12 name))
           in valueAtCert t0
    resolveCert _ = trivialCert
    classify tol vTrue vNull sig
      | abs (vTrue - vNull) < tol = Skeleton
      | sig > 3.0 = Separating
      | otherwise = Undecided

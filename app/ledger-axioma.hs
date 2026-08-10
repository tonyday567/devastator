{-# LANGUAGE OverloadedStrings #-}

-- | Ledger-format axioma for the toy devastator.
--
-- Pins the ledger shape:
--   - verdict entries are JSON objects citing tape files and previous entries,
--   - the ledger is append-only JSON Lines,
--   - every entry can be replayed from its cited tapes and reproduces the
--     recorded verdict.
module Main (main) where

import Circuit.Agent.Framing (Jsonl (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as Text.IO
import Devastator.Cert (Certificate, maxValueCert, trivialCert, valueAtCert)
import Devastator.Ledger
  ( VerdictEntry (..),
    appendEntry,
    emptyLedger,
    frameLedger,
    readLedger,
    recomputeVerdict,
  )
import Devastator.Replay (separationSignificance)
import Devastator.Tape (frameMeeting)
import Devastator.Toy (BilinearOp (..), multiIntegrate, nullOp, trueOp)
import Devastator.Verdict (Verdict (..), verdictAbs, verdictRel)
import System.Directory (getTemporaryDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

approx :: Double -> Double -> Double -> Bool
approx tol a b = abs (a - b) < tol * (1 + abs a + abs b)

main :: IO ()
main = do
  putStrLn "devastator-ledger-axioma: verdict ledger shape + replayability"

  tmp <- getTemporaryDirectory
  let truePath = tmp </> "devastator-toy-true.jsonl"
      nullPath = tmp </> "devastator-toy-null.jsonl"

  -------------------------------------------------------------------------
  -- produce the two tape files
  -------------------------------------------------------------------------
  putStrLn "tape files"
  let trueMeeting = multiIntegrate [("a", trueOp, 1.0)]
      nullMeeting = multiIntegrate [("a", nullOp, 1.0)]
  Text.IO.writeFile truePath (Text.unlines (unJsonl (frameMeeting trueMeeting)))
  Text.IO.writeFile nullPath (Text.unlines (unJsonl (frameMeeting nullMeeting)))
  putStrLn ("  true tape: " ++ truePath)
  putStrLn ("  null tape: " ++ nullPath)

  -------------------------------------------------------------------------
  -- build three verdict entries and append them to a ledger
  -------------------------------------------------------------------------
  putStrLn "ledger entries"
  let entry0 =
        VerdictEntry
          { veId = 0,
            veVerdict = verdictAbs 1e-6 (trivialCert trueMeeting) (trivialCert nullMeeting),
            veCertificate = "trivialCert",
            veTrueOp = opTag trueOp,
            veNullOp = opTag nullOp,
            veTrueLog = Text.pack truePath,
            veNullLog = Text.pack nullPath,
            veSignificance = separationSignificance trivialCert 1e-6 trueMeeting nullMeeting,
            veTolerance = 1e-6,
            veEpsilon = 1e-6,
            veThread = []
          }
      entry1 =
        VerdictEntry
          { veId = 1,
            veVerdict = verdictRel 1e-3 (maxValueCert trueMeeting) (maxValueCert nullMeeting),
            veCertificate = "maxValueCert",
            veTrueOp = opTag trueOp,
            veNullOp = opTag nullOp,
            veTrueLog = Text.pack truePath,
            veNullLog = Text.pack nullPath,
            veSignificance = separationSignificance maxValueCert 1e-6 trueMeeting nullMeeting,
            veTolerance = 1e-3,
            veEpsilon = 1e-6,
            veThread = [0]
          }
      entry2 =
        VerdictEntry
          { veId = 2,
            veVerdict = Undecided,
            veCertificate = "valueAtCert@0.05",
            veTrueOp = opTag trueOp,
            veNullOp = opTag nullOp,
            veTrueLog = Text.pack truePath,
            veNullLog = Text.pack nullPath,
            veSignificance = separationSignificance (valueAtCert 0.05) 0.05 trueMeeting nullMeeting,
            veTolerance = 1e-3,
            veEpsilon = 0.05,
            veThread = [0, 1]
          }
      ledger = foldl (flip appendEntry) emptyLedger [entry0, entry1, entry2]
  assert "ledger has three entries" $ length ledger == 3
  assert "entry ids are sequential" $ map veId ledger == [0, 1, 2]
  assert "entry0 is SKELETON" $ veVerdict entry0 == Skeleton
  assert "entry1 is SEPARATING" $ veVerdict entry1 == Separating
  assert "entry2 is UNDECIDED" $ veVerdict entry2 == Undecided

  -------------------------------------------------------------------------
  -- round-trip the ledger through JSONL
  -------------------------------------------------------------------------
  putStrLn "ledger round-trip"
  let jsonl = frameLedger ledger
      ledgerBack = readLedger jsonl
  assert "round-tripped ledger has three entries" $ length ledgerBack == 3
  assert "round-tripped ledger matches original" $ ledgerBack == ledger

  -------------------------------------------------------------------------
  -- replayability: recompute each entry from its cited tapes
  -------------------------------------------------------------------------
  putStrLn "replayability"
  (v0, sig0) <- recomputeVerdict entry0
  (v1, sig1) <- recomputeVerdict entry1
  (v2, sig2) <- recomputeVerdict entry2
  assert "entry0 recomputes to SKELETON" $ v0 == Skeleton
  assert "entry1 recomputes to SEPARATING" $ v1 == Separating
  assert "entry2 recomputes to UNDECIDED" $ v2 == Undecided
  assert "entry0 significance reproduces" $ approx 1e-10 sig0 (veSignificance entry0)
  assert "entry1 significance reproduces" $ approx 1e-10 sig1 (veSignificance entry1)
  assert "entry2 significance reproduces" $ approx 1e-10 sig2 (veSignificance entry2)

  -------------------------------------------------------------------------
  -- append-only check: a fresh entry extends the ledger without mutation
  -------------------------------------------------------------------------
  putStrLn "append-only"
  let entry3 =
        VerdictEntry
          { veId = 3,
            veVerdict = Skeleton,
            veCertificate = "trivialCert",
            veTrueOp = opTag trueOp,
            veNullOp = opTag nullOp,
            veTrueLog = Text.pack truePath,
            veNullLog = Text.pack nullPath,
            veSignificance = 0.0,
            veTolerance = 1e-6,
            veEpsilon = 1e-6,
            veThread = [2]
          }
      ledger' = appendEntry entry3 ledger
  assert "append extends length" $ length ledger' == 4
  assert "original ledger unchanged" $ length ledger == 3
  assert "prefix of appended ledger is original" $ take 3 ledger' == ledger

  putStrLn "ALL PASS"

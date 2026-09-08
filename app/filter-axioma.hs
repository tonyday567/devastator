{-# LANGUAGE OverloadedStrings #-}

-- | Filter axioma: end-to-end devastator pipeline on the spectral fiber.
--
-- Runs the true 2D NSE fiber against the zero-box null, writes both tapes,
-- issues two certificates (total energy and shell energy), appends the
-- verdicts to a ledger, and verifies round-trip + replayability.
module Main (main) where

import Data.Complex
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Devastator.Fiber (frameFiber, integrateFiber)
import Devastator.Cert (shellEnergyCert, totalEnergyCert)
import Devastator.Filter (filterVerdict, recomputeFiberVerdict)
import Devastator.Framing (Jsonl (..))
import Devastator.Ledger
  ( VerdictEntry (..),
    appendEntry,
    emptyLedger,
    frameLedger,
    readLedger,
  )
import Devastator.Spectral (SpectralField, fieldFromList, spectralModes, trueNseBox, zeroBox)
import Devastator.Verdict (Verdict (..))
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

nTrunc :: Int
nTrunc = 3

dt0 :: Double
dt0 = 5e-4

tEnd0 :: Double
tEnd0 = 0.2

seedField :: SpectralField
seedField =
  fieldFromList $
    [(k, 0) | k <- spectralModes nTrunc]
      ++ [ ((1, 0), 0.8 :+ 0.1),
           ((-1, 0), 0.8 :+ (-0.1)),
           ((0, 1), 0.5 :+ 0.0),
           ((0, -1), 0.5 :+ 0.0),
           ((2, 1), 0.3 :+ 0.2),
           ((-2, -1), 0.3 :+ (-0.2))
         ]

main :: IO ()
main = do
  putStrLn "devastator-filter-axioma: true fiber vs zero-box null through the ledger"

  tmp <- getTemporaryDirectory
  let truePath = tmp </> "devastator-fiber-true.jsonl"
      nullPath = tmp </> "devastator-fiber-null.jsonl"
      trueText = Text.pack truePath
      nullText = Text.pack nullPath

  -------------------------------------------------------------------------
  -- run true and null fibers; write tapes
  -------------------------------------------------------------------------
  putStrLn "fiber runs"
  let trueLog = integrateFiber nTrunc trueNseBox 0.05 dt0 tEnd0 seedField
      nullLog = integrateFiber nTrunc zeroBox 0.05 dt0 tEnd0 seedField
  Text.IO.writeFile truePath (Text.unlines (unJsonl (frameFiber trueLog)))
  Text.IO.writeFile nullPath (Text.unlines (unJsonl (frameFiber nullLog)))
  putStrLn ("  true posts: " ++ show (length trueLog))
  putStrLn ("  null posts: " ++ show (length nullLog))

  -------------------------------------------------------------------------
  -- issue two verdicts and append to a ledger
  -------------------------------------------------------------------------
  putStrLn "verdicts"
  let entry0 =
        filterVerdict
          0
          "totalEnergyCert"
          totalEnergyCert
          trueNseBox
          zeroBox
          trueLog
          nullLog
          trueText
          nullText
          1e-3
          []
      entry1 =
        filterVerdict
          1
          "shellEnergyCert@4"
          (shellEnergyCert 4)
          trueNseBox
          zeroBox
          trueLog
          nullLog
          trueText
          nullText
          1e-3
          [0]
      ledger = foldl (flip appendEntry) emptyLedger [entry0, entry1]
  assert "ledger has two entries" $ length ledger == 2
  assert "entry0 is SKELETON (total energy)" $ veVerdict entry0 == Skeleton
  assert "entry1 is SEPARATING (shell energy)" $ veVerdict entry1 == Separating

  -------------------------------------------------------------------------
  -- ledger round-trip
  -------------------------------------------------------------------------
  putStrLn "ledger round-trip"
  let jsonl = frameLedger ledger
      ledgerBack = readLedger jsonl
  assert "round-tripped ledger has two entries" $ length ledgerBack == 2
  assert "round-tripped entries match" $ ledgerBack == ledger

  -------------------------------------------------------------------------
  -- replayability: recompute each verdict from its cited tapes
  -------------------------------------------------------------------------
  putStrLn "replayability"
  (v0, sig0) <- recomputeFiberVerdict entry0
  (v1, sig1) <- recomputeFiberVerdict entry1
  assert "entry0 recomputes to SKELETON" $ v0 == Skeleton
  assert "entry1 recomputes to SEPARATING" $ v1 == Separating
  assert "entry0 significance reproduces" $ approx 1e-10 sig0 (veSignificance entry0)
  assert "entry1 significance reproduces" $ approx 1e-10 sig1 (veSignificance entry1)

  -------------------------------------------------------------------------
  -- append-only check
  -------------------------------------------------------------------------
  putStrLn "append-only"
  let entry2 =
        VerdictEntry
          { veId = 2,
            veVerdict = Skeleton,
            veCertificate = "trivial",
            veTrueOp = "nse-2d",
            veNullOp = "zero",
            veTrueLog = trueText,
            veNullLog = nullText,
            veSignificance = 0.0,
            veTolerance = 1e-6,
            veEpsilon = 0.0,
            veThread = [1]
          }
      ledger' = appendEntry entry2 ledger
  assert "append extends length" $ length ledger' == 3
  assert "original ledger unchanged" $ length ledger == 2

  putStrLn "ALL PASS"

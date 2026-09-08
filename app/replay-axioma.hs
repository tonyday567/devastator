{-# LANGUAGE OverloadedStrings #-}

-- | Replay axioma for the toy devastator.
--
-- Pins three claims:
--   1. Unchanged cones reproduce exactly (same-operator replay).
--   2. Separation is significant only above a calibrated noise floor.
--   3. O5 linearization-invariance: independent cells can be serialized in any
--      topological order without changing observables.
module Main (main) where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, stamped)
import Devastator.Cert (maxValueCert, trivialCert, valueAtCert)
import Devastator.Replay
  ( linearizationInvariant,
    replayMeeting,
    separationSignificance,
    swapBox,
    unchangedCone,
  )
import Devastator.Toy (ToyBody (..), multiIntegrate, nullOp, trueOp)
import Devastator.Verdict (Verdict (..), verdictAbs, verdictRel)
import System.Exit (exitFailure)

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
  putStrLn "devastator-replay-axioma: replay, noise floor, and O5"

  -------------------------------------------------------------------------
  -- same-operator replay: unchanged cones reproduce exactly
  -------------------------------------------------------------------------
  putStrLn "unchanged-cone replay"
  let trueLog = multiIntegrate [("a", trueOp, 1.0)]
      trueReplay = replayMeeting trueOp trueLog
      nullLog = multiIntegrate [("a", nullOp, 1.0)]
      nullReplay = replayMeeting nullOp nullLog
  assert "true replay reproduces every body exactly" $
    unchangedCone trueLog trueReplay
  assert "null replay reproduces every body exactly" $
    unchangedCone nullLog nullReplay

  -------------------------------------------------------------------------
  -- swapped-box replay: same seeds, different operator
  -------------------------------------------------------------------------
  putStrLn "swapped-box replay"
  let trueAsNull = swapBox nullOp trueLog
      nullAsTrue = swapBox trueOp nullLog
  assert "true seeds replayed with null operator differ from true log" $
    not (unchangedCone trueLog trueAsNull)
  assert "null seeds replayed with true operator differ from null log" $
    not (unchangedCone nullLog nullAsTrue)

  -------------------------------------------------------------------------
  -- separation significance with calibrated noise floor
  -------------------------------------------------------------------------
  putStrLn "separation significance"
  let sigTriv = separationSignificance trivialCert 1e-6 trueLog nullLog
      sigMax = separationSignificance maxValueCert 1e-6 trueLog nullLog
      sigEarly = separationSignificance (valueAtCert 0.05) 0.05 trueLog nullLog
  putStrLn ("  trivial significance: " ++ show sigTriv)
  putStrLn ("  max |u| significance: " ++ show sigMax)
  putStrLn ("  early |u| significance (eps=0.05): " ++ show sigEarly)
  assert "trivialCert has zero separation / zero signal" $
    sigTriv < 1.0
  assert "maxValueCert separates far above noise floor" $
    sigMax > 100.0
  assert "valueAtCert at t=0.05 is not significant under coarse floor" $
    sigEarly > 0.1 && sigEarly < 10.0

  -------------------------------------------------------------------------
  -- verdict oracles driven by significance
  -------------------------------------------------------------------------
  putStrLn "verdict oracles"
  let vTriv = verdictAbs 1e-6 (trivialCert trueLog) (trivialCert nullLog)
      vMax = verdictRel 1e-3 (maxValueCert trueLog) (maxValueCert nullLog)
      vEarly =
        let cTrue = valueAtCert 0.05 trueLog
            cNull = valueAtCert 0.05 nullLog
            sig = separationSignificance (valueAtCert 0.05) 0.05 trueLog nullLog
         in if abs (cTrue - cNull) < 1e-3
              then Skeleton
              else if sig > 3.0 then Separating else Undecided
  putStrLn ("  trivial verdict: " ++ show vTriv)
  putStrLn ("  max verdict: " ++ show vMax)
  putStrLn ("  early-time verdict: " ++ show vEarly)
  assert "trivialCert is SKELETON" $ vTriv == Skeleton
  assert "maxValueCert is SEPARATING" $ vMax == Separating
  assert "valueAtCert at t=0.05 is UNDECIDED under coarse floor" $ vEarly == Undecided

  -------------------------------------------------------------------------
  -- O5 linearization-invariance: independent cells, permuted order
  -------------------------------------------------------------------------
  putStrLn "O5 linearization-invariance"
  let cellsA = [("a", trueOp, 1.0), ("b", trueOp, 0.5)]
      cellsB = [("b", trueOp, 0.5), ("a", trueOp, 1.0)]
      cellsNull = [("a", nullOp, 1.0), ("b", nullOp, 0.5)]
  assert "true multi-cell cert is invariant under cell-order permutation" $
    linearizationInvariant maxValueCert cellsA cellsB
  assert "null multi-cell cert is invariant under cell-order permutation" $
    linearizationInvariant maxValueCert cellsNull (reverse cellsNull)
  assert "certificate invariant for a trivial certificate" $
    linearizationInvariant trivialCert cellsA cellsB

  -------------------------------------------------------------------------
  -- round-trip sanity: replay logs frame through the same JSONL tape
  -------------------------------------------------------------------------
  putStrLn "round-trip"
  let logA = multiIntegrate cellsA
      logB = multiIntegrate cellsB
  assert "permuted logs have the same number of posts" $
    length logA == length logB
  assert "permuted logs have the same final times" $
    approx 1e-12 (tbTime . body . stamped $ last logA) (tbTime . body . stamped $ last logB)

  putStrLn "ALL PASS"

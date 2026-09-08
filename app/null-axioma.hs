{-# LANGUAGE OverloadedStrings #-}
-- Tape lists are nonempty by construction.
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Null axioma: the Katz–Pavlović dyadic model as a hostile null.
--
-- Pins:
--   - the KP model conserves weighted energy E = ½ Σ λ^{-n} a_n² to Euler drift;
--   - energy leaves shell 0 and propagates to higher shells;
--   - the cascade front reaches a fixed fraction of the retained shells in a
--     time that does not grow with N — the signature of finite-time blow-up
--     in the truncation limit;
--   - tape round-trip.
--
-- This is N1 in the null catalogue. The fixed truncation is a finite ODE and
-- does not literally blow up, but the tower behavior is the hostile signal.
module Main (main) where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, stamped)
import Data.List (findIndex)
import Devastator.Null.Dyadic
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

-- | Initial energy concentrated at shell 0.
seed :: Int -> [Double]
seed n = take n (1.0 : repeat 0.0)

run :: Int -> Double -> Double -> [DyadicBody]
run n dt tEnd = map (body . stamped) (integrateDyadic n dt tEnd (seed n))

-- | Highest shell whose weighted energy exceeds 1% of total energy.
energyFront :: DyadicBody -> Int
energyFront b =
  maximum
    ( [-1]
        ++ [ i
           | (i, a) <- zip [0 ..] (dbShells b),
             let w = 0.5 * (2.0 :: Double) ^^ (-i) * a * a
              in w > 0.01 * dyadicEnergy b
           ]
    )

-- | Physical time (step index * dt) at which the energy front first reaches
-- the target shell. Nothing if it never does within the run.
timeToFront :: Int -> Double -> Int -> Maybe Double
timeToFront n dt target =
  let bodies = run n dt 0.3
   in fmap (\k -> fromIntegral k * dt) (findIndex (\b -> energyFront b >= target) bodies)

main :: IO ()
main = do
  putStrLn "devastator-null-axioma: Katz-Pavlovic dyadic cascade"

  -------------------------------------------------------------------------
  -- single run: weighted energy conservation + cascade out of shell 0
  -------------------------------------------------------------------------
  putStrLn "single run N=6"
  let n1 = 6
      dt1 = 1e-4
      tEnd1 = 0.5
      bodies1 = run n1 dt1 tEnd1
      e0 = dyadicEnergy (head bodies1)
      e1 = dyadicEnergy (last bodies1)
      frac0start = dyadicShellEnergy 1 (head bodies1) / e0
      frac0end = dyadicShellEnergy 1 (last bodies1) / e1
  putStrLn ("  weighted energy start/end: " ++ show e0 ++ " / " ++ show e1)
  putStrLn ("  high-shell fraction start/end: " ++ show frac0start ++ " / " ++ show frac0end)
  assert "weighted energy conserved to Euler drift" $ approx 1e-3 e0 e1
  assert "energy leaves shell 0" $ frac0end > frac0start + 0.05

  -------------------------------------------------------------------------
  -- tower observation: cascade front speed vs truncation N
  -------------------------------------------------------------------------
  putStrLn "tower (cascade front time vs N)"
  let t6 = timeToFront 6 1e-4 (3 * 6 `div` 4)
      t8 = timeToFront 8 1e-6 (3 * 8 `div` 4)
  putStrLn ("  N=6 time to reach shell 4: " ++ maybe "never" show t6)
  putStrLn ("  N=8 time to reach shell 6: " ++ maybe "never" show t8)
  assert "N=6 cascade reaches 75% height" $ maybe False (< 0.3) t6
  assert "N=8 cascade reaches 75% height" $ maybe False (< 0.3) t8
  assert "front time does not grow with N" $
    case (t6, t8) of
      (Just a, Just b) -> abs (b - a) < 0.1 * max a b + 1e-3
      _ -> False

  -------------------------------------------------------------------------
  -- tape round-trip
  -------------------------------------------------------------------------
  putStrLn "tape round-trip"
  let log1 = integrateDyadic n1 dt1 tEnd1 (seed n1)
      back = readDyadic (frameDyadic log1)
      bodiesBack = map (body . stamped) back
  assert "same number of posts" $ length back == length log1
  assert "bodies round-trip" $ bodiesBack == bodies1

  putStrLn "ALL PASS"

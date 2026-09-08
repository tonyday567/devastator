{-# LANGUAGE OverloadedStrings #-}

-- | N2 axioma: the short-wave amplification null.
--
-- Pins:
--   - the ideal kernel (OpenAI Euler paper (4.21)) integrates stably and
--     amplifies with a measured 1/beta law (their (4.22) direction);
--   - deleting the (1 - beta * P0) coupling mutes the amplification
--     (mechanism-deletion witness);
--   - amplification is increasing in the initial slope lambda;
--   - the stage ledger: injected mass cannot separate null from control
--     (SKELETON, summable by construction) while compounded amplification
--     separates them (SEPARATING).
module Main (main) where

import Devastator.Null.ShortWave
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

base :: IdealCfg
base = IdealCfg 0.25 0 2

logAmpAt :: Int -> IdealCfg -> (IdealCfg -> Double -> WaveUV -> WaveUV) -> Double
logAmpAt n cfg f = fst (integrateIdeal cfg n f)

main :: IO ()
main = do
  putStrLn "devastator-shortwave-axioma: N2 short-wave amplification null"

  putStrLn "integration"
  let coarse = logAmpAt 10000 base idealTendency
      fine = logAmpAt 20000 base idealTendency
  putStrLn ("  log-amp(1e4): " ++ show coarse)
  putStrLn ("  log-amp(2e4): " ++ show fine)
  assert "log-amp stable under step halving" $ approx 1e-6 coarse fine

  putStrLn "beta law"
  let a0 = fine
      a1 = logAmpAt 20000 (IdealCfg (0.25 / 4) 0 2) idealTendency
      a2 = logAmpAt 20000 (IdealCfg (0.25 / 16) 0 2) idealTendency
      g0 = a0 * 0.25 / 2
      g1 = a1 * (0.25 / 4) / 2
      g2 = a2 * (0.25 / 16) / 2
  putStrLn ("  log-amp beta, beta/4, beta/16: " ++ show a0 ++ ", " ++ show a1 ++ ", " ++ show a2)
  putStrLn ("  1/beta law values: " ++ show g0 ++ ", " ++ show g1 ++ ", " ++ show g2)
  assert "amplification grows as beta shrinks" $ a1 > a0 && a2 > a1
  assert "log-amp scales like 1/beta toward a plateau" $
    g1 > g0 && g2 >= g1 && g2 < 1.6 * g0

  putStrLn "mutation"
  let mutLogAmp b = logAmpAt 20000 (IdealCfg b 0 2) mutedTendency
      gap0 = mutLogAmp 0.25 - fine
      gap2 = mutLogAmp (0.25 / 16) - a2
  putStrLn ("  coupling gap at beta, beta/16: " ++ show gap0 ++ ", " ++ show gap2)
  assert "coupling suppresses amplification at finite beta" $ gap0 > 0.05
  assert "coupling suppression saturates at an O(1) gap" $
    gap2 > 0.8 * gap0 && gap2 < 1.4 * gap0
  assert "kernels share the 1/beta plateau" $
    abs (g2 - mutLogAmp (0.25 / 16) * (0.25 / 16) / 2) < 0.05

  putStrLn "lambda"
  let lam1 = logAmpAt 20000 (IdealCfg 0.25 1 2) idealTendency
  putStrLn ("  log-amp(lambda=1): " ++ show lam1)
  assert "growth increasing in initial slope" $ lam1 >= fine - 1e-9

  putStrLn "ledger"
  let towerCfg = TowerCfg 0.25 0.25 6 20000
      nullT = runTower towerCfg
      ctlT = runTower towerCfg {tcBetaRatio = 1.0}
      massN = towerMass towerCfg
      massC = towerMass towerCfg {tcBetaRatio = 1.0}
  putStrLn ("  null log-amps: " ++ show (trLogAmps nullT))
  putStrLn ("  control log-amps: " ++ show (trLogAmps ctlT))
  putStrLn ("  mass null/control: " ++ show massN ++ " / " ++ show massC)
  putStrLn ("  compounded null/control: " ++ show (towerLogAmp nullT) ++ " / " ++ show (towerLogAmp ctlT))
  assert "mass certificate is SKELETON" $
    verdictAbs 1e-9 massN massC == Skeleton
  assert "amplification certificate is SEPARATING" $
    verdictRel 1e-3 (towerLogAmp ctlT) (towerLogAmp nullT) == Separating

  putStrLn "ALL PASS"

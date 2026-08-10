{-# LANGUAGE OverloadedStrings #-}
-- Tape lists are nonempty by construction (seed post plus step posts);
-- head/tail are total on that construction.
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Fiber axioma: the spectral NSE fiber emitting a Post/JSONL tape.
--
-- Pins:
--   - skeleton contract: bilinearity (polarization), reality symmetry
--     preserved, energy/enstrophy box pairings vanish for the true box.
--   - O1′ energy audit: the per-step Euler identity closes exactly, with the
--     defect measured from the tape, inviscid and viscous.
--   - audit convergence: the local defect is quadratic in dt.
--   - 2D positive control: viscous run flattens; inviscid run stays bounded.
--   - tape round-trip.
--   - O0 reading: total energy is a skeleton-level certificate in 2D — it
--     cannot separate the true box from the zero box — while shell energy
--     separates them. (Not a theorem-grade SKELETON verdict: the zero box is
--     a regular null, not a proven blow-up. The blow-up nulls come later.)
module Main (main) where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped (..))
import Data.Complex
import Data.Map.Strict qualified as Data.Map.Strict
import Devastator.Fiber (FiberBody (..), bodyField, frameFiber, integrateFiber, readFiber)
import Devastator.Spectral
import Devastator.Verdict (Verdict (..), verdictRel)
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

-- | Truncation, timestep, horizon.
nTrunc :: Int
nTrunc = 3

dt0 :: Double
dt0 = 5e-4

tEnd0 :: Double
tEnd0 = 0.2

-- | Deterministic seed with the reality symmetry built in.
seedField :: SpectralField
seedField =
  fieldFromList $
    [ (k, 0) | k <- spectralModes nTrunc ]
      ++ [ ((1, 0), 0.8 :+ 0.1),
           ((-1, 0), 0.8 :+ (-0.1)),
           ((0, 1), 0.5 :+ 0.0),
           ((0, -1), 0.5 :+ 0.0),
           ((2, 1), 0.3 :+ 0.2),
           ((-2, -1), 0.3 :+ (-0.2))
         ]

-- | Fields from a fiber tape, one per post.
tapeFields :: [Stamped (Post FiberBody)] -> [SpectralField]
tapeFields = map (bodyField . body . stamped)

main :: IO ()
main = do
  putStrLn "devastator-fiber-axioma: spectral NSE fiber, contract + O1' audit"

  -------------------------------------------------------------------------
  -- skeleton contract
  -------------------------------------------------------------------------
  putStrLn "skeleton contract"
  let u = seedField
      v = scaleField 0.7 (fieldFromList [((1, 1), 0.4 :+ 0.3), ((-1, -1), 0.4 :+ (-0.3)), ((2, -1), 0.2 :+ 0.1), ((-2, 1), 0.2 :+ (-0.1))])
      alpha = 1.3
      beta = -0.6
      -- polarization: B(x,y) = (N(x+y) - N(x) - N(y)) / 2
      bilin box x y =
        scaleField 0.5 $
          foldl1 addFields
            [ nonlinearTendency nTrunc box (addFields x y),
              scaleField (-1) (nonlinearTendency nTrunc box x),
              scaleField (-1) (nonlinearTendency nTrunc box y)
            ]
      lhs = bilin trueNseBox (addFields (scaleField alpha u) (scaleField beta v)) v
      rhs = addFields (scaleField alpha (bilin trueNseBox u v)) (scaleField beta (bilin trueNseBox v v))
      SpectralField lm = lhs
      SpectralField rm = rhs
      linErr = maximum [magnitude (w - Data.Map.Strict.findWithDefault 0 k rm) | (k, w) <- Data.Map.Strict.toList lm]
  assert "bilinearity: polarization is linear in the first argument" $ linErr < 1e-10
  assert "seed has the reality symmetry" $ conjSymmetryError seedField < 1e-15

  -------------------------------------------------------------------------
  -- runs: true box inviscid + viscous; zero box control
  -------------------------------------------------------------------------
  let runTrueInv = integrateFiber nTrunc trueNseBox 0.0 dt0 tEnd0 seedField
      runTrueVis = integrateFiber nTrunc trueNseBox 0.05 dt0 tEnd0 seedField
      runZeroVis = integrateFiber nTrunc zeroBox 0.05 dt0 tEnd0 seedField
  putStrLn ("  true inviscid posts: " ++ show (length runTrueInv))
  putStrLn ("  true viscous posts: " ++ show (length runTrueVis))

  -------------------------------------------------------------------------
  -- contract per step: reality symmetry + box pairings
  -------------------------------------------------------------------------
  putStrLn "contract per step"
  let fieldsInv = tapeFields runTrueInv
      fieldsVis = tapeFields runTrueVis
      maxSymErr = maximum (map conjSymmetryError fieldsInv)
      maxPairE = maximum (map (abs . energyNonlinPairing nTrunc trueNseBox) fieldsInv)
      maxPairZ = maximum (map (abs . enstrophyNonlinPairing nTrunc trueNseBox) fieldsInv)
  putStrLn ("  max reality-symmetry error: " ++ show maxSymErr)
  putStrLn ("  max |energy pairing|: " ++ show maxPairE)
  putStrLn ("  max |enstrophy pairing|: " ++ show maxPairZ)
  assert "reality symmetry preserved along the run" $ maxSymErr < 1e-12
  assert "energy box pairing vanishes (contract)" $ maxPairE < 1e-10
  assert "enstrophy box pairing vanishes (2D invariant)" $ maxPairZ < 1e-10

  -------------------------------------------------------------------------
  -- O1′ energy audit: exact per-step Euler identity, measured from the tape
  -------------------------------------------------------------------------
  putStrLn "O1' energy audit"
  let audit box nu fs =
        [ (e1 - e0) - dt0 * (energyNonlinPairing nTrunc box f0 - 2 * nu * enstrophy f0) - 0.5 * dt0 * dt0 * tendencyEnergyNorm nTrunc box nu f0
          | (f0, f1) <- zip fs (tail fs),
            let e0 = energy f0,
            let e1 = energy f1
        ]
      auditErrsInv = audit trueNseBox 0.0 fieldsInv
      auditErrsVis = audit trueNseBox 0.05 fieldsVis
      maxAuditInv = maximum (map abs auditErrsInv)
      maxAuditVis = maximum (map abs auditErrsVis)
  putStrLn ("  max inviscid audit residual: " ++ show maxAuditInv)
  putStrLn ("  max viscous audit residual: " ++ show maxAuditVis)
  assert "inviscid energy audit closes exactly (defect measured)" $ maxAuditInv < 1e-10
  assert "viscous energy audit closes exactly (defect measured)" $ maxAuditVis < 1e-10

  -------------------------------------------------------------------------
  -- enstrophy audit: same identity in the enstrophy norm
  -------------------------------------------------------------------------
  putStrLn "enstrophy audit"
  let auditZ box nu fs =
        [ (z1 - z0) - dt0 * (enstrophyNonlinPairing nTrunc box f0 - 2 * nu * palinstrophy f0) - 0.5 * dt0 * dt0 * tendencyEnstrophyNorm nTrunc box nu f0
          | (f0, f1) <- zip fs (tail fs),
            let z0 = enstrophy f0,
            let z1 = enstrophy f1
        ]
      maxAuditZ = maximum (map abs (auditZ trueNseBox 0.05 fieldsVis))
  putStrLn ("  max viscous enstrophy audit residual: " ++ show maxAuditZ)
  assert "viscous enstrophy audit closes exactly" $ maxAuditZ < 1e-10

  -------------------------------------------------------------------------
  -- audit convergence: local defect is quadratic in dt
  -------------------------------------------------------------------------
  putStrLn "audit convergence"
  let f0 = last (take 10 fieldsInv)
      e0 = energy f0
      defect h =
        let f1 = stepField nTrunc h trueNseBox 0.0 f0
         in abs ((energy f1 - e0) - h * energyNonlinPairing nTrunc trueNseBox f0)
      ratio = defect dt0 / defect (dt0 / 2)
  putStrLn ("  local defect ratio dt vs dt/2: " ++ show ratio)
  assert "local energy defect is quadratic in dt" $ ratio > 3.6 && ratio < 4.4

  -------------------------------------------------------------------------
  -- 2D positive control: must flatten
  -------------------------------------------------------------------------
  putStrLn "2D positive control"
  let energiesVis = map energy fieldsVis
      energiesInv = map energy fieldsInv
      monotone = and (zipWith (\a b -> b <= a + 1e-12) energiesVis (tail energiesVis))
  assert "viscous energy decays monotonically" monotone
  assert "inviscid energy is conserved to Euler drift" $
    approx 1e-3 (last energiesInv) (head energiesInv)
  assert "viscous energy strictly decreases" $ last energiesVis < head energiesVis

  -------------------------------------------------------------------------
  -- tape round-trip
  -------------------------------------------------------------------------
  putStrLn "tape round-trip"
  let back = readFiber (frameFiber runTrueVis)
      fieldsBack = tapeFields back
      SpectralField fa = last fieldsVis
      SpectralField fb = last fieldsBack
      rtErr = maximum [magnitude (w - Data.Map.Strict.findWithDefault 0 k fb) | (k, w) <- Data.Map.Strict.toList fa]
  assert "same number of posts" $ length back == length runTrueVis
  assert "final field round-trips" $ rtErr < 1e-12

  -------------------------------------------------------------------------
  -- O0 reading: total energy is skeleton-level in 2D; shell energy separates
  -------------------------------------------------------------------------
  putStrLn "O0 reading: energy cert vs shell cert"
  let fieldsZero = tapeFields runZeroVis
      totalEnergyCert fs = energy (last fs)
      shellCert fs =
        let SpectralField m = last fs
         in 0.5 * sum [magnitude w ^ (2 :: Int) / norm2 k | (k, w) <- Data.Map.Strict.toList m, norm2 k > 4]
      vTotal = verdictRel 1e-3 (totalEnergyCert fieldsVis) (totalEnergyCert fieldsZero)
      vShell = verdictRel 1e-3 (shellCert fieldsVis) (shellCert fieldsZero)
  putStrLn ("  total energy true/zero: " ++ show (totalEnergyCert fieldsVis) ++ " / " ++ show (totalEnergyCert fieldsZero))
  putStrLn ("  shell |k|^2>4 energy true/zero: " ++ show (shellCert fieldsVis) ++ " / " ++ show (shellCert fieldsZero))
  assert "total energy cannot separate true box from zero box (skeleton-level)" $
    vTotal == Skeleton
  assert "shell energy separates true box from zero box" $
    vShell == Separating

  putStrLn "ALL PASS"

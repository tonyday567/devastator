{-# LANGUAGE OverloadedStrings #-}

-- | 2D spectral Galerkin fiber: vorticity-form Navier–Stokes on a periodic
-- box.
--
-- The field is stored as one scalar vorticity mode per kept wavevector.
-- Divergence freedom is by representation: the velocity reconstructs as
-- @û_k = i (ky, -kx) ω_k \/ |k|²@, which satisfies @k·û_k = 0@ identically.
-- Reality of the physical field is the symmetry @ω_{-k} = conj(ω_k)@.
--
-- The Galerkin truncation keeps modes @max(|kx|,|ky|) <= n@ and evolves
--
-- @dω_k\/dt = -ν|k|² ω_k - Σ_{p+q=k} coeff(p,q) ω_p ω_q@
--
-- with the true NSE box @coeff(p,q) = (p×q)\/|p|²@. The box is a parameter so
-- hostile nulls (dyadic, Tao-averaged, zero) can be swapped in under the same
-- skeleton contract.
--
-- Audit identities are exact algebra for the explicit Euler step. With full
-- tendency @f = N - ν|k|²ω@:
--
-- @E' - E = dt ⟨u,f⟩_E + (dt²\/2) |f|²_E@
-- @Z' - Z = dt ⟨ω,f⟩_Z + (dt²\/2) |f|²_Z@
--
-- so every per-step residual is exactly measurable from the tape, and the
-- cutoff flux is zero by Galerkin construction.
module Devastator.Spectral
  ( SpectralField (..),
    BilinearBox (..),
    trueNseBox,
    zeroBox,
    spectralModes,
    zeroField,
    fieldFromList,
    cross2,
    norm2,
    addFields,
    scaleField,
    nonlinearTendency,
    viscousTendency,
    tendency,
    stepField,
    energy,
    enstrophy,
    palinstrophy,
    energyNonlinPairing,
    enstrophyNonlinPairing,
    tendencyEnergyNorm,
    tendencyEnstrophyNorm,
    conjSymmetryError,
  )
where

import Data.Complex
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Scalar vorticity modes, one per kept wavevector, mean mode excluded.
newtype SpectralField = SpectralField {unSpectralField :: Map (Int, Int) (Complex Double)}
  deriving (Eq, Show)

-- | The swappable nonlinearity box: interaction coefficient for the triad
-- @p + q = k@. Only this coefficient distinguishes the true operator from a
-- null wearing its skeleton.
data BilinearBox = BilinearBox
  { boxTag :: Text,
    boxCoeff :: (Int, Int) -> (Int, Int) -> Double
  }

-- | 2D cross product of wavevectors.
cross2 :: (Int, Int) -> (Int, Int) -> Double
cross2 (px, py) (qx, qy) = fromIntegral (px * qy - py * qx)

-- | Squared wavevector magnitude.
norm2 :: (Int, Int) -> Double
norm2 (kx, ky) = fromIntegral (kx * kx + ky * ky)

-- | True 2D Navier–Stokes vorticity interaction.
trueNseBox :: BilinearBox
trueNseBox = BilinearBox "nse-2d" (\p q -> cross2 p q / norm2 p)

-- | Degenerate null: no nonlinear interaction at all. A skeleton-contract
-- member (energy cancellation holds trivially) with no cascade. Used to
-- show that total-energy certificates read only skeleton-level data in 2D.
zeroBox :: BilinearBox
zeroBox = BilinearBox "zero" (\_ _ -> 0.0)

-- | Kept modes: @max(|kx|,|ky|) <= n@, mean mode excluded.
spectralModes :: Int -> [(Int, Int)]
spectralModes n =
  [ (kx, ky)
  | kx <- [-n .. n],
    ky <- [-n .. n],
    (kx, ky) /= (0, 0)
  ]

zeroField :: Int -> SpectralField
zeroField n = SpectralField (Map.fromList [(k, 0) | k <- spectralModes n])

fieldFromList :: [((Int, Int), Complex Double)] -> SpectralField
fieldFromList = SpectralField . Map.fromList

addFields :: SpectralField -> SpectralField -> SpectralField
addFields (SpectralField a) (SpectralField b) = SpectralField (Map.unionWith (+) a b)

scaleField :: Double -> SpectralField -> SpectralField
scaleField a (SpectralField f) = SpectralField (Map.map ((a :+ 0) *) f)

-- | Nonlinear vorticity tendency @N_k = -Σ_{p+q=k} coeff(p,q) ω_p ω_q@.
--
-- The tendency is defined on the whole kept grid @spectralModes n@, not just
-- the support of the input: nonlinearity excites modes that are zero in the
-- input, and the truncation set is what "well-defined on the fiber" means.
nonlinearTendency :: Int -> BilinearBox -> SpectralField -> SpectralField
nonlinearTendency n box (SpectralField f) =
  SpectralField (Map.fromList [(k, go k) | k <- spectralModes n])
  where
    go k = negate $ Map.foldlWithKey' addTriad 0 f
      where
        addTriad acc p wp = case Map.lookup (subMode k p) f of
          Nothing -> acc
          Just wq -> acc + (boxCoeff box p (subMode k p) :+ 0) * wp * wq
    subMode (a, b) (c, d) = (a - c, b - d)

-- | Viscous vorticity tendency @-ν|k|² ω_k@, on the whole kept grid.
viscousTendency :: Int -> Double -> SpectralField -> SpectralField
viscousTendency n nu (SpectralField f) =
  SpectralField
    ( Map.fromList
        [ (k, ((-nu * norm2 k) :+ 0) * Map.findWithDefault 0 k f)
        | k <- spectralModes n
        ]
    )

tendency :: Int -> BilinearBox -> Double -> SpectralField -> SpectralField
tendency n box nu f = addFields (nonlinearTendency n box f) (viscousTendency n nu f)

-- | Explicit Euler step.
stepField :: Int -> Double -> BilinearBox -> Double -> SpectralField -> SpectralField
stepField n dt box nu f = addFields (onGrid n f) (scaleField dt (tendency n box nu f))
  where
    onGrid m (SpectralField g) =
      SpectralField (Map.fromList [(k, Map.findWithDefault 0 k g) | k <- spectralModes m])

-- | Energy @E = ½ Σ |ω_k|²\/|k|²@.
energy :: SpectralField -> Double
energy (SpectralField f) =
  0.5 * sum [magnitude w ^ (2 :: Int) / norm2 k | (k, w) <- Map.toList f]

-- | Enstrophy @Z = ½ Σ |ω_k|²@.
enstrophy :: SpectralField -> Double
enstrophy (SpectralField f) =
  0.5 * sum [magnitude w ^ (2 :: Int) | (_, w) <- Map.toList f]

-- | Palinstrophy @P = ½ Σ |k|² |ω_k|²@ — half the enstrophy dissipation rate.
palinstrophy :: SpectralField -> Double
palinstrophy (SpectralField f) =
  0.5 * sum [norm2 k * magnitude w ^ (2 :: Int) | (k, w) <- Map.toList f]

-- | Energy pairing of the nonlinear box with the field:
-- @Re Σ conj(ω_k) N_k \/ |k|²@. The skeleton contract demands this vanish.
energyNonlinPairing :: Int -> BilinearBox -> SpectralField -> Double
energyNonlinPairing n box f =
  let SpectralField nt = nonlinearTendency n box f
      SpectralField m = f
   in realPart $ sum [conjugate w * Map.findWithDefault 0 k nt / (norm2 k :+ 0) | (k, w) <- Map.toList m]

-- | Enstrophy pairing of the nonlinear box: @Re Σ conj(ω_k) N_k@. Vanishes
-- for the true 2D box (enstrophy is an inviscid invariant in 2D).
enstrophyNonlinPairing :: Int -> BilinearBox -> SpectralField -> Double
enstrophyNonlinPairing n box f =
  let SpectralField nt = nonlinearTendency n box f
      SpectralField m = f
   in realPart $ sum [conjugate w * Map.findWithDefault 0 k nt | (k, w) <- Map.toList m]

-- | Energy norm of the full tendency: @Σ |f_k|² \/ |k|²@. The exact Euler
-- energy defect per step is @(dt²\/2)@ times this.
tendencyEnergyNorm :: Int -> BilinearBox -> Double -> SpectralField -> Double
tendencyEnergyNorm n box nu f =
  let SpectralField t = tendency n box nu f
   in sum [magnitude w ^ (2 :: Int) / norm2 k | (k, w) <- Map.toList t]

-- | Enstrophy norm of the full tendency: @Σ |f_k|²@.
tendencyEnstrophyNorm :: Int -> BilinearBox -> Double -> SpectralField -> Double
tendencyEnstrophyNorm n box nu f =
  let SpectralField t = tendency n box nu f
   in sum [magnitude w ^ (2 :: Int) | (_, w) <- Map.toList t]

-- | Largest violation of the reality symmetry @ω_{-k} = conj(ω_k)@.
conjSymmetryError :: SpectralField -> Double
conjSymmetryError (SpectralField f) =
  maximum $
    0
      : [ magnitude (w - conjugate wneg)
        | ((kx, ky), w) <- Map.toList f,
          let wneg = Map.findWithDefault 0 (-kx, -ky) f
        ]

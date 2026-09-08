-- | N2 null: short-wave amplification, from the OpenAI Euler paper.
--
-- Source of structure: "Finite time blowup for the Euler equation"
-- (OpenAI), the ideal velocity system (4.20)-(4.21). The leading transverse
-- velocity (U, V) of a localized packet in the frozen parent frame
-- satisfies, with P0 = x^2, D = 1 + P0^2, x = beta * tau,
--
-- @ (D V')' = 2 (1 - beta * P0) V,   U = -V' @
--
-- integrated in tau with V(0) = 1, V'(0) = lambda. The null is the stage
-- tower whose beta_j shrink, so per-stage amplification compounds while
-- the injected increments stay summable; the control tower holds beta
-- fixed. The coupling term @beta * P0@ is the mechanism: deleting it
-- ('mutedTendency') mutes the amplification.
module Devastator.Null.ShortWave
  ( WaveUV (..),
    IdealCfg (..),
    idealTendency,
    mutedTendency,
    integrateIdeal,
    TowerCfg (..),
    TowerResult (..),
    runTower,
    towerMass,
    towerLogAmp,
  )
where

import Data.List (foldl')

-- | Transverse velocity pair (V, U) with U = -V'.
data WaveUV = WaveUV {wvV :: Double, wvU :: Double}
  deriving (Eq, Show)

-- | Ideal-system configuration: shear parameter beta, initial slope
-- lambda (V'(0)), and transfer target x = xtar.
data IdealCfg = IdealCfg
  { icBeta :: Double,
    icLambda :: Double,
    icXtar :: Double
  }
  deriving (Eq, Show)

-- | tau-derivative at stage time tau, x = beta * tau: the full kernel
-- (4.21) in first-order form.
idealTendency :: IdealCfg -> Double -> WaveUV -> WaveUV
idealTendency cfg tau (WaveUV v u) =
  WaveUV (-u) (- (2 * (1 - b * x * x) * v / d) - 4 * b * x * x * x * u / d)
  where
    b = icBeta cfg
    x = b * tau
    d = 1 + x ^ 4

-- | Mechanism deletion: the coupling (1 - beta * P0) flattened to 1.
-- Everything else identical; the mutation-review witness for N2.
mutedTendency :: IdealCfg -> Double -> WaveUV -> WaveUV
mutedTendency cfg tau (WaveUV v u) =
  WaveUV (-u) (- (2 * v / d) - 4 * b * x * x * x * u / d)
  where
    b = icBeta cfg
    x = b * tau
    d = 1 + x ^ 4

addW :: WaveUV -> WaveUV -> WaveUV
addW (WaveUV a b) (WaveUV c d) = WaveUV (a + c) (b + d)

scaleW :: Double -> WaveUV -> WaveUV
scaleW k (WaveUV a b) = WaveUV (k * a) (k * b)

rk4 :: Double -> (Double -> WaveUV -> WaveUV) -> Double -> WaveUV -> WaveUV
rk4 dt f t s =
  addW s (scaleW (dt / 6) (addW k1 (addW (scaleW 2 k2) (addW (scaleW 2 k3) k4))))
  where
    k1 = f t s
    k2 = f (t + dt / 2) (addW s (scaleW (dt / 2) k1))
    k3 = f (t + dt / 2) (addW s (scaleW (dt / 2) k2))
    k4 = f (t + dt) (addW s (scaleW dt k3))

-- | Integrate the ideal system to xtar and return the log-amplification
-- log |V(xtar)/V(0)| with the final direction. The state is renormalized
-- each step (the ODE is linear, so this splits scale exactly) keeping the
-- integration in range when the amplification overflows Double.
integrateIdeal ::
  IdealCfg ->
  -- | steps
  Int ->
  (IdealCfg -> Double -> WaveUV -> WaveUV) ->
  (Double, WaveUV)
integrateIdeal cfg n f = go n 0 0 (WaveUV 1 (-icLambda cfg))
  where
    dt = icXtar cfg / icBeta cfg / fromIntegral n
    go 0 _ lg s = (lg + log (abs (wvV s)), s)
    go k t lg s =
      let s' = rk4 dt (f cfg) t s
          m = max (abs (wvV s')) (abs (wvU s'))
       in go (k - 1) (t + dt) (lg + log m) (scaleW (1 / m) s')

-- | Stage-tower configuration. betaRatio < 1 compounds the shear across
-- stages (the null); betaRatio = 1 is the stagnant control.
data TowerCfg = TowerCfg
  { tcBeta0 :: Double,
    tcBetaRatio :: Double,
    tcStages :: Int,
    tcSteps :: Int
  }
  deriving (Eq, Show)

-- | Per-stage parameters and measured log-amplifications, oldest first.
data TowerResult = TowerResult
  { trBetas :: [Double],
    trLogAmps :: [Double]
  }
  deriving (Eq, Show)

runTower :: TowerCfg -> TowerResult
runTower cfg = TowerResult betas amps
  where
    betas = [tcBeta0 cfg * tcBetaRatio cfg ^ j | j <- [0 .. tcStages cfg - 1]]
    amps = [fst (integrateIdeal (IdealCfg b 0 2) (tcSteps cfg) idealTendency) | b <- betas]

-- | Total injected mass: the summable increment schedule eps_j = 2^-j.
-- Identical for null and control — this is the point.
towerMass :: TowerCfg -> Double
towerMass cfg = foldl' (+) 0 [2 ^^ (-j) | j <- [0 .. tcStages cfg - 1]]

-- | Compounded log-amplification of the tower.
towerLogAmp :: TowerResult -> Double
towerLogAmp = foldl' (+) 0 . trLogAmps

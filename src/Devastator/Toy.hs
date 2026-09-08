{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Toy scalar ODE meeting for the devastator observatory.
--
-- The toy is the scalar ODE @u' = f(u)@ with a swappable nonlinearity,
-- run as a closed cell ('toyRun') and stamped into a tape by the shared
-- runner in "Devastator.Run". Explicit Euler, post-step observations,
-- same schedule discipline as every other devastator system.
module Devastator.Toy
  ( ToyBody (..),
    BilinearOp (..),
    trueOp,
    nullOp,
    toyRun,
    integrate,
    multiIntegrate,
    seedBodies,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, stamped, pattern Stamped)
import Circuit.Parser.Json (decodeJson, encodeJson)
import Circuit.Parser.Json.Value (Json (..))
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Devastator.Run (Framed (..), Run (..), RunCfg (..), runTape, runTapeN)

-- | Body carried by each toy integration post.
data ToyBody = ToyBody
  { tbTime :: Double,
    tbValue :: Double,
    tbFlux :: Double
  }
  deriving (Eq, Show)

-- | The swappable nonlinearity box.
--
-- For the toy it is unary; the name "bilinear" anticipates the NSE fiber
-- where the box is genuinely bilinear.
data BilinearOp = BilinearOp
  { opTag :: Text,
    opApply :: Double -> Double
  }

-- | Regular control: exponential decay.
trueOp :: BilinearOp
trueOp = BilinearOp "true" negate

-- | Blow-up null: finite-time blow-up at t = 1.
nullOp :: BilinearOp
nullOp = BilinearOp "null" (\u -> u * u)

dt :: Double
dt = 0.01

tEnd :: Double
tEnd = 0.9

-- | The toy as a closed cell: state @(t, u)@, observation the toy body,
-- tick one explicit Euler step. The flux is stored in the body, exactly
-- the unfused-carrier slot: it is derivable from the value via the
-- operator, and lives in the body so the observation survives the step.
toyRun :: BilinearOp -> Run (Double, Double) ToyBody
toyRun op =
  Run
    (\(t, u) -> ToyBody t u (opApply op u))
    (\_ (t, u) -> (t + dt, u + dt * opApply op u))

-- | The multi-cell clock reads directly off the step index — @t = k * dt@,
-- not iterated addition — which is what the multi-cell tape's times encode.
cellRun :: BilinearOp -> Run (Double, Double) ToyBody
cellRun op =
  Run
    (\(t, u) -> ToyBody t u (opApply op u))
    (\k (_, u) -> (fromIntegral k * dt, u + dt * opApply op u))

toyCfg :: RunCfg
toyCfg = RunCfg "seed" "step" ["integrator"] ["integrator"]

stopClock :: (Double, Double) -> Bool
stopClock (t, _) = t >= tEnd

-- | Discrete-time Euler integration of @u' = f(u)@, stamped into a tape:
-- a seed post followed by one step post per integration step, each step
-- post threading the id of its immediate predecessor.
integrate :: BilinearOp -> [Stamped ToyBody]
integrate op = runTape stopClock toyCfg (toyRun op) (0, 1)

-- | Extract the seed bodies from a log, oldest first.
--
-- Seed posts are recognised by an empty thread. In the scalar and multi-cell
-- toys every non-seed post threads its predecessor, so this is exact.
seedBodies :: [Stamped ToyBody] -> [ToyBody]
seedBodies = map (body . stamped) . filter (null . thread . stamped)

-- | Multi-cell ODE meeting where each cell has its own operator and seed.
--
-- Cells are independent at each time step: a post only threads its own cell's
-- previous post. Therefore posts within a step can be emitted in any order.
-- This gives the toy a non-trivial linearization-invariance oracle (O5):
-- permuting the order of independent posts does not change the physics.
multiIntegrate :: [(Text, BilinearOp, Double)] -> [Stamped ToyBody]
multiIntegrate cells =
  runTapeN
    stopClock
    [RunCfg ("seed-" <> name) ("step-" <> name) ["integrator-" <> name] ["integrator-" <> name] | (name, _, _) <- cells]
    [cellRun op | (_, op, _) <- cells]
    [(0, u0) | (_, _, u0) <- cells]

-- * Codec

instance Framed ToyBody where
  frameBody b =
    decodeUtf8 $
      encodeJson $
        JObject
          [ ("time", jdouble (tbTime b)),
            ("value", jdouble (tbValue b)),
            ("flux", jdouble (tbFlux b))
          ]
  unframeBody t = do
    JObject o <- either (const Nothing) Just (decodeJson (encodeUtf8 t))
    time <- lookupDouble "time" o
    value <- lookupDouble "value" o
    flux <- lookupDouble "flux" o
    pure (ToyBody time value flux)

jdouble :: Double -> Json
jdouble = JNumber . fromFloatDigits

lookupDouble :: Text -> [(Text, Json)] -> Maybe Double
lookupDouble k o = case lookup k o of
  Just (JNumber s) -> Just (toRealFloat s)
  _ -> Nothing

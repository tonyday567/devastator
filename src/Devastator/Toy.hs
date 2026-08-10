{-# LANGUAGE OverloadedStrings #-}

-- | Toy scalar ODE meeting for the devastator observatory.
module Devastator.Toy
  ( ToyBody (..),
    BilinearOp (..),
    trueOp,
    nullOp,
    integrate,
    multiIntegrate,
    seedBodies,
  )
where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing (Stamped (..))
import Data.List (unfoldr)
import Data.Text (Text)

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

-- | Discrete-time Euler integration of @u' = f(u)@.
--
-- Produces a stamped log: a seed post followed by one step post per
-- integration step. Step posts thread the id of their immediate predecessor,
-- so the tape encodes the causal chain by construction.
integrate :: BilinearOp -> [Stamped (Post ToyBody)]
integrate op = go 0 0.0 1.0 [seed]
  where
    seed :: Stamped (Post ToyBody)
    seed =
      Stamped
        0
        ""
        ( Post
            { from = "seed",
              to = ["integrator"],
              thread = [],
              body = ToyBody 0.0 1.0 (opApply op 1.0)
            }
        )
    go :: Int -> Double -> Double -> [Stamped (Post ToyBody)] -> [Stamped (Post ToyBody)]
    go n t u acc
      | t >= tEnd = reverse acc
      | otherwise =
          let t' = t + dt
              flux = opApply op u
              u' = u + dt * flux
              postId = fromIntegral (n + 1)
              p =
                Post
                  { from = "step",
                    to = ["integrator"],
                    thread = [fromIntegral n],
                    body = ToyBody t' u' (opApply op u')
                  }
           in go (n + 1) t' u' (Stamped postId "" p : acc)

-- | Extract the seed bodies from a log, oldest first.
--
-- Seed posts are recognised by an empty thread. In the scalar and multi-cell
-- toys every non-seed post threads its predecessor, so this is exact.
seedBodies :: [Stamped (Post ToyBody)] -> [ToyBody]
seedBodies = map (body . stamped) . filter (null . thread . stamped)

-- | Per-cell state threaded through a multi-cell integration.
data CellState = CellState
  { csLastId :: PostId,
    csName :: Text,
    csOp :: BilinearOp,
    csU :: Double
  }

-- | Multi-cell ODE meeting where each cell has its own operator and seed.
--
-- Cells are independent at each time step: a post only threads its own cell's
-- previous post. Therefore posts within a step can be emitted in any order.
-- This gives the toy a non-trivial linearization-invariance oracle (O5):
-- permuting the order of independent posts does not change the physics.
multiIntegrate :: [(Text, BilinearOp, Double)] -> [Stamped (Post ToyBody)]
multiIntegrate cells = seeds ++ concat (unfoldr step (initStates, 1))
  where
    n = length cells
    seeds = zipWith makeSeed [0 ..] cells
    makeSeed i (name, op, u0) =
      Stamped
        i
        ""
        ( Post
            { from = "seed-" <> name,
              to = ["integrator-" <> name],
              thread = [],
              body = ToyBody 0.0 u0 (opApply op u0)
            }
        )
    initStates = zipWith makeState [0 ..] cells
    makeState i (name, op, u0) = CellState i name op u0
    step (states, k) =
      let t = fromIntegral k * dt
       in if t > tEnd + 1e-12
            then Nothing
            else
              let (posts, states') = unzip (map (advanceCell n k t) states)
               in Just (posts, (states', k + 1))

advanceCell :: Int -> Int -> Double -> CellState -> (Stamped (Post ToyBody), CellState)
advanceCell n k t (CellState prevId name op u) =
  let flux = opApply op u
      u' = u + dt * flux
      postId = fromIntegral (n * k) + prevId
      p =
        Post
          { from = "step-" <> name,
            to = ["integrator-" <> name],
            thread = [prevId],
            body = ToyBody t u' (opApply op u')
          }
   in (Stamped postId "" p, CellState postId name op u')

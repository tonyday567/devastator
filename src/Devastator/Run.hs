{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The devastator run spine: one closed cell, one runner, one tape.
--
-- Every devastator system is the same machine: a state, an observation of
-- that state, and a tick. The seed is a state, not a leg — applied once,
-- from outside, by the runner. 'runTape' stamps the run into the tape
-- discipline every system already kept by hand: a seed post with an empty
-- thread, then one post per tick threading its immediate predecessor.
--
-- The shape is the closed-cell grade of the circuits candidate spine
-- (observe/tick at a unit input), owned here so the devastator does not
-- pin itself to a candidate module; the swap, if the candidate graduates,
-- is field-for-field.
module Devastator.Run
  ( -- * The closed cell
    Run (..),

    -- * Routing
    RunCfg (..),

    -- * Runners
    runTape,
    runTapeN,

    -- * Body codec
    Framed (..),
  )
where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing (Stamped, pattern Stamped)
import Data.List (unfoldr)
import Data.Text (Text)
import Devastator.Framing (epoch)

-- | A closed cell: one observation per state, one tick per step, no input
-- after the seed.
data Run s b = Run
  { -- | The observation: read the tape body from the state alone.
    observe :: s -> b,
    -- | The tick: advance the state one integration step. The argument is
    -- the step index, starting at 1 — systems whose clock reads directly
    -- off the index (rather than accumulating) use it.
    tick :: Int -> s -> s
  }

-- | Routing fields for the posts a run emits.
data RunCfg = RunCfg
  { seedFrom :: Text,
    stepFrom :: Text,
    seedTo :: [Text],
    stepTo :: [Text]
  }

-- | Run one cell to the stop predicate, emitting one stamped post per
-- state: the seed state first, then the post-step state each tick. PostIds
-- increment from 0 and every step post threads its immediate predecessor,
-- so the tape encodes the causal chain by construction.
runTape :: forall s b. (s -> Bool) -> RunCfg -> Run s b -> s -> [Stamped b]
runTape stop cfg r s0 = go 0 s0 [seed]
  where
    seed :: Stamped b
    seed = Stamped (epoch, 0) (Post (seedFrom cfg) (seedTo cfg) [] (observe r s0))
    go :: Int -> s -> [Stamped b] -> [Stamped b]
    go n s acc
      | stop s = reverse acc
      | otherwise =
          let s' = tick r (n + 1) s
              p = Post (stepFrom cfg) (stepTo cfg) [fromIntegral n] (observe r s')
           in go (n + 1) s' (Stamped (epoch, fromIntegral (n + 1)) p : acc)

-- | Run several cells in lockstep under a shared clock: all seed posts
-- first, then per tick one post per cell in order. Cell @i@ occupies
-- PostIds @i, n+i, 2n+i, ...@ and threads only its own previous post, so
-- independent cells stay independent on the tape — the O5
-- linearization-invariance shape.
runTapeN :: forall s b. (s -> Bool) -> [RunCfg] -> [Run s b] -> [s] -> [Stamped b]
runTapeN stop cfgs runs s0s = seeds ++ concat (unfoldr step (s0s, 1))
  where
    n = length runs
    seeds :: [Stamped b]
    seeds =
      [ Stamped (epoch, fromIntegral i) (Post (seedFrom cfg) (seedTo cfg) [] (observe r s))
      | (i, cfg, r, s) <- zip4 [0 ..] cfgs runs s0s
      ]
    step :: ([s], Int) -> Maybe ([Stamped b], ([s], Int))
    step (ss, k)
      | any stop ss = Nothing
      | otherwise =
          let ss' = map (\(r, s) -> tick r k s) (zip runs ss)
              posts =
                [ Stamped (epoch, postId i k) $
                    Post (stepFrom cfg) (stepTo cfg) [postId i (k - 1)] (observe r s')
                | (i, cfg, r, s') <- zip4 [0 ..] cfgs runs ss'
                ]
           in Just (posts, (ss', k + 1))
    -- PostIds follow the cell's own chain: cell @i@ at tick @k@ sits at
    -- @n * T_k + i@ with @T_k@ the triangular number, threading @n * T_{k-1}
    -- + i@ — the recurrence @id_k = n * k + id_{k-1}@ in closed form.
    postId :: Int -> Int -> PostId
    postId i k = fromIntegral (n * k * (k + 1) `div` 2) + fromIntegral i

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

-- | The body codec: one serialisation discipline for every tape body, so
-- the generic 'Devastator.Tape' codec can frame any run's output.
class Framed b where
  frameBody :: b -> Text
  unframeBody :: Text -> Maybe b


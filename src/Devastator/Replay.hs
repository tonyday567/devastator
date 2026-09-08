{-# LANGUAGE OverloadedStrings #-}

-- | Replay axioma for the toy devastator.
--
-- The devastator filter rests on three oracle-guarded claims:
--
-- 1. Unchanged cones reproduce exactly. Replaying a meeting with the same
--    operator produces a log whose bodies match at every PostId whose thread
--    is unchanged.
-- 2. Separation is significant only above a calibrated noise floor. The floor
--    is measured by perturbing seeds and replaying; the ratio of true-null
--    separation to that floor is what the verdict reports.
-- 3. Linearization invariance (O5). Independent posts can be emitted in any
--    topological order without changing observables.
module Devastator.Replay
  ( replayMeeting,
    swapBox,
    unchangedCone,
    separationSignificance,
    linearizationInvariant,
  )
where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing (Stamped, stamp, stamped)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Devastator.Cert (Certificate)
import Devastator.Toy (BilinearOp (..), ToyBody (..), multiIntegrate, seedBodies)

-- | Extract the (name, operator, seed value) triples needed to replay a
-- multi-cell meeting.
--
-- Names are recovered from @from = "seed-<name>"@. The supplied operator
-- replaces whatever operator produced the original log.
cellsFromLog :: BilinearOp -> [Stamped ToyBody] -> [(Text, BilinearOp, Double)]
cellsFromLog op meeting =
  [ (Text.drop 5 (from p), op, tbValue (body p))
  | sp <- filter (null . thread . stamped) meeting,
    let p = stamped sp,
    Text.isPrefixOf "seed-" (from p)
  ]

-- | Replay a meeting from its seeds with a (possibly swapped) operator.
--
-- The operator tag is overwritten; every cell keeps its name and initial value.
replayMeeting :: BilinearOp -> [Stamped ToyBody] -> [Stamped ToyBody]
replayMeeting op meeting = multiIntegrate (cellsFromLog op meeting)

-- | Swap the nonlinearity box and replay the meeting from the same seeds.
swapBox :: BilinearOp -> [Stamped ToyBody] -> [Stamped ToyBody]
swapBox = replayMeeting

-- | Check that every post in the first log whose PostId also appears in the
-- second log has the same thread and the same body.
--
-- This is the "unchanged cone" oracle made executable: if nothing in a post's
-- ancestry changed, its body must reproduce exactly.
unchangedCone :: [Stamped ToyBody] -> [Stamped ToyBody] -> Bool
unchangedCone as bs = all match as
  where
    match a = case find ((== postIdOf a) . postIdOf) bs of
      Nothing -> False
      Just b ->
        thread (stamped a) == thread (stamped b)
          && body (stamped a) == body (stamped b)
    postIdOf = snd . stamp

-- | Calibrated noise floor for a certificate on a given log.
--
-- Perturb each seed by +/- epsilon, replay with the same operator, and take
-- the largest absolute change in the certificate. The floor is never reported
-- as zero: if epsilon is zero or the system is perfectly insensitive, the
-- floor is clamped to epsilon itself so the significance ratio stays honest.
noiseFloor :: Certificate ToyBody -> Double -> [Stamped ToyBody] -> Double
noiseFloor cert epsilon meeting =
  let cells = cellsFromLog (BilinearOp "noise" id) meeting -- op unused for perturbation
      base = cert (multiIntegrate cells)
      floors =
        [ abs (cert (multiIntegrate cells') - base)
        | (i, _) <- zip [(0 :: Int) ..] cells,
          d <- [-epsilon, epsilon],
          let cells' = perturb i d cells
        ]
   in max epsilon (maximum (0 : floors))
  where
    perturb i d = zipWith (\j c -> if i == j then let (n, op, u) = c in (n, op, u + d) else c) [(0 :: Int) ..]

-- | Significance ratio of the separation between two logs.
--
-- Returns @|cert(true) - cert(null)| / noiseFloor(cert, epsilon, trueLog)@.
-- A ratio above the chosen threshold is what earns a SEPARATING verdict.
separationSignificance :: Certificate ToyBody -> Double -> [Stamped ToyBody] -> [Stamped ToyBody] -> Double
separationSignificance cert epsilon trueLog nullLog =
  let sep = abs (cert trueLog - cert nullLog)
      floor' = noiseFloor cert epsilon trueLog
   in sep / floor'

-- | Linearization-invariance oracle (O5) for independent cells.
--
-- Two cell orderings that differ only by permuting independent cells within a
-- step must yield the same certificate. The physics (the causal DAG) is the
-- same; only the file serialization changes.
linearizationInvariant :: Certificate ToyBody -> [(Text, BilinearOp, Double)] -> [(Text, BilinearOp, Double)] -> Bool
linearizationInvariant cert cellsA cellsB =
  let logA = multiIntegrate cellsA
      logB = multiIntegrate cellsB
   in cert logA == cert logB

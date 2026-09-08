{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Katz–Pavlović dyadic model: a hostile null for the devastator.
--
-- Shell amplitudes a_n evolve as
--
-- @da_n\/dt = λ^{2n} a_{n-1}² - λ^{2n+1} a_n a_{n+1}@,  n ≥ 0,  a_{-1} = 0.
--
-- The model conserves energy E = ½ Σ a_n² and is known to exhibit finite-time
-- blow-up as the truncation N → ∞. In any fixed truncation it is a finite ODE
-- and exists globally, but energy rushes to the highest retained shell; the
-- rush accelerates with N. That tower behavior is what the devastator records.
module Devastator.Null.Dyadic
  ( DyadicBody (..),
    kpBox,
    dyadicEnergy,
    dyadicShellEnergy,
    stepDyadic,
    dyadicRun,
    integrateDyadic,
    frameDyadic,
    readDyadic,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, These (..), frameStored, pattern Stamped)
import Circuit.Parser.Json (decodeJson, encodeJson)
import Circuit.Parser.Json.Value (Json (..))
import Data.List (unfoldr)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import Devastator.Framing (Jsonl (..), epoch, uncons)
import Devastator.Run (Framed (..), Run (..), RunCfg (..), runTape)
import Devastator.Tape (frameTape, readTape)

-- | Body carried by each dyadic post: time plus the shell amplitudes.
data DyadicBody = DyadicBody
  { dbTime :: Double,
    dbShells :: [Double]
  }
  deriving (Eq, Show)

lambda :: Double
lambda = 2.0

-- | Katz–Pavlović nonlinear box: the interaction coefficients above.
kpBox :: Int -> [Double]
kpBox n = [lambda ^ (2 * k) | k <- [0 .. n - 1]]

-- | Total energy of a dyadic state. The KP model conserves
-- @E = ½ Σ λ^{-n} a_n²@, not the unweighted sum.
dyadicEnergy :: DyadicBody -> Double
dyadicEnergy b =
  0.5 * sum [lambda ^^ (-(i :: Int)) * a * a | (i, a) <- zip [0 ..] (dbShells b)]

-- | Energy above a shell cutoff, using the same weights.
dyadicShellEnergy :: Int -> DyadicBody -> Double
dyadicShellEnergy cutoff b =
  0.5 * sum [lambda ^^ (-(i :: Int)) * a * a | (i, a) <- zip [0 ..] (dbShells b), i >= cutoff]

-- | Tendency of the KP model on the first n shells.
dyadicTendency :: Int -> [Double] -> [Double]
dyadicTendency n as =
  [ go i
  | i <- [0 .. n - 1]
  ]
  where
    at i = if i >= 0 && i < n then as !! i else 0.0
    go i =
      let left = if i == 0 then 0.0 else lambda ^ (2 * i) * (at (i - 1)) ^ (2 :: Int)
          right = lambda ^ (2 * i + 1) * (at i) * (at (i + 1))
       in left - right

-- | Explicit Euler step.
stepDyadic :: Double -> Int -> [Double] -> [Double]
stepDyadic dt n as = zipWith (+) as (map (dt *) (dyadicTendency n as))

-- | The KP model as a closed cell: state @(t, shells)@, observation the
-- shell amplitudes, tick one explicit Euler step.
dyadicRun :: Int -> Double -> Run (Double, [Double]) DyadicBody
dyadicRun n dt =
  Run
    (\(t, as) -> DyadicBody t as)
    (\_ (t, as) -> (t + dt, stepDyadic dt n as))

dyadicCfg :: RunCfg
dyadicCfg = RunCfg "dyadic-seed" "dyadic-step" ["dyadic"] ["dyadic"]

jdouble :: Double -> Json
jdouble = JNumber . fromFloatDigits

encodeBody :: DyadicBody -> Text
encodeBody b =
  decodeUtf8 $
    encodeJson $
      JObject
        [ ("time", jdouble (dbTime b)),
          ("shells", JArray (V.fromList (map jdouble (dbShells b))))
        ]

decodeBody :: Text -> Maybe DyadicBody
decodeBody t = do
  JObject o <- either (const Nothing) Just (decodeJson (encodeUtf8 t))
  time <- lookupDouble "time" o
  JArray vs <- lookup "shells" o
  shells <- traverse decodeDouble (V.toList vs)
  pure (DyadicBody time shells)
  where
    lookupDouble k o = case lookup k o of
      Just (JNumber s) -> Just (toRealFloat s)
      _ -> Nothing
    decodeDouble (JNumber s) = Just (toRealFloat s)
    decodeDouble _ = Nothing

-- | Integrate the KP model and emit a Post/JSONL tape.
integrateDyadic ::
  -- | number of shells
  Int ->
  -- | dt
  Double ->
  -- | tEnd
  Double ->
  -- | initial amplitudes
  [Double] ->
  [Stamped DyadicBody]
integrateDyadic n dt tEnd seed =
  runTape (\(t, _) -> t >= tEnd - 1e-15) dyadicCfg (dyadicRun n dt) (0, take n (seed ++ repeat 0.0))

-- * Codec

instance Framed DyadicBody where
  frameBody = encodeBody
  unframeBody = decodeBody

frameDyadic :: [Stamped DyadicBody] -> Jsonl
frameDyadic = frameTape

readDyadic :: Jsonl -> [Stamped DyadicBody]
readDyadic = readTape

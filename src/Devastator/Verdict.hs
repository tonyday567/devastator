-- | Verdict logic for the nonlinearity-swap filter.
module Devastator.Verdict
  ( Verdict (..),
    verdictAbs,
    verdictRel,
  )
where

-- | Possible outcomes of comparing a certificate on the true log vs a null log.
data Verdict = Skeleton | Separating | Undecided
  deriving (Eq, Show)

-- | Compare certificate values with an absolute tolerance.
verdictAbs :: Double -> Double -> Double -> Verdict
verdictAbs tol vTrue vNull
  | abs (vTrue - vNull) < tol = Skeleton
  | vNull > vTrue + tol = Separating
  | otherwise = Undecided

-- | Compare certificate values with a tolerance scaled to the larger value.
verdictRel :: Double -> Double -> Double -> Verdict
verdictRel tol vTrue vNull =
  let tol' = tol * max 1.0 (max vTrue vNull)
   in verdictAbs tol' vTrue vNull

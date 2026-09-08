-- | Candidate regularity certificates for the toy devastator.
module Devastator.Cert
  ( Certificate,
    trivialCert,
    maxValueCert,
    valueAtCert,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, stamped)
import Devastator.Toy (ToyBody (..))

-- | A certificate reads the tape and emits a single scalar.
type Certificate = [Stamped ToyBody] -> Double

-- | Skeleton-level sanity check: cannot distinguish true from null.
trivialCert :: Certificate
trivialCert _ = 0.0

-- | Energy-like observer: the largest absolute value on the tape.
--
-- This one sees the blow-up, so the filter should mark it SEPARATING.
maxValueCert :: Certificate
maxValueCert = maximum . map (abs . tbValue . body . stamped)

-- | Value observer at a requested time.
--
-- Returns the absolute value of the first body whose time is at or after the
-- requested time. Used to demonstrate the UNDECIDED verdict: at very early
-- times the true and null traces are close, and with a coarse noise floor the
-- separation is not significant.
valueAtCert :: Double -> Certificate
valueAtCert t0 meeting =
  case dropWhile ((< t0) . tbTime . body . stamped) meeting of
    (sp : _) -> abs . tbValue . body . stamped $ sp
    [] -> 0.0

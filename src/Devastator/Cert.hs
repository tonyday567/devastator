{-# LANGUAGE OverloadedStrings #-}

-- | Candidate regularity certificates for the devastator.
--
-- A certificate reads a tape and emits a single scalar. Certificates are
-- consumers: a fold over the observations (maxValueCert), a final read
-- (totalEnergyCert), a read at a requested time (valueAtCert).
module Devastator.Cert
  ( Certificate,
    trivialCert,
    maxValueCert,
    valueAtCert,
    totalEnergyCert,
    shellEnergyCert,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, stamped)
import Data.Complex (magnitude)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Devastator.Fiber (FiberBody (..), bodyField)
import Devastator.Spectral (SpectralField (..), energy, norm2)
import Devastator.Toy (ToyBody (..))

-- | A certificate reads the tape and emits a single scalar.
type Certificate b = [Stamped b] -> Double

-- | Skeleton-level sanity check: cannot distinguish true from null.
trivialCert :: Certificate b
trivialCert _ = 0.0

-- | Energy-like observer: the largest absolute value on the tape.
--
-- This one sees the blow-up, so the filter should mark it SEPARATING.
maxValueCert :: Certificate ToyBody
maxValueCert = maximum . map (abs . tbValue . body . stamped)

-- | Value observer at a requested time.
--
-- Returns the absolute value of the first body whose time is at or after the
-- requested time. Used to demonstrate the UNDECIDED verdict: at very early
-- times the true and null traces are close, and with a coarse noise floor the
-- separation is not significant.
valueAtCert :: Double -> Certificate ToyBody
valueAtCert t0 meeting =
  case dropWhile ((< t0) . tbTime . body . stamped) meeting of
    (sp : _) -> abs . tbValue . body . stamped $ sp
    [] -> 0.0

-- * Fiber certificates

-- | Total energy of the final field.
totalEnergyCert :: Certificate FiberBody
totalEnergyCert = energy . bodyField . body . stamped . last

-- | Energy in shells strictly above the cutoff (in squared wavevector).
--
-- This is the first spectral certificate: it sees scale redistribution, which
-- the total energy certificate cannot.
shellEnergyCert :: Double -> Certificate FiberBody
shellEnergyCert cutoff meeting =
  let SpectralField m = bodyField (body (stamped (last meeting)))
   in 0.5 * sum [magnitude w ^ (2 :: Int) / norm2 k | (k, w) <- Map.toList m, norm2 k > cutoff]

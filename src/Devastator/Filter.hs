{-# LANGUAGE OverloadedStrings #-}

-- | Filter pipeline for the devastator observatory.
--
-- A filter takes a true fiber log and a null fiber log, runs a certificate on
-- both, and packages the result as a replayable verdict entry. This is the
-- devastator's core operation at any height: compare operators via the tape.
module Devastator.Filter
  ( FiberCertificate,
    totalEnergyCert,
    shellEnergyCert,
    filterVerdict,
    recomputeFiberVerdict,
  )
where

import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing (Jsonl (..), Stamped (..))
import Data.Complex (magnitude)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Devastator.Fiber (FiberBody, bodyField, readFiber)
import Devastator.Ledger (VerdictEntry (..))
import Devastator.Spectral (BilinearBox (..), SpectralField (..), energy, norm2)
import Devastator.Verdict (Verdict, verdictRel)

-- | A fiber certificate reads the tape and emits one scalar.
type FiberCertificate = [Stamped (Post FiberBody)] -> Double

-- | Total energy of the final field.
totalEnergyCert :: FiberCertificate
totalEnergyCert = energy . bodyField . body . stamped . last

-- | Energy in shells strictly above the cutoff (in squared wavevector).
--
-- This is the first spectral certificate: it sees scale redistribution, which
-- the total energy certificate cannot.
shellEnergyCert :: Double -> FiberCertificate
shellEnergyCert cutoff meeting =
  let SpectralField m = bodyField (body (stamped (last meeting)))
   in 0.5 * sum [magnitude w ^ (2 :: Int) / norm2 k | (k, w) <- Map.toList m, norm2 k > cutoff]

-- | Build one verdict entry from a true/null fiber pair and a certificate.
--
-- Significance here is a simple relative separation; a calibrated noise floor
-- (perturbation of the initial field) can be substituted once the fiber
-- replay module supports it.
filterVerdict ::
  PostId ->
  Text ->
  FiberCertificate ->
  BilinearBox ->
  BilinearBox ->
  [Stamped (Post FiberBody)] ->
  [Stamped (Post FiberBody)] ->
  Text ->
  Text ->
  Double ->
  [PostId] ->
  VerdictEntry
filterVerdict eid certName cert trueBox nullBox trueLog nullLog truePath nullPath tol thread =
  let cTrue = cert trueLog
      cNull = cert nullLog
      sig = abs (cTrue - cNull) / max 1e-12 (max (abs cTrue) (abs cNull))
      v = verdictRel tol cTrue cNull
   in VerdictEntry
        { veId = eid,
          veVerdict = v,
          veCertificate = certName,
          veTrueOp = boxTag trueBox,
          veNullOp = boxTag nullBox,
          veTrueLog = truePath,
          veNullLog = nullPath,
          veSignificance = sig,
          veTolerance = tol,
          veEpsilon = 0.0,
          veThread = thread
        }

-- | Recompute a fiber verdict entry from its cited tape files.
--
-- The generic 'Devastator.Ledger.recomputeVerdict' is hardcoded to toy-body
-- tapes; this is the fiber-body variant.
recomputeFiberVerdict :: VerdictEntry -> IO (Verdict, Double)
recomputeFiberVerdict e = do
  trueJsonl <- readTapeFile (veTrueLog e)
  nullJsonl <- readTapeFile (veNullLog e)
  let trueLog = readFiber trueJsonl
      nullLog = readFiber nullJsonl
      cert = resolveFiberCert (veCertificate e)
      cTrue = cert trueLog
      cNull = cert nullLog
      sig = abs (cTrue - cNull) / max 1e-12 (max (abs cTrue) (abs cNull))
      v = verdictRel (veTolerance e) cTrue cNull
  pure (v, sig)
  where
    readTapeFile path =
      Jsonl . Text.lines <$> Text.IO.readFile (Text.unpack path)
    resolveFiberCert "totalEnergyCert" = totalEnergyCert
    resolveFiberCert name
      | "shellEnergyCert@" `Text.isPrefixOf` name =
          let cutoff = read (Text.unpack (Text.drop 16 name))
           in shellEnergyCert cutoff
    resolveFiberCert _ = totalEnergyCert

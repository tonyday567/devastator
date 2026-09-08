{-# LANGUAGE OverloadedStrings #-}

-- | Filter pipeline for the devastator observatory.
--
-- A filter takes a true fiber log and a null fiber log, runs a certificate on
-- both, and packages the result as a replayable verdict entry. This is the
-- devastator's core operation at any height: compare operators via the tape.
module Devastator.Filter
  ( FiberCertificate,
    filterVerdict,
    resolveFiberCert,
    judgeFiber,
    recomputeFiberVerdict,
  )
where

import Circuit.Agent (PostId)
import Circuit.Agent.Framing (Stamped)
import Data.Text (Text)
import Data.Text qualified as Text
import Devastator.Cert (Certificate, shellEnergyCert, totalEnergyCert)
import Devastator.Fiber (FiberBody)
import Devastator.Ledger (VerdictEntry (..), recomputeWith)
import Devastator.Run (Framed)
import Devastator.Spectral (BilinearBox (..))
import Devastator.Verdict (Verdict, verdictRel)

-- | A fiber certificate reads the fiber tape and emits one scalar.
type FiberCertificate = Certificate FiberBody

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
  [Stamped FiberBody] ->
  [Stamped FiberBody] ->
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

-- | Resolve a fiber certificate by its ledger name.
resolveFiberCert :: Text -> FiberCertificate
resolveFiberCert "totalEnergyCert" = totalEnergyCert
resolveFiberCert name
  | "shellEnergyCert@" `Text.isPrefixOf` name =
      let cutoff = read (Text.unpack (Text.drop 16 name))
       in shellEnergyCert cutoff
resolveFiberCert _ = totalEnergyCert

-- | Fiber verdict judgement: relative separation against the larger value.
judgeFiber ::
  VerdictEntry ->
  FiberCertificate ->
  [Stamped FiberBody] ->
  [Stamped FiberBody] ->
  (Verdict, Double)
judgeFiber e cert trueLog nullLog =
  let cTrue = cert trueLog
      cNull = cert nullLog
      sig = abs (cTrue - cNull) / max 1e-12 (max (abs cTrue) (abs cNull))
   in (verdictRel (veTolerance e) cTrue cNull, sig)

-- | Recompute a fiber verdict entry from its cited tape files.
recomputeFiberVerdict :: VerdictEntry -> IO (Verdict, Double)
recomputeFiberVerdict = recomputeWith resolveFiberCert judgeFiber

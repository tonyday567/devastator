# devastator — Navier–Stokes and Euler in Haskell

A device that prices certificates about fluids against hostile dynamics.
A certificate is a cheap observable claim about a flow — energy bounded,
enstrophy conserved, BKM integral convergent. A null is a finite, exact
system engineered to break exactly one claim while keeping the rest
green. The ledger records which certificates separate the true box from
each null, so any future "proof by energy estimate" can be priced in one
lookup.

This readme is the working context for the project — what has been
built, what has been priced, what is open. The fuller narrative
(commentary on the Euler and Navier–Stokes announcements, the role of
[circuits](https://github.com/tonyday567/circuits), coordination as
flow) lands with circuits-0.3, when example code can be written against
the settled API. Until then, the context parks here.

## the claim, calibrated

Two papers by OpenAI, both cloned with their Lean at
`~/other/external/NavierStokesAndEuler` (2482 Lean files, Lean
4.34.0-rc2 + Mathlib, `formalization.yaml` reporting `sorry_count: 0`,
against DeepMind's FormalConjectures reference statements for the Clay
alternatives):

- **Euler** (45pp, unforced): smooth, compactly supported,
  divergence-free initial data on ℝ³ whose Euler solution blows up in
  finite time — C¹ norm unbounded, and the Beale–Kato–Majda integral
  ∫‖ω‖∞ dt diverges. Mechanism: iterated short-wave amplification —
  localized oscillations grown by a background flow à la
  Lifschitz–Hameiri / Friedlander–Vishik, on successively shorter time
  intervals, with initial increments summable in every Sobolev norm. If
  correct this resolves a famous open problem — though it is *not* the
  Clay prize (prize is Navier–Stokes).
- **Navier–Stokes** (165pp, forced): for every ν > 0, smooth compactly
  supported forcing and zero initial data giving finite-time unbounded
  velocity at bounded kinetic energy — Fefferman's alternatives (C)/(D),
  *but with forcing*. The Millennium problem is unforced, so this is
  "partial claiming" precisely: alternatives (C)/(D) as stated in the
  official description, in a setting the prize doesn't cover.

The whole scheme is *designed to evade every known cheap certificate* —
energy bounded by construction, BKM divergent by construction, all
Sobolev norms finite at t=0. That makes it a gift for this device: the
job is to make the evasion explicit, recorded in the ledger.

## N2 — short-wave amplification, landed

`Devastator.Null.ShortWave` — the Euler paper's ideal velocity system
(4.21), `(D V')' = 2 (1 - beta P0) V`, integrated exactly in
log-rescaled form; seven-axioma suite green (convergence, 1/beta
amplification law, coupling mutation, lambda monotonicity, ledger
verdicts: mass SKELETON / amplification SEPARATING).

Physics correction, kept: the `(1 - beta P0)` coupling is an O(1)
*suppressor* (its beta cancels against the 1/beta time window), not the
amplification source — the amplification is carried by the `2/D`
kernel; the coupling shapes the ray geometry for frame transfer. See
`src/Devastator/Null/ShortWave.hs` and `app/shortwave-axioma.hs`.

## run spine

Three hand-built integration loops and three body codecs collapsed onto
`Devastator.Run`: a closed-cell runner `Run s b = observe/tick`, tape
running through the same spine (`runTape` / `runTapeN`), one `Framed`
codec class, certificates generic over the payload, ledger replay
unified through `recomputeWith`. All axiomas and persisted tapes
byte-identical to baseline.

## 3D — staged against the OpenAI construction

A 3D fiber whose true box is genuine 3D NSE plus the OpenAI
*configuration* (forcing and initial data at leading order), treating
configuration as harness and box as physics. Stages:

1. **Fiber3 at n ≤ 8**: `Devastator.Spectral` generalized to
   `(kx,ky,kz)`; helicity as the audit twin (3D inviscid enstrophy is
   not conserved); forcing support in the stepper.
2. **Tao-averaged box**: same skeleton, rotated coefficients — the
   ideal separation test.
3. **Pulses as a finite null**: N2-style extraction of the leading-order
   pulse amplitude system from the NS paper (`Devastator.Null.Pulse`).
4. **Machina**: measure at what truncation the skeleton signatures stop
   being representable — pricing how much of this construction any
   spectral certificate can ever see.

The wall, honestly stated: no honest spectral truncation represents the
pulses; their whole point is living between the modes. The full
correction tower is not rebuilt here; that is what their Lean is for.

## packets — the OpenAI packet as a Cell

The circuits mapping, design stage: a packet is a `Cell`; the fluid is
not.

    Packet   = Cell These PacketState (->) Stencil LocalJet
    Ensemble = tensor of packets on the annulus, unfused
    Pressure = Body with yank, or an explicit Fourier projector named as a Body
    force    = observe (the residual after compact close)
    nulls    = same ensemble, different box in step

The tick tensor is the physics. `(,)` is the lockstep ensemble — Moore,
every packet ticking together, which is precisely what kills
intermittency. `These` is the cone: only me / only you / both — the
schedule of supports the paper spends §§6–7 building. Fused/unfused is
not an implementation detail.

What the vocabulary buys: compactness as typed closure (`Nu These =
NonEmpty` — the last pulse's death is in the type); force as the
remainder of close (the Clay question is literally whether `step`
closes at zero); a two-axis filter (box = the devastator's axis, wiring
= circuits' axis — a purely local cone cannot run NS; ∇p is nonlocal);
permutation sensitivity as a locality certificate; stage composition as
`Comp` of machines.

The honest wall: none of this touches the estimates. The 165 pages live
inside `step`, and no typing shortcuts them. Proofs inside `step`,
prices on the wiring.

## status

- N2, run spine, 3D staging, packets design: landed / folded in; all
  green against baseline.
- Lean build 🚩 killed: the multi-hour build was freezing the board; the
  ~93% built tree stays on disk, resuming is one command if the verdict
  is ever wanted. The statement audit does not wait on it: the forcing
  gap can be read off the formal statements directly.
- N1 (KP dyadic), fiber, toy, replay, ledger, filter: all green,
  untouched.

## open calls

- **Statement audit**: read the formalized (C)/(D) off the repo source
  and diff against Fefferman's unforced prize statement — the forcing
  gap is the whole difference between "Clay" and "Clay-adjacent".
  Optional hygiene first: grep sorry/admit + `#print axioms` on the main
  declarations.
- **Machina**: measure the paper's stage-parameter sequence against the
  ideal-system amplification actually delivered — quantify the
  bookkeeping headroom.
- **Two-axis ledger entries**: extend `VerdictEntry` with a wiring
  field.
- **Permutation oracle**: a packet-ensemble toy pinning the locality
  sensitivity claim at an axioma.
- **Stage tower as Comp of machines**, on the run spine.
- **3D stage 1**, on the runner's word.

## running the checks

```bash
cabal build all
cabal run devastator-axioma -- all   # or per-topic: shortwave, fiber,
                                     # filter, ledger, replay, null, toy
```

## links

residual-of: devastator-3d, packets, devastator port (uncarded)

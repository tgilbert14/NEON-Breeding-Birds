# Data takeaways — RELEASE-2026 results pending

## Pre-release notice

The numerical takeaways previously stored here came from a superseded 46-site,
detection-adjacent build. They must not be reused for Pass 7. In particular, old
site rankings, richness ranges, rarefaction targets, correlations, counts, and
precipitation coverage were not computed from the new opportunity-complete
contract.

This file intentionally publishes no replacement ecological result until the
exact RELEASE-2026 candidate is produced, independently validated, reviewed, and
merged. The release evidence and exact revision must be recorded in
[`BUILD-TEST-HANDOFF.md`](BUILD-TEST-HANDOFF.md) before this note is replaced.

## Fixed release facts

These are release and contract facts, not derived ecological findings:

- Bird product: `DP1.10003.001`, immutable `RELEASE-2026`, DOI
  `10.48443/v6hs-mx57`.
- Required bird-site roster: exactly 47 sites, including `PUUM`.
- Air-temperature context: 47 of 47 bird sites.
- Plant-phenology context: 47 of 47 bird sites.
- Precipitation source-stream context: 20 of 47 bird sites. A public annual value
  additionally requires a complete calendar year; unsupported values remain
  missing and are never imputed.
- Runtime is bundle-only. A visitor cannot change results through a live API fetch.

## What is being recomputed

The candidate rebuild starts from `brd_perpoint`, not from detection rows. It
retains every attempted physical count and gives each one a `positive`,
`supported_zero`, or `unavailable` outcome. Valid physical counts keyed by
`survey_id` are the sample-incidence units. Repeated bouts remain separate
protocol samples, but they are not described as independent places, years, or
occupancy replicates.

The same visit ledger is aggregated to `pointkey x year` only for annual support
and audit. A point-year is positive when any valid bout is positive, even if
another bout is a supported-zero count. At either stated grain:

- **positive** — valid support and at least one eligible protocol-filtered
  detection;
- **supported zero** — valid support and no eligible in-window, non-flyover
  detection;
- **unavailable** — no valid count at that grain; missing support, never zero.

Eligible protocol-filtered community detections must join a valid visit, occur in
formal point-count minutes 1–6, have a known detection method, have species/subspecies
identification that safely canonicalizes to a genus + species binomial, have
positive finite integer cluster size, and not be a flyover. Parent species and all
reported subspecies share that one biological-species community unit. Exact
reported taxonomy remains provenance. Minute 88 incidentals,
missing/unknown minute values, flyovers, coarse identifications, invalid visits,
ambiguous joins, missing/unknown methods, raw methods, observer support, and raw
distance states remain auditable even when they do not enter metrics. Observer
support is limited to site/species aggregate counts; raw observer values and
row-level aliases remain
producer-local and are removed before the receipt-bound scientific evidence is
transferred to the independent validator. The observer aggregates and complete-
source digest are producer attestations; the allowlisted projection digest and
all scientific row/opportunity derivations are independently verified across the
job boundary.

The privacy-safe site-wide detection audit export conserves every public `obs` +
`held` row, including taxa that never appear in the species picker. It carries
canonical and reported taxonomy, eligibility, protocol-window, method, flyover,
distance, and held-reason fields without observer identities or free text.

From that common ledger, the validator recomputes:

- observed eligible richness and birds-per-valid-count detection index;
- valid-physical-count detection frequency for the Bird Board;
- bias-corrected physical-count Chao2, sample coverage, sampled incidence
  accumulation, and common-count-support rarefaction;
- supported annual series, including real zero-detection years;
- site/map/search summaries under the same eligibility rule;
- the relative area-and-effort-standardized distance signature;
- RELEASE-2026 environmental context and 2017–2024 cross-site descriptive
  comparisons.

## Interpretation rules that will remain true

- Birds per count is a **detection index**, not abundance, density, occupancy, or
  population size.
- Detection frequency is a physical-count sample summary, not geographic spread
  or detection-corrected occupancy.
- A supported zero means a valid count occurred and no eligible bird was detected;
  it does not prove absence.
- Chao2 is an extrapolation. Results with weak duplicate incidence support do not
  lead, and uncertainty must travel with the estimate. The implemented point is
  `Sobs + ((T-1)/T) * Q1 * (Q1-1) / (2 * (Q2+1))`.
- Accumulation describes the sampled curve only; it does not predict whether
  additional counts would add species. Hill q1/q2 are unstandardized plug-in
  summaries, not effort-robust or detection-corrected estimates.
- The distance panel is a relative observation signature, not density or a fitted
  detection function; its 0–200 m display limit and excluded observed-distance
  count must remain visible.
- Cross-site climate panels are descriptive space-for-time comparisons, not
  causal effects or forecasts. The 2017–2024 bird window selects realized count
  months for temperature context; precipitation uses only complete calendar years
  from the pinned context record. Context products are not bird measurements and
  missing precipitation is never imputed.
- Raw observed richness and detection counts should not rank sites with unequal
  support; use the common-count rarefied result with coverage for comparison.

## Publication checklist for replacement takeaways

When the candidate is green, any numerical replacement for this note must state:

1. the exact git revision and successful validation run;
2. source-receipt release, DOI, retrieval time, and 47-site roster;
3. the common valid-count rarefaction target and 2017–2024 analysis window;
4. the total valid-count support and its positive/supported-zero split, plus the
   separately labelled positive, supported-zero, and unavailable point-year audit
   totals behind each reported result;
5. uncertainty and support diagnostics for extrapolations or associations;
6. context-product support for every environmental comparison;
7. the explicit caveats above.

Until then, the app's old 46-site numbers are archived history, not Pass 7
evidence.

# Driver knowledge package — Breeding Birds Pass 7

## Decision

**CONTEXT / HOLD DRIVER INGESTION / NO DRIVER BYTE CHANGE.** Breeding birds
provide an effort-standardized consumer-community context layer for the Driver
Cascade. They do not carry a sub-annual driver lag: NEON point counts occur only
once or twice during the breeding season. The Driver may use pooled,
opportunity-complete, rarefied richness as descriptive corroboration, but must not
promote the detection index, raw richness, or a site-level association to a causal
driver claim.

## Evidence contract

- Source: NEON `DP1.10003.001`, `RELEASE-2026`, DOI
  `10.48443/v6hs-mx57`, generated 2026-01-23.
- Scope: exact 47-site release roster including PUUM; published bird record ends
  in 2024, so provisional 2025 observations are not release evidence.
- Effort authority: `brd_perpoint`, physical key
  `siteID + eventID + plotID + pointID`.
- Incidence grain: each authoritative valid physical six-minute count
  (`survey_id`). Repeated bouts are explicit samples, not independent places or
  occupancy replicates. `pointkey x year` remains the annual/audit grain.
- Eligible join: valid unambiguous visit + formal minute 1–6 + safely canonicalized
  species/subspecies identification + known detection method + positive finite
  integer cluster count + non-flyover.
- Community taxon grain: one normalized genus + species binomial. Parent species
  and subspecies collapse to one unit; exact reported taxonomy remains provenance.
- Supported zero: at the incidence grain, a valid physical count with no eligible
  detection; at the annual/audit grain, a point-year with at least one valid bout
  and no eligible detection. A point-year with no valid bout is unavailable, not
  zero.
- Primary cross-site signal: 2017–2024 sample-incidence richness rarefied to the
  minimum complete physical-count support, with coverage, target count, and site
  count disclosed. Rarefaction standardizes count support only.
- Birds per count: detection index only; never population, abundance, density,
  occupancy, or a Driver response variable.
- Environmental channels: RELEASE-2026 air temperature, precipitation, and plant
  phenology are contextual. Source support is temperature 47/47, precipitation
  20/47, and phenology 47/47, but source presence does not guarantee a complete
  estimand. BARR and TOOL each support only one of two realized bird-count months,
  so their breeding-window temperature is `NA`, never imputed. Both remain in
  every 47-site bird, Search-site, and export roster; only the finite-temperature
  gradient uses 45 of 47 sites.

## Pass 7 learning

The previous bundles stored positive detections and lifetime point summaries but
discarded the visit ledger. A computed impractical-visit filter was never used.
Consequently, empty sampled point-years were indistinguishable from no survey,
all incidence metrics silently conditioned on detection, and denominators could
include unusable attempts. Flyovers were removed only from one numerator while
still inflating richness and other community surfaces. Distance sentinels were
converted to undifferentiated missing values, and compound detection methods were
collapsed inconsistently.

The prior taxon grain also allowed a parent species and its reported subspecies
to inflate richness as separate units. Pass 7 schema v4 makes the biological
species binomial executable across opportunity summaries, incidence, rarefaction,
Chao2, annual and map richness, Bird Board, search, observer aggregates, and
exports. Unsafe nomenclature, unknown methods, and noninteger cluster counts fail
closed while remaining in the privacy-safe site audit.

Pass 7 replaces that model with auditable `visits`, `opportunity`, `obs`, `held`,
`points`, and `meta` channels. It preserves raw feasibility, observer, method,
flyover, and distance state; makes supported zeros explicit; and applies one
protocol-filtered predicate across community metrics and exports. Physical counts
are the complete incidence universe; point-years remain visible for annual/audit
support. The release pipeline fetches immutable sources into empty staging,
validates independently, and publishes review branches rather than unchecked
bytes to `master`.

The pre-pass network Search and national picker also mixed lifetime site/taxon
values into the comparative surface. Pass 7 binds those artifacts to the same
2017–2024 physical-count universe as the continental gradient. Their full taxon
and site frames are independently reconstructed, so a pre-window-only detection or
a stale lifetime richness value fails release validation.

## Driver use and prohibitions

The Driver may describe a pooled cross-site association only after exact-release
rebuild and support-complete validation. It must show `n`, coverage, the
rarefaction target, missing environmental support, and the space-for-time/biome/
latitude confounding boundary. Any Spearman result is descriptive `rho + n`, not
an independent-site confidence interval. It must not infer that warming at one
site causes richness change, treat a non-detection as absence, compare raw
richness across unequal effort, infer breeding status/territory from eligibility
or singing, or use flyover-dominated counts as an on-point community signal.

## Promotion state

This package remains **CONTEXT** before and after Pass 7. The engineering and
opportunity contracts are candidates for **ADOPT** across other observational
apps. The app-local release gate is now complete:

- [Run 30454799557](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30454799557)
  produced, independently validated, and published exact review candidate
  `e3ec1cd35cc75891ac6eebd87da307d8266f8ca5`. Its full oracle covers 47 sites,
  26,365 valid physical counts including 117 supported-zero counts, and 24,509
  supported point-years including 79 supported-zero point-years.
- Release identity
  `sha256:28cf09453f25d5d8fc509d414c7549fbefec45f6f89dc611946360944976a3ac`
  binds the exact source/environment receipts, payload, and manifest contract. The
  exact manifest covers 121 runtime files and 91 pinned packages.
- Recovery PR [#3](https://github.com/tgilbert14/NEON-Breeding-Birds/pull/3)
  used exact head `ffd0f05d13a716118d1efc63a0abbbfaca7f054a`; exact-head
  [CI 30817207865](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30817207865)
  passed before merge. Candidate, recovery head, and merged production revision
  `97c3e4c25b69068c7d8b3d56bc3da3bc019e5097` share tree
  `61cd60092c87e2e127e0baeef9ae3a1f0447b8f3`, so recovery changed no candidate
  code, data, manifest, receipt, or poster byte.
- Exact-master [CI 30818593951](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30818593951),
  [Pages deployment 30818592101](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30818592101),
  and [production smoke 30818593688](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30818593688)
  all succeeded. Production smoke verified that Pages and Connect served the same
  exact validated release instance.

These receipts promote the app's exact source/context and opportunity contracts,
not a causal Drivers result. The final disposition is therefore **CONTEXT / HOLD
DRIVER INGESTION / NO DRIVER BYTE CHANGE**. A future Driver-specific decision may
adopt the reusable opportunity contract or descriptive pooled context only after
register/implication reconciliation in the Driver repository; this app-local docs
closeout does not rebuild or mutate any Driver artifact.

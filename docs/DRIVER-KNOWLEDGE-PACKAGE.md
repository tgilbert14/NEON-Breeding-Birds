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
  estimand. BARR and TOOL each support only 1 of 2 realized bird-count months
  (July, not June), so their breeding-window temperature is `NA`, never imputed.
  Both sites remain in every 47-site bird, Search-site, and export roster; only the
  finite-temperature gradient uses 45 of 47 sites.

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
apps. A synthetic 47-site schema-v4 rehearsal, independent raw/bundle oracles,
positive/all-zero runtime lifecycles, local responsive checks, and exact
direct/chunked environmental producer parity pass.

Official run 11 (`30413616743`, head `24041e05e6c17ce7ae68b0b23e20921a3e3a6839`)
completed the exact 47-site bird producer, evidence sanitizer, and privacy scan,
then was intentionally cancelled after 17 environmental sites because observed
throughput could not credibly fit the 300-minute producer cap. Official run 12
(`30419237413`, head `ef9ae6103b150ab0a1c376b1c408f821717ea556`)
completed the first three four-site environmental chunks and was intentionally
cancelled while the fourth chunk returned so the timeout-adjusted head could be
used. The resulting missing-private-root message was the expected fail-closed
effect of cancellation cleanup, not a scientific or shard-isolation failure.
Neither run uploaded artifacts or reached the independent validator.

The batching path reduces environmental network requests from 141 site-product
calls to 36 bounded chunk-product calls without changing candidate bytes. Its
first three complete official chunks project to roughly 301 minutes for all 12
chunks; timeout-only commit `7bd89bea6d14146e8b3320df40123a9be40029d0`
raises the producer cap to 360 minutes. That is planning capacity, not a release
receipt.

Official Run 13
([30424003027](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30424003027),
exact head `7bd89bea6d14146e8b3320df40123a9be40029d0`) completed producer job
`90486375883` but failed validator job `90538428552`; publisher job `90543288734`
skipped. The producer receipts cover all 47 sites, 27,076 `brd_perpoint` rows,
373,518 `brd_countdata` rows, and environmental source support of 47/20/47. The
bird and environmental receipt SHA-256 values are respectively
`55f30d251428cb932ce4fb474bd394ed5a14f3f7c0d077bfb6b7fa9193db88bc` and
`03f4bc77a8cea49cc5160e250aff50ffb717093fe1b5dd02b73006e9da85662e`.

The exact Run 13 producer, raw-evidence, and environmental-evidence artifact IDs
are `8719216871`, `8719217732`, and `8719219169`. Their GitHub archive SHA-256
values are respectively
`62b3d16b3e598ef579b3467ac991d365dc57981c95d638a1a3029c68972982a4`,
`8767b9f37aa5093f3d3ce07643d28fe87c61da939ab3d0cf9d34ff2d51c21d45`,
and `9f467bf7557934cb35769cb8e071c4b79f3ed8b1b54ad8e0a694ad97bdfe7450`.
The full names, sizes, and inner archive hashes are preserved in
`docs/BUILD-TEST-HANDOFF.md`. These are unvalidated producer artifacts, not Driver
evidence eligible for ingestion.

The Run 13 clean validator independently accepted all 47 bird and environmental
evidence bundles, then the old derived rule correctly stopped at BARR's 1/2
realized-month temperature support. Finalized contract commit
`2da56ee499c10064b47b468bd23330fba6b35892` retains BARR and TOOL with an
explicit missing aggregate and no imputation. A local exact-gate audit against the
preserved Run 13 evidence passed the full oracle: 26,365 valid physical counts
(117 supported-zero counts) and 24,509 supported point-years (79 supported-zero
point-years). It retained 47 site-index, climate, cross-site, Search-site, and
export rows; Search contains 3,632 taxon-site rows, 530 taxa, and all 47 sites.
Two derived rebuilds had the exact SHA-256 values recorded in the build handoff.
That local result is repair evidence only and does not promote the Driver package.

### Run 14 pending promotion receipts

Run 14
([30454799557](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30454799557))
was dispatched on exact head `2da56ee499c10064b47b468bd23330fba6b35892` and
was still in progress at this documentation cut. No Run 13 or local value may be
copied into its placeholders.

- **PENDING JOB/ARTIFACT RECEIPTS:** final producer, validator, and publisher job
  identities/conclusions plus exact produced, evidence, and validated-candidate
  artifact names, IDs, sizes, archive/inner hashes, or explicit skipped status.
- **PENDING RELEASE RECEIPTS:** independent counts/support, two-build hashes,
  payload/manifest/release-stamp identities, and exact automation-branch head.
- **PENDING PR/MERGE:** reviewed candidate diff, exact-head green CI, merge commit,
  and default-branch verification.
- **PENDING PRODUCTION:** exact Pages and Connect identities, semantic smoke,
  opportunity/supported-zero lifecycle, fixed-window surfaces, responsive QA, and
  accessibility QA.
- **PENDING DRIVER DOCS-ONLY CLOSEOUT:** register/implication reconciliation after
  production, with no Driver artifact rebuild or byte change.

Until those fields are replaced with exact official evidence, the disposition is
**CONTEXT / HOLD DRIVER INGESTION / NO DRIVER BYTE CHANGE**. Driver register
reconciliation is docs-only and occurs after those receipts; app artifact hashes
stay unchanged.

# Breeding Birds scientific contract

## Scope and source

This app consumes NEON Breeding landbird point counts `DP1.10003.001` from the
immutable `RELEASE-2026` release, DOI `10.48443/v6hs-mx57`, generated
2026-01-23. The release roster is exactly 47 sites, including `PUUM`, and the
published time availability is 2013-06 through 2024-07. Source identity,
retrieval toolchain, row counts, exact scientific evidence allowlists, a digest
of the complete producer-local response, a separate digest of the cross-job
evidence projection, and the exact site roster are recorded in
`data/source_receipt.json`. The complete-source digest is a producer attestation:
the producer checks it before bundling, but the private source object does not
cross the job boundary. The projection digest and every scientific visit,
detection, eligibility, and opportunity derivation are independently verified in
the clean validator.

Environmental overlays are context only. Their pinned RELEASE-2026 identities
are air temperature `DP1.00002.001` / `10.48443/p69b-5e50`, precipitation
`DP1.00044.001` / `10.48443/v29j-eg88`, and plant phenology
`DP1.10055.001` / `10.48443/p75s-7p48`. Precipitation support is partial and is
never imputed. Environmental relationships are descriptive space-for-time
associations, not causal effects, forecasts, or bird measurements.

## Survey and opportunity grains

`brd_perpoint` is the authority for sampling effort. One physical visit is keyed
by `siteID + eventID + plotID + pointID`; year and bout are retained and checked.
Only a visit with `samplingImpractical == "OK"` is a valid six-minute count.
Missing, impractical, conflicting, duplicated, or otherwise ambiguous visits do
not enter denominators. They fail the build or remain explicitly held.

The sample-incidence unit is one authoritative valid physical six-minute count,
keyed by `visits$survey_id`. One or two bouts at a point-year remain separate
sample units for Chao2, coverage, rarefaction, accumulation, and Board detection
frequency. They are repeated protocol samples, not independent places, years, or
occupancy replicates. The `pointkey x year` opportunity ledger remains the
authority for annual support and audit summaries. Therefore:

- `visits`: every attempted physical count, its raw feasibility state, and its
  reconciled `positive` / `supported_zero` / `unavailable` outcome.
- `opportunity`: every attempted point-year, its valid and held bout counts, and
  the corresponding annual/audit outcome.
- `positive`: at least one eligible in-window non-flyover detection at the stated
  grain.
- `supported_zero`: valid survey support and no eligible in-window non-flyover
  detection, including flyover-only or coarse-identification-only records.
- `unavailable`: no valid count at the stated grain. This is missing support,
  never zero.

A point-year with one positive count and one supported-zero count is positive in
the annual/audit ledger while retaining both physical-count outcomes. The two zero
ledgers are therefore validated and reported separately.

Detections can never create survey effort. A detection without an unambiguous
matching visit is held and cannot enter any scientific metric.

`pointCountMinute` defines the within-visit protocol window. The raw token is
retained as `pointCountMinuteRaw`, alongside its parsed integer, provenance
state, and `in_protocol_window` flag. Only integer minutes 1–6 are part of the
formal count. NEON minute 88 is an incidental observation outside that window;
blank and unparseable values have missing/unknown support. These rows are held
with an explicit reason and never contribute to a numerator, incidence record,
map, search result, or flyover audit summary.

This interpretation follows the NEON *Bird User Guide*, Revision F, §3.8, which
defines `pointCountMinute == 88` as an incidental rare/unusual observation made
outside the formal six-minute count.

## Eligible protocol-filtered detections

Community metrics use records that are all of:

1. joined to an unambiguous valid visit;
2. recorded in formal `pointCountMinute` 1–6;
3. identified at the species or subspecies level and safely canonicalized to a
   genus + species binomial community unit;
4. recorded with a nonblank, non-`unknown` detection method;
5. carrying a positive finite integer `clusterSize` count;
6. not recorded as a flyover.

One biological species is one community unit. A reported parent binomial and any
reported subspecies trinomials collapse to the normalized first two scientific-
name tokens. Species-rank names with extra tokens, malformed names, and other
nomenclatural forms that cannot be safely resolved from `scientificName +
taxonRank` fail closed. Every row retains exact `reportedTaxonID`,
`reportedScientificName`, `reportedVernacularName`, and `reportedTaxonRank`
provenance. Display names prefer a reported parent-binomial common name; when no
parent row exists, they use a deterministic reported-name fallback.

In-window flyovers remain in the auditable detection table and exports but are
operationally excluded from on-point richness, ubiquity, incidence, Chao2,
accumulation, maps, search ranks, and the birds-per-count numerator. This filter
does not infer territory or breeding status. Out-of-window incidentals,
missing/unknown minute rows, missing/unknown detection methods, unsafe taxonomy,
and invalid cluster counts remain traceable in the held ledger. All source rows
are conserved.

## Metric meanings

- Birds per count is `eligible non-flyover birds / valid physical bouts`. It is a
  detection index, not abundance, density, occupancy, or population size.
- Detection frequency is the percentage of valid physical six-minute counts on
  which an eligible species was detected. It is a sample detection summary, not
  detection-corrected occupancy or a measure of geographic spread.
- Richness is the count of distinct eligible canonical biological-species units;
  subspecies never add richness beside their parent species.
- Chao2, sample coverage, incidence rarefaction, and accumulation use the full
  valid physical-count universe, including supported-zero counts. The
  bias-corrected Chao2 point is
  `Sobs + ((T-1)/T) * Q1 * (Q1-1) / (2 * (Q2+1))` for all `Q2 >= 0`.
  Chao2 is suppressed as a headline when `Q2 < 3`; its interval is unavailable
  when singleton support is insufficient (including `Q1` equal to 0 or 1) rather
  than shown as a zero-width interval.
- Chao/Jost incidence coverage is the estimated share of total incidence
  probability represented by detected species, equivalently the chance that a
  new standardized incidence belongs to an already-detected species. Its
  explicit `Q2 == 0` correction uses
  `A = (T-1)(Q1-1) / ((T-1)(Q1-1) + 2)`.
- Across-site bird comparisons use a common 2017–2024 analysis window and lead
  with incidence richness rarefied to the smallest valid physical-count support.
  Site pages may describe the full 2013–2024 release. Raw richness, raw detection
  counts, and lifetime site-index values never leak into the cross-site result.
- The cross-site artifact carries its analysis bounds and window-specific support:
  valid counts, positive counts, supported-zero counts, points, eligible birds,
  species-count incidences, singletons, doubletons, and the common rarefaction
  target. Positive plus supported-zero counts must equal `T_counts`. Contextual
  top species, detection index, Board frequency, and point footprint are recomputed
  inside the same window rather than joined from the lifetime site index.
- Rarefaction standardizes the number of sample counts only. Residual differences
  in completeness, detectability, space, timing, and survey design remain, so
  coverage and count support accompany the result.
- Hill q1 and q2 are unstandardized plug-in diversity summaries of the observed
  sample-incidence frequencies. They are not rarefied, effort-robust, or
  detection-corrected.
- Accumulation describes only the shape of the sampled curve; it does not predict
  whether additional counts would add species.
- A yearly species series is zero only for a surveyed year with no eligible
  detection. A year with no supported opportunity is `NA`/absent support.

## Environmental context window

The bird analysis window and environmental source record have different roles.
Valid bird counts from 2017–2024 determine the distinct calendar months used for
`breeding_temp_c`. For each realized month, the app first requires a
coverage-qualified RELEASE-2026 calendar-month temperature climatology, then gives
each realized month equal weight. Every realized count month must be supported or
the comparison fails closed.

`precip_annual_mm` is the mean of complete 12-month calendar-year totals in the
pinned RELEASE-2026 environmental record. It is not restricted to bird detections,
not a bird response, and not imputed. Its number of complete years and environmental
year bounds accompany the value; sites without a complete precipitation year remain
`NA`. The release has a precipitation source stream at 20 of 47 bird sites; source
availability alone does not substitute for a complete year. Monthly phenology and
temperature displays are seasonal context, not evidence of a within-season bird
response.

## Method, observer, and distance channels

The raw detection method is preserved. Component flags for singing, calling,
visual, and drumming make compound methods auditable; a canonical channel is a
display aid only. A compound known method remains eligible unless it contains a
flyover component. Missing and literal `unknown` methods fail closed rather than
being silently treated as non-flyovers. Singing is a recorded detection channel,
not proof of territory or breeding status. Flyover status is independent and has
priority for eligibility.
Raw observer values are used only inside temporary producer-local staging to
derive site/species support counts. Before any cross-job upload, each source RDS
is replaced with the exact allowlisted `brd_perpoint` and `brd_countdata`
scientific projection and checked against its independent projection digest.
Personnel tables, `measuredBy`, free-text sampling-impractical remarks, and
source UIDs are excluded. Public bundles and exports contain only aggregate
observer counts; the clean validator checks their schema and evidence-derived
bounds without receiving identities or row-level aliases. Those observer
aggregates are therefore producer attestations, not independently recomputed
identity counts.

Schema-v4 public detection rows carry canonical and reported taxonomy plus the
explicit canonicalization and detection-method states. The About tab exposes a
privacy-safe site-wide `obs + held` audit CSV, so flyover-only, coarse-only,
unsafe-taxonomy, missing-method, invalid-cluster, and other held rows remain
reachable even when no species profile exists. Observer identity, personnel,
free text, and source UIDs remain excluded.

`observerDistanceRaw`, numeric `observerDistance`, and `distance_state` are all
preserved. States are `observed`, `sentinel_not_estimable` for 999/9999,
`source_missing`, and `invalid`. Only finite observed distances enter distance
summaries. The area-standardized distance panel is a relative detection-frequency
signature conditioned on valid survey effort; it is not density and not a fitted
detection function. The plotted profile is explicitly truncated to 0–200 m and
reports both the number used and the number of otherwise-observed distances above
that plotting limit.

## Fail-closed tests

Registered fixtures must cover supported-zero counts and point-years, unavailable
counts and point-years, two-bout separation for incidence, point-year aggregation
for annual audit, duplicate/conflicting/orphan joins, flyover-only surveys, mixed
flyover/eligible detections, point-count minute 88, missing and unknown minute
tokens, row-order invariance, compound and missing methods, noninteger cluster
counts, parent plus multiple subspecies collapse, parent-name display preference,
all distance states and 0–200 m truncation accounting, empty incidence columns,
bias-corrected Chao2 point/variance fixtures, the `Q2 == 0` coverage branch,
2017–2024 cross-site exclusion, positive + supported-zero count conservation,
`U_incidence` / `Q1_incidence` / `Q2_incidence` re-derivation, common rarefaction
support, window-specific context fields, realized-month climate support, site-wide
row conservation, and export/codebook parity. Release validation also checks every
real bundle, exact 47-site roster, release/DOI/schema identities, conservation
identities, semantic markers, independent manifest equality, exact cross-job
evidence tables and columns, projection digests, prohibited-field scans, and
email-like-value scans.

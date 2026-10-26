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

Environmental retrieval may use deterministic chunks of at most four canonical
sites as a transport optimization only. Each complete `loadByProduct()` response
is checked for exact requested-site scope and required table support, then split
before scientific table or stream selection into private site/product shards.
Unknown metadata, foreign sites, ambiguous columns, missing required source-table
support, and path or ownership violations fail closed. Shards are mode 0600 under
a marked mode-0700 job-local root, are re-audited at read time, are removed
immediately after site consumption, and never cross the job boundary. A present
source stream can still lack a coverage-qualified value for one realized month;
that scientific support boundary follows the explicit no-imputation rule below.
The exact public bundles, validation evidence, support counts, and receipt must be
byte-identical to direct site-local retrieval; full 47-site direct/chunked parity
is a required producer regression. Chunking changes neither environmental meaning
nor the independent validation contract.

## Survey and opportunity grains

`brd_perpoint` is the authority for sampling effort. One physical visit is keyed
by `siteID + eventID + plotID + pointID`; year and bout are retained and checked.
Only a visit with `samplingImpractical == "OK"` is a valid six-minute count.
Missing, impractical, conflicting, duplicated, or otherwise ambiguous visits do
not enter denominators. They fail the build or remain explicitly held.

Impractical visits carry `count_hold_reason = "sampling_impractical"`; valid
counts carry `NA`. A missing `clusterSize` never withdraws a count (see Eligible
protocol-filtered detections). The raw `clusterSize` token is retained as
`clusterSizeRaw`, `clusterSize` holds the count actually used, and
`cluster_size_state` records which (see Cluster sizes).

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
  never zero. A missing `clusterSize` never makes a count unavailable.

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

Community metrics use records (`enters_incidence`) that are all of:

1. joined to an unambiguous valid visit;
2. recorded in formal `pointCountMinute` 1–6;
3. identified at the species or subspecies level and safely canonicalized to a
   genus + species binomial community unit;
4. recorded with a nonblank, non-`unknown` detection method;
5. not recorded as a flyover.

A `clusterSize` is not required for incidence. Presence, supported zeros,
richness, detection frequency, ubiquity, Chao2, coverage, rarefaction,
accumulation, maps, and search therefore use every such detection. The detection
index (`enters_index`) additionally requires a count: `cluster_size_state` is
`recorded` or `blank_read_as_one` (below). Every birds-per-count figure (site,
year, species, grid, and the 2017–2024 window) and every bird total or top
species by birds is `NA` whenever an `enters_incidence` detection in its scope is
not `enters_index`, and the number of such detections is stated beside it
(`detections_without_count`, `n_detections_without_count`,
`n_detections_without_count_window`). A missing count is never read as zero
birds.

This is the audit 2026-10-18 item 16 split. It replaces the PR #8 stopgap that
withdrew PUUM's counts as unavailable.

### Cluster sizes (wren's ruling, 2026-10-21)

`cluster_size_state` takes five values: `recorded` (a positive integer count),
`blank_read_as_one`, `blank_no_bird` (a blank line with no taxon),
`blank_unresolved`, and `invalid`. The rule, as wren signed it:

> A blank `clusterSize` on a record that names a bird (non-blank reported taxon)
> is read as one bird when its site-year is listed below and shows the recording
> signature: blank on at least half of that site-year's bird records, and `1` on
> fewer than half of its recorded sizes. NEON.DOC.014041 Rev L, SOP B, sets the
> cluster size of a non-clustered bird to 1, and these crews wrote a size only for
> groups. This reads a documented recording convention. It is not a statistical
> imputation, and NEON has not corrected these records. Listed for RELEASE-2026:
> PUUM 2018, 2021, 2024. A blank line with no taxon is not a detection. A blank
> bird record in an unlisted site-year leaves birds per count `NA` with its stated
> count and is reported for review. A present value that is not a positive integer
> (0, negative, fractional, text) is invalid and is never read as one. Validation
> fails if a listed site-year still has blanks but no longer shows the signature.

Evidence (wren, from the committed bundles): at PUUM, blanks are 75.5%, 97.1% and
95.5% of bird records in 2018, 2021 and 2024, and a recorded 1 is only 5.7 to
18.2% of recorded sizes; 616 singing records are blank and none has a size of 1.
In the 381 site-years of the other 46 sites, no bird record is missing its count
and a recorded 1 is 74 to 99.9% of recorded sizes.

The rule covers every bird record in a listed site-year: species, coarse
identifications, flyovers and minute 88. Eligibility does not change, so a flyover
read as one bird enters only the flyover audit totals. Those totals count flyovers
with a usable count (recorded or read as one); a flyover with no usable count stays
in the detection ledger and out of the audit totals. The listing lives in
`BIRD_CLUSTER_READ_AS_ONE` (`R/site_metadata.R`); the producer, both oracles and
the runtime derive the state independently, and the producer stops when a listed
site-year has blanks without the signature. Every birds-per-count figure states
how many detections it reads as one (`detections_count_read_as_one`,
`n_detections_count_read_as_one`, `n_detections_count_read_as_one_window`), and
the page carries wren's caveat: "At PUUM, NEON crews wrote a cluster size only for
groups in 2018, 2021 and 2024 and left single birds blank; the protocol counts a
single bird as 1. This figure counts each blank as one bird (881 of 1,137 birds).
If any blank was an uncounted group, the true figure is higher."

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
missing/unknown minute rows, missing/unknown detection methods, and unsafe taxonomy
remain traceable in the held ledger. No row is held for its cluster size. All
source rows are conserved.

## Metric meanings

- Birds per count is `eligible non-flyover birds / valid physical bouts`. It is a
  detection index, not abundance, density, occupancy, or population size. It is
  `NA` with a stated count of detections without a usable `clusterSize` wherever
  one exists in scope, and it states how many blanks it read as one bird. Large
  flocks inflate it.
- Detection frequency is the percentage of valid physical six-minute counts on
  which an eligible species was detected. It is a sample detection summary, not
  detection-corrected occupancy or a measure of geographic spread.
- Richness is the count of distinct eligible canonical biological-species units;
  subspecies never add richness beside their parent species.
- Chao2, sample coverage, incidence rarefaction, and accumulation use the full
  valid physical-count universe, including supported-zero counts. The
  bias-corrected Chao2 point is
  `Sobs + ((T-1)/T) * Q1 * (Q1-1) / (2 * (Q2+1))` for all `Q2 >= 0`.
  The Chao2 point does not lead when `Q2 < 3`, or `Chao2 - S_obs > 0.5 * S_obs`,
  or its 95% upper bound `> 1.7 * Chao2` (unrounded values; wren, 2026-10-21);
  the banner names the gate that fired. Both cutoffs sit in empty gaps of the
  release, so a refresh cannot flip a site back and forth. Its interval is
  unavailable when singleton support is insufficient (including `Q1` equal to 0
  or 1) rather than shown as a zero-width interval.
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
  target. Positive plus supported-zero counts must equal `T_counts`. The common
  target is the smallest 2017-2024 valid-count support in the 47-site roster
  (PUUM's 90 counts). The window bird total and index are `NA` when
  `n_detections_without_count_window` is above zero. Contextual
  top species, detection index, Board frequency, and point footprint are recomputed
  inside the same window rather than joined from the lifetime site index.
- Rarefaction standardizes the number of sample counts only. Residual differences
  in completeness, detectability, space, timing, and survey design remain, so
  coverage and count support accompany the result.
- Hill q1 and q2 are unstandardized plug-in diversity summaries of the observed
  sample-incidence frequencies. They are not rarefied, effort-robust, or
  detection-corrected.
- Accumulation describes only the shape of the sampled curve; it does not predict
  whether additional counts would add species. Its verdict uses the final slope,
  `Q1 / T` species per valid count from the same Chao2 incidence record (the
  expected richness lost by dropping one count; Colwell et al. 2012): still
  rising when `25 * Q1 >= T`, nearly flat when `100 * Q1 < T`, flat when
  `Q1 = 0`, levelling off otherwise, "gaining about one species per N valid
  counts" with `N = round(T / Q1)`.
- Ubiquity (point detection footprint) is denominated on the points with a valid
  count in or after 2018, because NEON cut the grids per site from 15 to 10 from
  2018; the numerator counts those points' detections in any year. The
  cross-site `mean_ubiquity` uses the points with a valid count in 2018-2024 and
  their detections across the 2017-2024 window, averaged over the species
  detected at those points.
- The climate gradient is descriptive. A Spearman rho is shown with a Bonett and
  Wright (2000) 95% interval, `tanh(atanh(rho) +/- 1.96 * sqrt((1 + rho^2 / 2) /
  (n - 3)))`, from 10 sites; with 6 to 9 sites (precipitation has 6) it carries
  an exact two-sided permutation p over all orderings of the mid-ranks; below 6
  sites no rho is shown. The interval assumes independent sites; nearby sites are
  alike, so it is likely too narrow.
- Richness is a minimum: NEON publishes federally listed and sensitive species
  above species level at every site, state-listed species too at HARV, BART and
  PUUM, and withholds some sensitive species at GRSM, RMNP and YELL (User Guide
  Rev G section 4; issue log 38204 and 78908). The page states this beside
  richness and Chao2, with the site's share of bird records above species level.
- NEON issue-log design notes that change how a site reads (the 2018 grid cut,
  COVID reductions, missed counts recorded only from 2020, TOOL quarantine, the
  YELL flood, SOAP and LAJA plot relocations, DELA's closure) appear in "About
  this site", above the annual table, and in the PDF report.
- A yearly species series is zero only for a surveyed year with no eligible
  detection. A year with no supported opportunity is `NA`/absent support.

## Environmental context window

The bird analysis window and environmental source record have different roles.
Valid bird counts from 2017–2024 determine the distinct calendar months used for
`breeding_temp_c`. For each realized month, the app first requires a
coverage-qualified RELEASE-2026 calendar-month temperature climatology, then gives
each realized month equal weight. If even one realized count month lacks that
support, `breeding_temp_c` is `NA`: the site and all bird evidence remain in the
47-site release, but the site is omitted from the temperature gradient and the
display states the reduced denominator. `n_realized_months` and
`n_supported_realized_months` preserve the exact boundary. Missing temperature is
never imputed, and a partial realized window is never relabeled as a breeding-season
mean.

In the exact RELEASE-2026 record, BARR and TOOL each have valid June and July bird
counts but a coverage-qualified climatology for only July (`1/2` realized months).
They remain in every 47-site bird, search, and export roster with
`breeding_temp_c = NA`; the temperature gradient therefore reports 45 of 47 sites.

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
unsafe-taxonomy, missing-method, and other held rows remain
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
counts, detections without a usable count (incidence kept, every bird figure in
scope `NA` with its stated count), parent plus multiple subspecies collapse, parent-name display preference,
all distance states and 0–200 m truncation accounting, empty incidence columns,
bias-corrected Chao2 point/variance fixtures, the `Q2 == 0` coverage branch,
2017–2024 cross-site exclusion, positive + supported-zero count conservation,
`U_incidence` / `Q1_incidence` / `Q2_incidence` re-derivation, common rarefaction
support, window-specific context fields, complete and incomplete realized-month
climate states, site-wide row conservation, 45/47 finite-temperature support, and
export/codebook parity. Release validation also checks every real bundle, exact
47-site roster, release/DOI/schema identities, conservation identities, semantic
markers, independent manifest equality, exact cross-job evidence tables and
columns, projection digests, prohibited-field scans, and email-like-value scans.

## Official release attestation

[Refresh run 30454799557](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30454799557)
ran on scientific-contract head `2da56ee499c10064b47b468bd23330fba6b35892`.
Producer job `90585528840`, clean-validator job `90662811450`, and restricted
publisher job `90672878106` all succeeded. The validator independently reconciled
every source visit and detection, reconstructed the environmental context, rebuilt
all derived indexes twice with byte equality, regenerated and verified the exact
manifest/release stamp, and passed the real positive-site, distance-profile, and
opportunity-complete all-zero Shiny lifecycles.

The official result contains 47 sites, 27,076 `brd_perpoint` source rows, 373,518
`brd_countdata` source rows, 26,365 valid physical counts including 117 supported-
zero counts, and 24,509 supported point-years including 79 supported-zero point-
years. Temperature, precipitation, and phenology source support is respectively
47/47, 20/47, and 47/47. BARR and TOOL retain incomplete realized-month
temperature support and `breeding_temp_c = NA`; all bird, Search-site, and export
surfaces retain 47 sites while the finite-temperature gradient uses 45.

The schema-v3 release stamp binds source receipt SHA-256
`55f30d251428cb932ce4fb474bd394ed5a14f3f7c0d077bfb6b7fa9193db88bc`,
environment receipt SHA-256
`03f4bc77a8cea49cc5160e250aff50ffb717093fe1b5dd02b73006e9da85662e`,
payload SHA-256
`82bbbcd2ea4e478d4e1cae60823363d6fb49f9cb72498569393ff146dcc3b252`,
manifest-contract SHA-256
`80ac001287046d72983a2e1ee4fd6c5354d75b19b4f1c451d913d7c8d210b9cf`,
and release ID
`sha256:28cf09453f25d5d8fc509d414c7549fbefec45f6f89dc611946360944976a3ac`.
The final manifest covers 121 runtime files and 91 pinned packages. These hashes
record the Pass 7 release of 2026-07-29. Later code and data changes moved the
payload and release identity; the current values live in `data/release_stamp.json`
(byte-identical to `docs/release.json`), which CI and the post-deploy smoke verify.

Publisher output `e3ec1cd35cc75891ac6eebd87da307d8266f8ca5`, recovery head
`ffd0f05d13a716118d1efc63a0abbbfaca7f054a`, and merged production revision
`97c3e4c25b69068c7d8b3d56bc3da3bc019e5097` all have tree
`61cd60092c87e2e127e0baeef9ae3a1f0447b8f3`; no scientific, manifest, data, or
poster byte changed during recovery. Exact-head
[PR CI 30817207865](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30817207865)
and exact-master
[CI 30818593951](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30818593951)
both passed the complete scientific and generated-byte contract. Pages
[deployment 30818592101](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30818592101)
and [production smoke 30818593688](https://github.com/tgilbert14/NEON-Breeding-Birds/actions/runs/30818593688)
succeeded; the smoke verified that Pages and Connect served the same exact release
instance. This production attestation changes no estimand and does not convert any
descriptive association into a causal result.

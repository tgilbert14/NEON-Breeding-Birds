# NEON Breeding Bird Explorer

An unofficial R/Shiny explorer for NEON **Breeding landbird point counts**
(`DP1.10003.001`). Pass 7 is pinned to the immutable **RELEASE-2026** dataset
(DOI [`10.48443/v6hs-mx57`](https://doi.org/10.48443/v6hs-mx57)) and its exact
47-site roster, including `PUUM`.

- **Living Poster:** <https://tgilbert14.github.io/NEON-Breeding-Birds/>
- **App:** <https://019ee116-75d9-5940-8ccd-9b8c7afabce4.share.connect.posit.cloud/>

The app is bundle-only at runtime. It does not call the NEON API when a visitor
opens it; the published data, receipts, derived indexes, and dependency manifest
are reviewed and committed together.

## The scientific grain

The structural `brd_perpoint` table is the effort authority. A physical visit is
`siteID + eventID + plotID + pointID`, and only a visit with
`samplingImpractical == "OK"` enters a denominator. Each valid six-minute bout is
one sample-incidence unit. One or two bouts aggregate to a supported
`point x year` opportunity only for annual support and audit summaries; repeated
counts are explicit samples, not independent places or occupancy replicates.

- A **supported zero** is a valid physical count with no eligible in-window,
  non-flyover bird detection. It remains in the incidence denominator. A separate
  point-year outcome is recomputed across all valid bouts for annual audit, so a
  point-year can be positive even when one of its bouts is a supported-zero count.
- An **unavailable** attempted count is not valid survey support. A point-year
  with no valid bout is likewise unavailable. Neither state is a biological zero.
- Protocol-filtered community metrics use valid, positive species/subspecies
  detections from formal point-count minutes 1–6 with a known detection method
  and exclude flyovers. Parent species and reported subspecies collapse to one safe,
  normalized binomial community unit; exact reported names, ranks, taxon IDs, and
  common names remain provenance. Unsafe nomenclature, missing/unknown methods,
  minute 88 incidentals, missing/unknown minutes, flyovers, and other held records
  remain auditable in the bundle and site-wide export.

This distinction drives richness, Chao2, sample coverage, rarefaction,
accumulation, annual series, and the Bird Board.

## How to read the app

- **Overview** summarizes detected species and the birds-per-count detection
  index. The index is not abundance, density, occupancy, or population size.
- **Community** uses the complete valid physical-count universe, including
  supported-zero counts. Chao2 is the bias-corrected sample-incidence
  extrapolation; unstable estimates do not lead. Accumulation describes only the
  sampled curve and does not predict what additional counts would find.
- **Bird Board** leads with **detection frequency**: the percentage of valid
  six-minute counts on which a species was detected. It is a sample detection
  summary, not geographic spread or detection-corrected occupancy.
- **Species Profile** carries visit and opportunity support, annual supported
  zeros, QC provenance, and a relative area-and-effort-standardized distance
  signature. The distance bars are explicitly truncated to 0–200 m, disclose
  otherwise-observed distances beyond that limit, and are neither density nor a
  fitted detection function.
- **Across the continent** defaults to incidence richness rarefied to a common
  number of valid counts in the common 2017–2024 window. This standardizes sample
  count only; it is a descriptive space-for-time comparison, not a causal model
  or forecast. The optional Hill summaries are unstandardized plug-in summaries,
  not effort-robust or detection-corrected alternatives.

Environmental overlays are context only. The 2017–2024 bird window selects the
calendar months used for breeding-temperature context; the temperature values are
coverage-qualified RELEASE-2026 month climatologies. Under that release,
temperature and phenology support all 47 bird sites; precipitation has a source
stream at 20 of 47, is summarized only from complete calendar years, and is never
imputed.

## Bundle contract

Each `data/sites/<SITE>.rds` is schema v4:

```text
list(obs, visits, opportunity, points, held, meta)
```

`obs` retains canonical species-community units beside exact source-reported
taxonomy, raw and interpreted point-count minute fields, raw and canonical
detection-method fields, flyover eligibility, and both raw and interpreted
distance state. Missing/unknown detection methods fail closed. `visits` contains
physical survey attempts plus count-level support/detection outcomes;
`opportunity` contains point-year support and annual/audit outcomes;
`held` preserves rows that cannot enter scientific metrics; `meta` binds the
bundle to the release receipt and schema. Observer support is published only as
site/species aggregate counts inside `meta`; raw `measuredBy` values, row-level
pseudonyms, free-text sampling remarks, personnel tables, and unused source-row
identifiers remain producer-local and are removed before cross-job transfer. The
validator receives only the receipt-bound, exact two-table scientific evidence
projection needed to reconcile every visit and detection row.

The About tab exposes a privacy-safe site-wide detection audit CSV so flyover-only,
coarse-only, unsafe-taxonomy, and held-only rows do not depend on appearing in the
species picker. The existing per-species export remains available.

The complete interpretation contract is in
[`docs/SCIENCE-CONTRACT.md`](docs/SCIENCE-CONTRACT.md). Build, verification, and
release evidence live in
[`docs/BUILD-TEST-HANDOFF.md`](docs/BUILD-TEST-HANDOFF.md).

## Run locally

Use R 4.5.2 with the packages pinned by `manifest.json`, then run from the repo
root:

```r
Sys.setenv(BRD_LIVE = "0")
shiny::runApp(".", port = 8192)
```

The app intentionally fails closed on stale or incompatible bundles. A local run
therefore requires the validated RELEASE-2026 candidate and never falls back to a
live fetch or partial roster.

## Refresh and release

Do not fetch into `data/` or push generated data directly to `master`. The
[`refresh-data` workflow](.github/workflows/refresh-data.yml) produces the exact
47-site release in empty staging, validates it in a clean checkout, rebuilds all
derived indexes twice, verifies the bundle and manifest, and publishes only the
`automation/breeding-birds-release-2026` review branch. Merging that reviewed,
green head is the explicit deploy decision.

See [`DEPLOY.md`](DEPLOY.md) for the operator procedure.

Built by Desert Data Labs · desertdatalabs@gmail.com. Not affiliated with
NEON, Battelle, or NSF.

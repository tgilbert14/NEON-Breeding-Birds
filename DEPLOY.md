# Deploy and release runbook

The NEON Breeding Bird Explorer has two public surfaces built from `master`:

```text
GitHub Pages (`docs/`)             Posit Connect Cloud (R/Shiny)
Living Poster and app route       App code + exact committed runtime files
marker: breeding-birds-poster-v1  marker: breeding-birds-release-2026-v1
```

The application is bundle-only. Production must never fetch from NEON, silently
fall back to a partial site set, or rebuild data during startup. Connect restores
the exact `manifest.json` and reads the committed schema-v4 site bundles.

## Release identity

- Bird product: `DP1.10003.001`
- Release: `RELEASE-2026`
- DOI: `10.48443/v6hs-mx57`
- Required roster: exactly 47 sites, including `PUUM`
- Context support: temperature 47/47, phenology 47/47, precipitation source
  stream 20/47 (annual values require a complete calendar year)
- Runtime R: 4.5.2
- Default/deploy branch: `master`
- Data review branch: `automation/breeding-birds-release-2026`

Precipitation gaps are release availability, not zeros, and are never imputed.

## Normal release procedure

### 1. Start the candidate workflow

Run **Actions → Propose immutable NEON breeding-bird refresh → Run workflow** on
the exact reviewed code revision. A full release requires the repository secret
`NEON_TOKEN`. Every run performs a new immutable-release fetch into empty staging;
there is no committed-data bypass.

The workflow is deliberately split into three trust boundaries:

1. **Producer** fetches the immutable release into empty temporary staging,
   writes source and environmental receipts, derives aggregate observer support,
   then replaces bird source files with the receipt-bound two-table privacy-safe
   evidence projection before upload.
2. **Independent validator** starts from a clean checkout, materializes the
   candidate plus separately packaged bird and environmental evidence, scans the
   bird evidence allowlists and values, reconciles every scientific row, rebuilds
   derived indexes twice, verifies deterministic equality, runs bundle/manifest
   oracles, and sources the complete app offline.
3. **Restricted publisher** may update only
   `automation/breeding-birds-release-2026`. It cannot push to `master`; a
   reviewer opens the pull request with a reviewer-authenticated GitHub session,
   which starts normal exact-head CI. The publisher's `GITHUB_TOKEN` branch push
   cannot start a `push` workflow run. If automation later updates that existing
   pull request, GitHub creates approval-required `pull_request` synchronize runs;
   a repository write user must select **Approve workflows to run** before those
   exact-head checks can execute.

A rebuild of the same release is a no-op. The validator computes a data content
key over `data/` and `data-sample/` that ignores the retrieval tool versions in
both receipts, the per-bundle `meta$source_receipt_sha256`, and
`data/release_stamp.json`, then compares it with `master`. When the keys match,
the run summary says "content unchanged; nothing published" and the publisher
does not run. A change to `manifest.json` or `docs/release.json` alone is not a
data change. When the content did change, the publisher writes a run summary
(compare link, old and new `release` and `release_id`, changed-file count) and
opens or comments on one issue titled "Breeding Birds: validated candidate
waiting for review". That issue is the signal to review the branch and open the
PR; close it after the merge.

The `release_watch` job reads NEON's anonymous product metadata on every run. It
warns when NEON lists a release newer than the pin, and it raises one warning per
NEON change-log entry dated after the pinned release's generation date. Read
those entries before the next rollover; they never change the pinned build.

### 2. Review the candidate pull request

Before merging, require all of the following on the exact PR head:

- pinned CI is green;
- the source receipt names RELEASE-2026, its DOI, and exactly 47 sites;
- the schema-v3 source-receipt contract (independent of bundle schema v4) binds
  both the complete producer-local source digest and exact privacy-safe
  evidence-projection digest for every site;
- every `data/sites/` and `data/env/` bundle has the exact roster;
- the opportunity oracle separately verifies count-level and point-year audit
  positives, supported zeros, and held/unavailable states; point-count minute 1–6
  eligibility; minute-88/missing-minute holds; flyover exclusion; and conservation
  identities;
- derived indexes are deterministic, and the cross-site artifact contains only
  window-specific 2017–2024 bird fields with valid-count support, coverage, and
  the common rarefaction target;
- `manifest.json` exactly matches the runtime file set and checksums;
- the candidate diff contains only intentional code, data, receipt, manifest, and
  documentation changes.

Open the candidate PR with a reviewer-authenticated GitHub session after the
publisher reports the exact branch SHA. That PR event must start **Validate
Breeding Birds**. Verify the successful check ran on that exact head SHA before
merge. If automation updates an already-open candidate PR, approve its pending
workflow runs, then verify the green checks belong to the new exact `headRefOid`.

Record the workflow and PR evidence in
[`docs/BUILD-TEST-HANDOFF.md`](docs/BUILD-TEST-HANDOFF.md). Merge only the green,
reviewed head. Never copy an artifact from a failed or superseded run.

### 3. Deploy from `master`

Posit Connect Cloud watches `master`; merging the exact candidate is the app
deploy. GitHub Pages serves `/docs` from `master`. Do not run a separate
`rsconnect::deployApp()` or commit an `rsconnect/` directory.

The Connect content must use this repository's `manifest.json`. Regenerate the
independent schema-v3 payload stamp (the bundle remains schema v4) only in the
pinned validator, in this exact non-cyclic sequence:

```sh
BIRD_MANIFEST_PHASE=prestamp Rscript --vanilla scripts/write_manifest.R
BIRD_RELEASE_STAMP_MODE=write BIRD_WRITE_PAGES_RELEASE=1 \
  Rscript --vanilla scripts/write_release_stamp.R
BIRD_MANIFEST_PHASE=final Rscript --vanilla scripts/write_manifest.R
BIRD_RELEASE_STAMP_MODE=verify BIRD_WRITE_PAGES_RELEASE=1 \
  Rscript --vanilla scripts/write_release_stamp.R
Rscript --vanilla scripts/verify_manifest.R
```

`neonUtilities` and other producer-only packages must not appear as runtime
dependencies.

### 4. Verify production

After both surfaces update, run:

```sh
bash scripts/post_deploy_smoke.sh
```

The smoke test requires the Pages and Connect semantic markers, not merely HTTP
200 responses. Once the exact release instance is live it also requires a
`<title>` on both surfaces and `<html lang="en">` on the app. The
**Verify Breeding Birds production** workflow runs it on every push to `master`
and every Monday at 14:30 UTC, so a Connect rollback between merges turns a
scheduled run red. Also verify the Living Poster and app at desktop, tablet, and
320–390 px mobile widths; keyboard focus; the single primary call to action; site
selection; one supported-zero physical count and one supported-zero annual audit
series; Bird Board valid-count detection frequency; the 2017–2024 cross-site
window/support copy; precipitation missingness; and the relative distance
signature's 0–200 m truncation caveat.

## Local diagnostic build

Operators may reproduce the producer in disposable directories, but should not
write network output directly into the tracked `data/` tree:

```sh
export NEON_TOKEN="…"
export BIRD_RAW_DIR="build/raw/birds"
export BIRD_RECEIPT="build/source_receipt.json"
export BIRD_OUTPUT_ROOT="build/candidate"
export BIRD_ENV_RECEIPT="build/candidate/data/environment_source_receipt.json"

Rscript --vanilla scripts/test_helpers.R
Rscript --vanilla scripts/fetch_bird_all.R
Rscript --vanilla scripts/bundle_bird_data.R
Rscript --vanilla scripts/refresh_env_data.R
```

Use a fresh, empty staging directory for every attempt. The validator owns the
derived-index rebuild, manifest generation, and release decision; a successful
local fetch is not publish authority.

## First-time hosting configuration

GitHub Pages must be configured for `master` / `/docs`. Posit Connect Cloud must
be connected to `tgilbert14/NEON-Breeding-Birds`, watch `master`, and publish the
multi-file Shiny app (`global.R`, `ui.R`, `server.R`). The current app route is
already embedded in `docs/index.html`; change it only when the Connect content URL
actually changes, then verify both semantic markers again.

### Connect content variables

Set these in Connect Cloud under the content's **Settings > Variables**. A
variable takes effect only after a rebuild, so republish once after adding or
changing one.

| Variable | Purpose | Notes |
|---|---|---|
| `CARTO_BASEMAP_KEY` | Keyed CARTO basemap tiles (read in `global.R`). | Browser-visible tile key, not a secret: restrict it to the app's origins and rotate it in the CARTO dashboard. Without it every map falls back to the Esri grey basemap. Tim sets it and republishes once, then confirms `basemaps.cartocdn.com` tile requests carry `?key=`. |

`NEON_TOKEN` is a GitHub Actions secret for the refresh workflow only. The
deployed app never reads it, so do not set it on Connect.

## Release rollover

NEON publishes a new annual release each January (RELEASE-2027 is expected in
late January 2027). The `release_watch` job warns when one appears. A rollover
moves every pin below together in one reviewed change, then runs the refresh
workflow; the candidate still arrives on the review branch and reaches `master`
only through a reviewer-opened PR. Never edit `data/` or `manifest.json` by hand.

Find every pin with:

```sh
git grep -n "RELEASE-2026\|release-2026\|v6hs-mx57\|2026-01-23\|2024-07-31\|2024-12" -- ':!data' ':!data-sample'
```

Pins that must move together:

- **Bird producer**, `scripts/fetch_bird_all.R`: `RELEASE`, `DOI`,
  `RELEASE_GENERATED`, the survey-year range check (`2013:2024`), and
  `release_start` / `release_end` (`2024-07-31` today).
- **Bundle, evidence and stamp scripts**: `RELEASE` and `DOI` in
  `scripts/bundle_bird_data.R` and `scripts/write_release_stamp.R`; the receipt
  release and DOI checks in `scripts/sanitize_bird_evidence.R` and
  `scripts/scan_bird_evidence.R`; the release attributes and roster messages in
  `scripts/build_site_index.R`, `scripts/build_cross_site.R` and
  `scripts/refresh_site_climate.R`.
- **Environmental producer**, `scripts/refresh_env_data.R`: `RELEASE`, the
  `out$source` label, the `start_d` / `end_d` window (`2024-12` today), and the
  support-count messages. Re-check the 47/20/47 support counts against the new
  release.
- **Independent oracles**: `scripts/verify_bundle.R` (release, DOI,
  `release_generated_utc`, stamp identity), `scripts/verify_raw_candidate.R`
  (release, DOI, year range, date window) and `scripts/verify_env_candidate.R`
  (release, source label, month window).
- **Fixtures**: `scripts/test_bird_evidence_privacy.R`,
  `scripts/test_release_stamp.R`, `scripts/test_verify_env_candidate.R`,
  `scripts/test_env_batch_producer_parity.R`, `scripts/test_env_batching.R`.
- **App**: `global.R` `NEON_RELEASE`, `NEON_DOI`, `NEON_DATA_SPAN` and `APP_RELEASE_MARKER`;
  the `ddl-app-ready` marker in `ui.R`; release wording in `server.R`,
  `R/bird_helpers.R` (codebook), `R/site_metadata.R` (cross-site window,
  `BIRD_CROSS_SITE_YEAR_MAX`) and `R/bird_evidence_contract.R`.
- **Pages cover**: the release line in `docs/index.html`, plus the marker check
  in `scripts/check_cover.mjs`.
- **Workflows**: `BIRD_RELEASE` in `.github/workflows/refresh-data.yml`, its
  artifact names (`breeding-birds-produced-2026`, `breeding-birds-raw-evidence-2026`,
  `breeding-birds-env-evidence-2026`, `breeding-birds-release-2026`), the review
  branch `automation/breeding-birds-release-2026`, and the bot commit message;
  the marker check in `.github/workflows/ci.yml`.
- **Post-deploy checks**: `app_marker` and the stamp `release` / `doi` checks in
  `scripts/post_deploy_smoke.sh`.
- **Documents**: this file (surface diagram, Release identity, review branch,
  review checklist), `README.md`, `AGENTS.md` (product, DOI, generation date,
  Connect marker), `docs/SCIENCE-CONTRACT.md`, `docs/DATA-TAKEAWAYS.md`, and a
  new entry in `docs/BUILD-TEST-HANDOFF.md` and `docs/DRIVER-KNOWLEDGE-PACKAGE.md`.

The new DOI and generation date come from NEON's product metadata
(`.data.releases[]` at `https://data.neonscience.org/api/v0/products/DP1.10003.001`).
Changing the Connect marker changes what `scripts/post_deploy_smoke.sh`, CI and
any Driver suite check that names it look for, so update them in the same review.

## Failure policy

- Missing token, site, receipt, structural table, context support, or manifest
  evidence is a failed release, not a warning.
- Unavailable survey opportunity is never converted to zero.
- A failed optional context fetch cannot silently preserve mixed-release files.
- Automation never resolves a failure by pushing directly to `master`.
- Production rollback is a reviewed revert of the exact merge, followed by the
  same semantic smoke check.

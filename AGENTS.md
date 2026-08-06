# Repository operating instructions

These instructions apply to the entire repository. User and platform instructions
take precedence.

## Mandatory entry point

Before inspecting, changing, testing, rebuilding, publishing, or reporting on this
repository, read `docs/BUILD-TEST-HANDOFF.md`, `docs/SCIENCE-CONTRACT.md`, and
`docs/DRIVER-KNOWLEDGE-PACKAGE.md` completely. For suite work, also read the Driver
repository's complete `docs/NEON-SUITE-LEARNING-LOOP.md`,
`docs/NEON-SUITE-REVAMP-PLAN.md`, and `docs/neonize-playbook.md`.

Start and end every session with `git status --short --branch`. Preserve changes
you did not create.

## Scientific contract

- Product: NEON Breeding landbird point counts `DP1.10003.001`, `RELEASE-2026`,
  DOI `10.48443/v6hs-mx57`, generated 2026-01-23.
- A raw sampling opportunity is one attempted point-count visit identified by
  `siteID + eventID + plotID + pointID`. Its authority is `brd_perpoint`, not the
  presence of a detection. The analysis opportunity rolls valid bouts up to one
  `pointkey x year` record while preserving the visit ledger and held bouts.
- A supported zero is a valid sampled opportunity with no eligible bird detection.
  Missed, impractical, invalid, unsupported, or ambiguous visits are unavailable,
  not zero. Never reconstruct the denominator from positive detections.
- Birds per count is a detection index, not abundance, density, occupancy, or a
  population estimate. Only formal `pointCountMinute` values 1–6 are eligible;
  minute 88 and missing/invalid minute states stay auditable but held. Flyovers
  stay auditable but do not enter the breeding-site community activity index.
- Richness and incidence use eligible species/subspecies records and the exact
  opportunity key. Coarser or ambiguous identifications cannot inflate richness.
- Preserve raw effort state, distance sentinel/state, and observer support. Invalid
  keys, conflicting states, and detections without eligible opportunities fail
  closed or enter an explicit held state.
- Environmental products are contextual, not bird measurements: air temperature
  `DP1.00002.001` covers 47/47 release sites, precipitation `DP1.00044.001` covers
  20/47, and plant phenology `DP1.10055.001` covers 47/47. Relationships remain
  descriptive/correlational and must expose support and missing-data boundaries.

## Build, release, and data rules

1. Runtime boots entirely from committed bundles and local assets. Production must
   not depend on NEON, Google Fonts, CDNs, or another network service at startup.
2. Never edit `manifest.json` by hand. Generate it only in the pinned R 4.5.2 / Ubuntu
   22.04 validator using the dated 2026-07-15 package snapshot, then validate exact
   files, checksums, dependency identities, and geospatial source URLs.
3. A refresh assembles all 47 release sites in empty staging, verifies exact release
   and opportunity contracts in a clean independent job, and publishes only a review
   PR. It never writes unchecked bytes directly to `master`.
4. Pin R, runner image, package sources, workflow actions, and release identities.
   Do not weaken a gate to make an environment pass.
5. Every Shiny custom-message handler accepts exactly one payload argument, including
   handlers that ignore it.
6. A release requires green checks on the exact review head and merge, exact manifest
   equality, Connect's `breeding-birds-release-2026-v1` marker, and Pages'
   `breeding-birds-poster-v1` marker. HTTP 200 alone is not health.
7. Pages and the app use the static Living Poster frame: one hook, one promise, one
   contextual CTA, one Driver route, local responsive art with accessible descriptions,
   and durable image provenance. Keep the illustration / data boundary in that provenance;
   the cover does not require a visible artwork badge.

## Durable closeout

Immediately before editing either durable record, re-read its latest entry. Update
`docs/BUILD-TEST-HANDOFF.md` with time zone, scope, source/release identities,
commands, expected and actual outcomes, exact revisions, failures, residual risks,
and the next concrete action. Update `docs/DRIVER-KNOWLEDGE-PACKAGE.md` with evidence,
opportunity contract, eligible joins, engineering learning, and an explicit `ADOPT`,
`HOLD`, `CONTEXT`, `COMPLEMENT`, `REJECT`, or `NONE` decision.

A companion pass is not complete until its verified package is represented in the
Driver suite register and implication backlog. Do not change Driver artifact bytes
from this app until the decision and evidence are complete.

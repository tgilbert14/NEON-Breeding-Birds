# Project status: NEON Breeding Bird Explorer

> Read this before flagging anything. Maintained by `triage` and by the app's owner session after every
> review or ship. The **Deliberately deferred** section is binding: a cold-boot agent does not re-flag what it
> lists. Suite doctrine: https://github.com/tgilbert14/NEON-Driver-Cascade/blob/master/docs/neonize-playbook.md
> (this repo's `docs/neonize-playbook.md` is a pointer once PR #9 merges). App authority:
> `docs/SCIENCE-CONTRACT.md`, `docs/BUILD-TEST-HANDOFF.md`, `DEPLOY.md`.

As of: 2026-10-26 · Version: `master` @ `67b3b39` (2026-10-21) · Stage: live; audit wave 2 in review as PR #8

## Release and data state

| Item | Value | How to check |
|---|---|---|
| Live app | https://019ee116-75d9-5940-8ccd-9b8c7afabce4.share.connect.posit.cloud/ (from `docs/index.html`) | TG-Data-Apps `monitoring/sites.tsv` |
| Cover | https://tgilbert14.github.io/NEON-Breeding-Birds/ | |
| Deployed revision | `67b3b39` (live receipt and marker match master, probe 2026-10-26) | `post_deploy_smoke` |
| NEON product | `DP1.10003.001` · `RELEASE-2026` · DOI `10.48443/v6hs-mx57` | the bundle meta |
| Data through | July 2024 (RELEASE-2026 bird months end 2024-07) | the landing view (`NEON_DATA_SPAN`) |
| Review branch | `automation/breeding-birds-release-2026` at `e97e56c`: the 2026-10-02 no-op build, superseded by PR #8 | `gh api repos/tgilbert14/NEON-Breeding-Birds/compare/master...automation/breeding-birds-release-2026` |
| Last executed refresh | run on PR #8's head (see `docs/BUILD-TEST-HANDOFF.md`) | `node .claude/scripts/neon-suite-probe.mjs --app breeding-birds` |
| 60-day schedule cutoff | 2026-12-20 | the probe |
| Suite lint | 0 red on PR #8's head (2026-10-26) | `node .claude/scripts/neon-suite-lint.mjs --app breeding-birds --worktree` |
| Open PRs, in merge order | #8 (this app's audit wave 2); #9 (doctrine session's playbook pointer) is independent | `gh pr list --repo tgilbert14/NEON-Breeding-Birds` |

## Now (P1: blocks a ship, a refresh, a demo or a user)

- [ ] Merge PR #8 once its exact-head CI is green · owner `Tim` · evidence PR #8 checks
- [ ] After the merge, confirm Connect and Pages show PR #8's release instance and markers · owner `Tim` · evidence `post-deploy.yml`

## Next (P2: real, not urgent)

- [ ] Set `CARTO_BASEMAP_KEY` on the Connect content and republish (the live map runs the Esri grey fallback) · owner `Tim` · evidence `DEPLOY.md`
- [ ] Add a master ruleset (PR required, CI checks required, no force push or deletion) · owner `Tim` · evidence probe `no-protection`
- [ ] Delete the stale review branch `automation/breeding-birds-release-2026` after PR #8 merges · owner `Tim` · evidence probe `review-branch-waiting`

## Later (P3: polish)

- [ ] Port the QC-flag keyboard handler to Mosquito, Ground Beetle, Phenology and the Atlas · owner each repo's session

## Deliberately deferred (DO NOT re-flag: decided, with the reason)

- **Monthly 5-hour fetch of an immutable release, and no `skip_download` input**: deferred because the appendix fix (gate the fetch on a release and producer-script key, publish nothing when unchanged) conflicts with this repo's playbook ban on a committed-data bypass, and that file belongs to the doctrine session. The content-key no-op already stops empty review branches. Decided by the owner session on 2026-10-26. Reopen when the doctrine session rules on `skip_download` versus the committed-data ban.
- **Output-level `bindCache` for site renders**: deferred because prepared sites are already cached across visitors and `site_birds()` reuses the board; per-output caching needs a cache-key review of every reactive. Decided by the owner session on 2026-10-26. Reopen when a site load takes more than 5 s on Connect after the merge.
- **Remote branch clutter and auto-delete on merge**: a repository setting and branch deletions are Tim's. Reopen when Tim turns on auto-delete.
- **Suite decisions already settled (never re-flag in this repo):** the review-branch refresh contract (a person's merge of the review branch is the deploy; no workflow pushes the watched branch); `skip_download` runs may publish to the review branch; terra pinned at 1.8-50; `neonUtilities` out of `manifest.json`; SRER as the default site; no visible AI-art badges; the copy voice (no em dashes, no pitch, no hype, no sincerity labels, never state what the reader can already see).

## Owner decisions pending

- `skip_download` versus the playbook's committed-data ban · options: gate the fetch on a release and producer-script key / keep the monthly full fetch · recommendation: the gate, because RELEASE-2026 is immutable and the gated run publishes nothing · asked on 2026-10-26 in `status/breeding-birds.md`

## Recently shipped (so cold-boot agents do not re-open it)

- 2026-10-21 · NEON token probe on a data endpoint before every refresh · PR #7 · merge `67b3b39` · probe live check
- 2026-10-21 · CARTO basemap authentication code path and master's manifest repair · PR #6 · merge `9afb8ee`

## Keeping this file true

- "As of" and "Version" come from git, never from memory or from this file.
- An item moves to Recently shipped only with a merge SHA and a live check.
- When a deferral's reopen condition is met, move it back to Now or Next with the evidence.

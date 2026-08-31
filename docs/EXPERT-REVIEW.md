# Expert review — Pass 7 disposition

## Status

The June 2026 expert review was performed against the superseded 46-site bundle
and pre-Pass-7 helper contracts. Its site totals, rankings, correlations, line
references, grades, and statements about implemented behavior are archived and
must not be cited as evidence for RELEASE-2026.

Pass 7 converts the useful findings from that review into explicit contracts and
fail-closed tests. The authoritative interpretation is now
[`SCIENCE-CONTRACT.md`](SCIENCE-CONTRACT.md); build and release evidence belongs in
[`BUILD-TEST-HANDOFF.md`](BUILD-TEST-HANDOFF.md). Derived ecological results remain
unreported here until the exact 47-site candidate has passed the clean validator.

## Review findings carried forward

| Review concern | Pass 7 disposition |
|---|---|
| Detection rows could be mistaken for sampling effort | `brd_perpoint` is the sole effort authority. A valid physical count requires `samplingImpractical == "OK"`. Detections cannot create effort. |
| Detection-only incidence omitted valid zero surveys | The bundle carries a complete physical-count ledger plus point-year audit opportunities. Supported-zero counts enter Chao2, coverage, rarefaction, accumulation, and Board frequency; supported-zero point-years enter annual summaries. Unavailable support enters neither. |
| Flyovers distorted the on-point community index | Flyovers remain auditable but are operationally excluded from every on-point community numerator and incidence metric. |
| Compound detection methods were collapsed or misclassified | Raw methods are retained with singing, calling, visual, and drumming component flags plus a canonical display channel. |
| Sentinel and missing distances were conflated | Raw distance tokens, numeric values, and `distance_state` are separate. `999`/`9999` are retained as `sentinel_not_estimable` provenance and never treated as metres. |
| Area-corrected distance bars could be read as density | The profile is labelled a **relative area-and-effort-standardized detection signature**, explicitly limited to 0–200 m with used/excluded counts. It is neither density nor a fitted detection function. |
| Ubiquity could be read as occupancy | The Bird Board now leads with **detection frequency across valid six-minute counts** and explicitly says it is neither geographic spread nor detection-corrected occupancy. |
| Unstable Chao2 could lead with false precision | The bias-corrected Chao2 point carries uncertainty and singleton/doubleton support diagnostics; a result with `Q2 < 3` does not lead. |
| Cross-site raw richness reflected unequal effort | The continental comparison uses the common 2017–2024 window and defaults to incidence richness rarefied to the smallest common valid-count support, with coverage disclosed. Rarefaction standardizes sample count only; Hill q1/q2 remain unstandardized plug-in summaries. |
| Downloads lacked portable interpretation | The schema and export codebook document units, eligibility, missingness, support state, and metric meanings. |

## Assertions the release review must test

The expert disposition is not complete until the exact candidate proves all of
the following on real release data:

1. The roster is exactly 47 sites and includes `PUUM`.
2. Every eligible detection joins one unambiguous valid `survey_id` physical
   count and its point-year audit record and has `pointCountMinute` in the formal
   1–6 window; minute 88 and missing/unknown minute values are held with explicit
   reasons.
3. Visit, opportunity, positive, supported-zero, unavailable, eligible, flyover,
   and held-record conservation identities close for every site.
4. Two valid bouts remain two explicit incidence samples and detection-index
   denominator counts while aggregating to one point-year annual/audit opportunity.
5. Supported zero is rendered as zero only where survey support exists, and
   count-level zeros are never conflated with point-year annual/audit zeros;
   unsupported counts and years remain unavailable.
6. Temperature and phenology have 47-site context support, precipitation has
   exactly 20-site source support, annual precipitation uses complete calendar
   years only, the 2017–2024 bird window selects realized count months, and no
   environmental gap is imputed.
7. Distance, method, aggregate observer support, and eligibility provenance survive
   bundle, derived-index, UI, and export paths without publishing row-level observer
   values, deterministic aliases, free-text sampling remarks, or unused source IDs.
8. The Bird Board, 2017–2024 national comparison, site map, search index, and
   downloads use the same eligibility, physical-count, and point-year audit
   contracts. The national artifact and UI use only window-specific bird fields;
   lifetime site-index values cannot leak into cross-site results.
9. Accumulation is labelled as a sampled curve rather than a prediction, the
   distance signature discloses its 0–200 m truncation, and exported schema-v4
   support fields make each result independently interpretable.

The automated fixtures and `scripts/verify_bundle.R` are the executable gate for
these assertions. A future expert review should examine the validated 47-site
candidate and record exact revision and workflow evidence before publishing any
new ecological takeaway.

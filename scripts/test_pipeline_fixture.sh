#!/usr/bin/env bash
# End-to-end release-chain test on a synthetic 47-site fixture: producer,
# evidence sanitizer and scanner, independent raw oracle, every derived builder
# twice (byte determinism), a scratch release stamp, and the full bundle oracle.
# It runs in a temporary copy of the tracked tree and never writes committed
# data. Usage: bash scripts/test_pipeline_fixture.sh [character|integer|double]
set -euo pipefail
MODE="${1:-character}"
REPO="$(git rev-parse --show-toplevel)"
WORK="$(mktemp -d)"
FX="$WORK/fixture"
ROOT="$WORK/root"
mkdir -p "$FX" "$ROOT"
cd "$REPO"
git ls-files --cached --others --exclude-standard | grep -v -e '^data/sites/' -e '^data-sample/' > "$WORK/tracked.txt"
rsync -a --files-from="$WORK/tracked.txt" ./ "$ROOT/"
mkdir -p "$ROOT/data/sites" "$ROOT/data-sample"
rm -f "$ROOT/data/site_index.rds" "$ROOT/data/cross_site.rds" "$ROOT/data/search_index.rds"
cd "$ROOT"
sha256() { if command -v sha256sum >/dev/null; then sha256sum "$@"; else shasum -a 256 "$@"; fi; }

echo "== fixture ($MODE)"
Rscript --vanilla scripts/make_pipeline_fixture.R "$FX" "$MODE"
export BIRD_RAW_DIR="$FX/raw" BIRD_RECEIPT="$FX/receipt.json" BIRD_OUTPUT_ROOT="$ROOT"
echo "== producer"
Rscript --vanilla scripts/bundle_bird_data.R > "$FX/bundle.log" 2>&1 || { tail -20 "$FX/bundle.log"; exit 1; }
grep -E "Bundling (PUUM|SRER|GUAN)" "$FX/bundle.log"
echo "== sanitize and scan"
Rscript --vanilla scripts/sanitize_bird_evidence.R
Rscript --vanilla scripts/scan_bird_evidence.R
echo "== raw oracle"
Rscript --vanilla scripts/verify_raw_candidate.R
echo "== derived builders, twice"
rebuild() {
  for s in build_site_index refresh_site_climate build_cross_site build_search_index; do
    Rscript --vanilla "scripts/$s.R"
  done
}
derived=(data/site_index.rds data/site_climate.rds data/site_month_clim.rds data/cross_site.rds data/search_index.rds)
rebuild
sha256 "${derived[@]}" > "$WORK/derived.sha256"
rebuild > /dev/null
sha256 -c "$WORK/derived.sha256"
echo "== scratch release stamp"
cp manifest.json "$WORK/manifest_final.json"
jq 'del(.files["data/release_stamp.json"])' "$WORK/manifest_final.json" > manifest.json
BIRD_RELEASE_STAMP_MODE=write BIRD_WRITE_PAGES_RELEASE=1 Rscript --vanilla scripts/write_release_stamp.R
cp "$WORK/manifest_final.json" manifest.json
echo "== bundle oracle"
Rscript --vanilla scripts/verify_bundle.R
echo "OK: synthetic 47-site release chain passed ($MODE)."

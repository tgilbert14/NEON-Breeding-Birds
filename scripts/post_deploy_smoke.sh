#!/usr/bin/env bash
set -euo pipefail

pages="https://tgilbert14.github.io/NEON-Breeding-Birds/"
app="https://019ee116-75d9-5940-8ccd-9b8c7afabce4.share.connect.posit.cloud/"
pages_marker="breeding-birds-poster-v1"
app_marker="breeding-birds-release-2026-v1"
stamp_path="data/release_stamp.json"

if [[ ! -s "$stamp_path" ]]; then
  echo "Missing committed release stamp: $stamp_path" >&2
  exit 1
fi
expected_release_id=$(python3 -c '
import hashlib, json, re, sys
with open(sys.argv[1], encoding="utf-8") as handle:
    stamp = json.load(handle)
expected_fields = [
    "schema_version", "app_id", "product", "release", "doi",
    "source_receipt_sha256", "environment_receipt_sha256",
    "payload_sha256", "manifest_contract_sha256", "release_id",
]
if list(stamp) != expected_fields or stamp.get("schema_version") != 3:
    raise SystemExit("invalid schema-v3 release stamp")
if (stamp.get("app_id") != "NEON-Breeding-Birds" or
        stamp.get("product") != "DP1.10003.001" or
        stamp.get("release") != "RELEASE-2026" or
        stamp.get("doi") != "10.48443/v6hs-mx57"):
    raise SystemExit("invalid release contract")
for field in ("source_receipt_sha256", "environment_receipt_sha256", "payload_sha256",
              "manifest_contract_sha256"):
    if not re.fullmatch(r"[0-9a-f]{64}", stamp.get(field, "")):
        raise SystemExit(f"invalid {field}")
material = "\n".join([
    "neon-breeding-birds-release-instance-v3",
    stamp["app_id"], stamp["product"], stamp["release"], stamp["doi"],
    stamp["source_receipt_sha256"], stamp["environment_receipt_sha256"],
    stamp["payload_sha256"], stamp["manifest_contract_sha256"],
])
release_id = stamp.get("release_id", "")
expected_id = "sha256:" + hashlib.sha256(material.encode("utf-8")).hexdigest()
if release_id != expected_id:
    raise SystemExit("committed release_id does not re-derive from schema v3")
print(release_id)
' "$stamp_path")
expected_pages_release_sha256=$(sha256sum "$stamp_path" | awk '{print $1}')

deadline=$((SECONDS + 900))
attempt=0
while (( SECONDS < deadline )); do
  attempt=$((attempt + 1))
  revision="${GITHUB_SHA:-manual}-$attempt"
  pages_body=$(curl --fail --silent --show-error --location --max-time 15 \
    --header "Cache-Control: no-cache" "${pages}?verify=${revision}" || true)
  pages_release_sha256=$(curl --fail --silent --show-error --location --max-time 15 \
    --header "Cache-Control: no-cache" "${pages}release.json?verify=${revision}" | \
    sha256sum | awk '{print $1}' || true)
  app_body=$(curl --fail --silent --show-error --location --max-time 30 \
    --header "Cache-Control: no-cache" "${app}?verify=${revision}" || true)

  pages_ready=false
  pages_release_ready=false
  app_ready=false
  if grep -Fq "$pages_marker" <<<"$pages_body" &&
     grep -Fq "NEON Breeding Bird Explorer" <<<"$pages_body"; then
    pages_ready=true
  fi
  if [[ "$pages_release_sha256" == "$expected_pages_release_sha256" ]]; then
    pages_release_ready=true
  fi
  if grep -Fq "$app_marker" <<<"$app_body" &&
     grep -Fq "DP1.10003.001" <<<"$app_body" &&
     grep -Fq 'name="ddl-release-instance"' <<<"$app_body" &&
     grep -Fq "$expected_release_id" <<<"$app_body" &&
     ! grep -Eiq "startup error|application failed to start|service unavailable" <<<"$app_body"; then
    app_ready=true
  fi

  if [[ "$pages_ready" == true && "$pages_release_ready" == true && "$app_ready" == true ]]; then
    echo "OK: Pages and Connect serve exact validated release instance $expected_release_id."
    exit 0
  fi
  echo "Attempt $attempt: waiting for exact Pages and Connect revision (pages=$pages_ready pages_release=$pages_release_ready app=$app_ready)..."
  sleep 15
done

echo "Production did not expose the exact validated Pages/Connect release instance in 15 minutes." >&2
exit 1

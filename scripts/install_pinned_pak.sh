#!/usr/bin/env bash
# Install the reviewed pak bootstrap before setup-r-dependencies executes.
# The action is configured with pak-version: none and must reuse this library.
set -euo pipefail

: "${RUNNER_TEMP:?RUNNER_TEMP is required}"
: "${GITHUB_ENV:?GITHUB_ENV is required}"
: "${R_LIBS_USER:?setup-r must set R_LIBS_USER before the pak bootstrap}"

pak_version="0.11.1"
pak_sha256="c074353d1341a21d9eb713f8dd0f067300a19cd32953e3e0c036f32617231e37"
pak_file="pak_${pak_version}_R-4-5_x86_64-linux-gnu.tar.gz"
pak_url="https://r-lib.github.io/p/pak/stable/linux-gnu/x86_64/${pak_file}"
pak_archive="${RUNNER_TEMP}/${pak_file}"
pak_library="${R_LIBS_USER}"

if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
  echo "Pinned pak bootstrap supports only the reviewed Linux x86_64 runner." >&2
  exit 1
fi

if [[ "$pak_library" != /* || "$pak_library" == *:* ]]; then
  echo "R_LIBS_USER must resolve to one absolute reviewed library path." >&2
  exit 1
fi

mkdir -p "$pak_library"
curl --proto '=https' --proto-redir '=https' --tlsv1.2 \
  --fail --show-error --silent --location \
  --retry 5 --retry-all-errors "$pak_url" --output "$pak_archive"
printf '%s  %s\n' "$pak_sha256" "$pak_archive" | sha256sum --check --strict

R_LIB_FOR_PAK="$pak_library" PAK_ARCHIVE="$pak_archive" \
  Rscript --vanilla -e '
    stopifnot(
      identical(as.character(getRversion()), "4.5.2"),
      identical(R.Version()$arch, "x86_64"),
      identical(R.Version()$os, "linux-gnu")
    )
    install.packages(
      Sys.getenv("PAK_ARCHIVE"), repos = NULL,
      lib = Sys.getenv("R_LIB_FOR_PAK"), quiet = TRUE
    )
  '

R_LIB_FOR_PAK="$pak_library" Rscript --vanilla -e '
  loadNamespace("pak", lib.loc = Sys.getenv("R_LIB_FOR_PAK"))
  library_path <- normalizePath(Sys.getenv("R_LIB_FOR_PAK"), mustWork = TRUE)
  stopifnot(library_path %in% normalizePath(.libPaths(), mustWork = TRUE))
  actual <- as.character(packageVersion("pak", lib.loc = Sys.getenv("R_LIB_FOR_PAK")))
  actual_path <- normalizePath(find.package("pak"), mustWork = TRUE)
  expected_path <- normalizePath(file.path(library_path, "pak"), mustWork = TRUE)
  stopifnot(identical(actual, "0.11.1"), identical(actual_path, expected_path))
  cat("OK: checksum-pinned pak", actual, "is ready.\n")
'
printf 'R_LIB_FOR_PAK=%s\n' "$pak_library" >> "$GITHUB_ENV"

# ===========================================================================
# One-time migration: recode the observerDistance sentinel (999 / 9999) -> NA
# in the already-built per-site bundles, so we don't have to re-fetch the raw
# NEON download. The canonical build (scripts/bundle_bird_data.R, na_sentinel())
# now does this at build time; this script brings the SHIPPED bundles in line.
#
# 999 = NEON "distance not estimable" placeholder (flocks / flyovers / far), NOT a
# measurement. It was (a) mislabelling 602 visual records as long-range visual IDs
# in bird_qc()'s "visualfar" flag, (b) printing as a literal "999 m" in the profile
# table + per-species CSV, and (c) polluting raw distance summaries. observerDistance
# feeds NO abundance/richness/cascade metric, so this shifts zero headline numbers.
#
# Run with R-4.5.2 via PowerShell (NOT git-bash — git-bash segfaults reading these).
# Atomic per-file replace (tempfile + file.rename) so a concurrent reader is safe.
# ===========================================================================
dir  <- "C:/Users/tsgil/OneDrive/Documents/VGS - R/NEON-Breeding-Birds"
SENT <- c(999, 9999)
files <- c(list.files(file.path(dir, "data/sites"), pattern = "\\.rds$", full.names = TRUE),
           list.files(file.path(dir, "data-sample"), pattern = "\\.rds$", full.names = TRUE))

total <- 0L; touched <- 0L
for (f in files) {
  b <- readRDS(f)
  if (!is.list(b) || is.null(b$obs) || !"observerDistance" %in% names(b$obs)) next
  od <- suppressWarnings(as.numeric(b$obs$observerDistance))
  hit <- !is.na(od) & od %in% SENT
  n <- sum(hit)
  if (n == 0L) next
  b$obs$observerDistance <- ifelse(hit, NA_real_, od)
  tmp <- paste0(f, ".tmp")
  saveRDS(b, tmp, compress = "xz")
  file.rename(tmp, f)            # atomic replace on the same volume
  total <- total + n; touched <- touched + 1L
  cat(sprintf("  %-28s recoded %4d -> NA\n", basename(f), n))
}
cat(sprintf("\nDONE: recoded %d sentinel detections across %d bundle(s).\n", total, touched))

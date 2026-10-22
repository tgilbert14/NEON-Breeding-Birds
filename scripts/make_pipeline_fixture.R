# Synthetic 47-site raw fixture for the release chain (producer, sanitizer, raw
# oracle, derived builders, stamp, bundle oracle). Every site gets a small valid
# baseline; PUUM, SRER and GUAN carry the edge cases: missing, invalid and text
# clusterSize tokens, flyovers and coarse IDs without a count, minute 88, blank
# and out-of-window minutes, unknown methods, unsafe names, orphans, impractical
# visits, and a pre-window-only missing count. Run by scripts/test_pipeline_fixture.sh;
# it never touches committed data.
# Usage: Rscript --vanilla scripts/make_pipeline_fixture.R <outdir> <integer|character|double>
suppressPackageStartupMessages({ library(jsonlite); library(digest) })
source("R/site_metadata.R")
source("R/bird_evidence_contract.R")
args <- commandArgs(trailingOnly = TRUE)
OUT <- args[[1]]
MODE <- if (length(args) >= 2) args[[2]] else "double"
dir.create(file.path(OUT, "raw"), recursive = TRUE, showWarnings = FALSE)
sites <- sort(as.character(neon_sites$site))

visit_row <- function(site, plot, point, year, bout, state = "OK", observer = "obs-a", month = "06") {
  data.frame(
    siteID = site, plotID = plot, pointID = point,
    eventID = paste(site, plot, year, bout, sep = "."),
    boutNumber = as.integer(bout),
    startDate = sprintf("%d-%s-1%dT10:00:00Z", year, month, as.integer(bout)),
    samplingImpractical = state, release = "RELEASE-2026",
    measuredBy = observer,
    decimalLatitude = 19.5, decimalLongitude = -155.3,
    endCloudCoverPercentage = 10, kmPerHourObservedWindSpeed = 2,
    nlcdClass = "evergreenForest", observedAirTemp = 15,
    observedHabitat = "forest", samplingProtocolVersion = "NEON.DOC.014041vK",
    startCloudCoverPercentage = 10, stringsAsFactors = FALSE)
}
det_row <- function(v, sci, rank = "species", cluster = 1, method = "singing",
                    minute = "2", dist = "20", taxon = "TAXA", vern = "Bird") {
  data.frame(
    siteID = v$siteID, plotID = v$plotID, pointID = v$pointID, eventID = v$eventID,
    boutNumber = v$boutNumber, startDate = v$startDate,
    taxonRank = rank, scientificName = sci, clusterSize = cluster,
    detectionMethod = method, observerDistance = dist, pointCountMinute = minute,
    release = "RELEASE-2026", sexOrAge = "Male", taxonID = taxon,
    vernacularName = vern, stringsAsFactors = FALSE)
}

records <- list()
for (site in sites) {
  pp <- list(); cd <- list()
  # Baseline: three plots x two years x two bouts, all valid with one eligible row.
  for (plot in sprintf("%s_00%d", site, 1:3)) for (year in c(2018L, 2022L)) for (bout in 1:2) {
    v <- visit_row(site, plot, "21", year, bout)
    pp[[length(pp) + 1]] <- v
    cd[[length(cd) + 1]] <- det_row(v, "Turdus migratorius", cluster = 1 + bout)
  }
  if (site == "PUUM") {
    # V1 stays valid: eligible + cluster-blocked row on the same count.
    v1 <- visit_row(site, "PUUM_010", "21", 2019L, 1); pp[[length(pp) + 1]] <- v1
    cd[[length(cd) + 1]] <- det_row(v1, "Chasiempis sandwichensis", cluster = 2)
    cd[[length(cd) + 1]] <- det_row(v1, "Himatione sanguinea", cluster = NA)
    cd[[length(cd) + 1]] <- det_row(v1, "Turdus migratorius", cluster = NA)
    cd[[length(cd) + 1]] <- det_row(v1, "Chasiempis sandwichensis", cluster = -1)
    # V2 withdrawn: blocked row plus every kind of other row.
    v2 <- visit_row(site, "PUUM_010", "21", 2019L, 2); pp[[length(pp) + 1]] <- v2
    cd[[length(cd) + 1]] <- det_row(v2, "Himatione sanguinea", cluster = NA)
    cd[[length(cd) + 1]] <- det_row(v2, "Chlorodrepanis virens", cluster = 3, method = "flyover")
    cd[[length(cd) + 1]] <- det_row(v2, "Chlorodrepanis virens", cluster = NA, method = "flyover")
    cd[[length(cd) + 1]] <- det_row(v2, "Zosterops sp.", rank = "genus", cluster = 1)
    cd[[length(cd) + 1]] <- det_row(v2, "Zosterops japonicus", cluster = 1, minute = "88")
    cd[[length(cd) + 1]] <- det_row(v2, "Zosterops japonicus", cluster = 1, minute = "")
    cd[[length(cd) + 1]] <- det_row(v2, "Zosterops japonicus", cluster = 1, minute = "7")
    cd[[length(cd) + 1]] <- det_row(v2, "Zosterops japonicus", cluster = 1, method = "unknown")
    cd[[length(cd) + 1]] <- det_row(v2, "zosterops japonicus", cluster = 1)
    cd[[length(cd) + 1]] <- det_row(v2, "", cluster = 1)
    cd[[length(cd) + 1]] <- det_row(v2, "Zosterops japonicus", rank = "subspecies", cluster = NA)
    # V3 coarse-only blocked rows: not withdrawn (supported zero).
    v3 <- visit_row(site, "PUUM_011", "21", 2019L, 1); pp[[length(pp) + 1]] <- v3
    cd[[length(cd) + 1]] <- det_row(v3, "Zosterops sp.", rank = "genus", cluster = NA)
    # V4 flyover-only blocked rows: not withdrawn.
    v4 <- visit_row(site, "PUUM_011", "21", 2019L, 2); pp[[length(pp) + 1]] <- v4
    cd[[length(cd) + 1]] <- det_row(v4, "Chlorodrepanis virens", cluster = NA, method = "flyover")
    # V5 minute-88 blocked only: not withdrawn.
    v5 <- visit_row(site, "PUUM_012", "21", 2019L, 1); pp[[length(pp) + 1]] <- v5
    cd[[length(cd) + 1]] <- det_row(v5, "Himatione sanguinea", cluster = NA, minute = "88")
    # V6 impractical with a clean row.
    v6 <- visit_row(site, "PUUM_012", "21", 2019L, 2, state = "High wind"); pp[[length(pp) + 1]] <- v6
    cd[[length(cd) + 1]] <- det_row(v6, "Himatione sanguinea", cluster = 1)
    cd[[length(cd) + 1]] <- det_row(v6, "Himatione sanguinea", cluster = NA)
    # V7 unknown-method blocked only: not withdrawn.
    v7 <- visit_row(site, "PUUM_013", "21", 2019L, 1); pp[[length(pp) + 1]] <- v7
    cd[[length(cd) + 1]] <- det_row(v7, "Himatione sanguinea", cluster = NA, method = "unknown")
    # V8 empty valid count.
    v8 <- visit_row(site, "PUUM_013", "21", 2019L, 2); pp[[length(pp) + 1]] <- v8
    # V9 zero and fractional cluster: withdrawn; point PUUM_014_21 has no valid count left.
    v9 <- visit_row(site, "PUUM_014", "21", 2019L, 1, observer = "obs-z"); pp[[length(pp) + 1]] <- v9
    cd[[length(cd) + 1]] <- det_row(v9, "Himatione sanguinea", cluster = 0)
    cd[[length(cd) + 1]] <- det_row(v9, "Chasiempis sandwichensis", cluster = 1.5, method = "calling, visual")
    # V10 impractical-only point.
    v10 <- visit_row(site, "PUUM_015", "21", 2019L, 1, state = "Rain"); pp[[length(pp) + 1]] <- v10
    # Orphan detection.
    orphan <- visit_row(site, "PUUM_099", "21", 2019L, 1)
    cd[[length(cd) + 1]] <- det_row(orphan, "Himatione sanguinea", cluster = NA)
    # Point-year with valid bout 1 and withdrawn bout 2 already covered by V1/V2.
  }
  if (site == "SRER") {
    # Pre-window-only missing count: lifetime index NA, 2017-2024 window finite.
    v <- visit_row(site, "SRER_050", "21", 2016L, 1); pp[[length(pp) + 1]] <- v
    cd[[length(cd) + 1]] <- det_row(v, "Turdus migratorius", cluster = NA)
    cd[[length(cd) + 1]] <- det_row(v, "Corvus corax", cluster = 2)
  }
  if (site == "GUAN") {
    # Every incidence detection lacks a count (all species and the window NA).
    for (k in seq_along(cd)) cd[[k]]$clusterSize <- NA
  }
  pp <- do.call(rbind, pp); cd <- do.call(rbind, cd)
  if (MODE == "integer") cd$clusterSize <- as.integer(cd$clusterSize)
  if (MODE == "character") {
    cd$clusterSize <- ifelse(is.na(cd$clusterSize), "", as.character(cd$clusterSize))
    if (site == "PUUM") {
      base <- cd[cd$eventID == "PUUM.PUUM_010.2019.1" & cd$scientificName == "Chasiempis sandwichensis", , drop = FALSE][1, ]
      extra <- base[rep(1, 4), ]; extra$clusterSize <- c("abc", " 4 ", "2.0", "1e0"); extra$scientificName <- "Leiothrix lutea"
      cd <- rbind(cd, extra)
    }
  }
  raw <- list(brd_perpoint = pp, brd_countdata = cd,
              brd_personnel = data.frame(personnel = "x", stringsAsFactors = FALSE))
  raw <- bird_canonical_source(raw)
  path <- file.path(OUT, "raw", paste0(site, "_raw.rds"))
  saveRDS(raw, path, compress = "xz", version = 3)
  records[[site]] <- list(site = site, file = basename(path),
    full_source_content_sha256 = bird_full_source_sha256(raw),
    evidence_projection_sha256 = bird_evidence_projection_sha256(raw),
    brd_perpoint_rows = nrow(raw$brd_perpoint), brd_countdata_rows = nrow(raw$brd_countdata))
}
receipt <- list(
  schema_version = 3L, product = "DP1.10003.001", release = "RELEASE-2026",
  doi = "10.48443/v6hs-mx57", release_generated_utc = "2026-01-23T00:07:49Z",
  retrieval = list(package = "basic", tool = "neonUtilities::loadByProduct",
    neonUtilities_version = "4.0.2", r_version = "4.5.2", token_required = TRUE,
    staging_was_empty = TRUE),
  evidence_projection = list(schema_version = BIRD_EVIDENCE_SCHEMA_VERSION,
    tables = BIRD_EVIDENCE_TABLE_COLUMNS,
    privacy = "no observer identity, personnel table, free-text sampling remarks, or source UIDs"),
  expected_site_count = 47L, expected_sites = sites,
  fetched_site_count = 47L, fetched_sites = sites, files = unname(records))
jsonlite::write_json(receipt, file.path(OUT, "receipt.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
cat("fixture written to", OUT, "mode", MODE, "\n")

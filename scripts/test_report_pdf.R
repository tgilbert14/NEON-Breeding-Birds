#!/usr/bin/env Rscript
# Server-side PDF site report gate: render the two-page report for bundled sites
# and check each file is a non-empty PDF with exactly two pages whose body stays
# inside the page. PUUM exercises wren's read-as-one caveat on real data.
# Run from the repository root: Rscript --vanilla scripts/test_report_pdf.R

Sys.setenv(BRD_LIVE = "0")
suppressPackageStartupMessages(source("global.R"))

# Count "/Type /Page" objects (not the "/Type /Pages" tree node). cairo writes
# its page dictionaries inside Flate-compressed object streams, so every stream
# is inflated with base memDecompress() and searched along with the plain bytes.
pdf_page_count <- function(path) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  as_text <- function(r) rawToChar(r[r != as.raw(0)])
  plain <- as_text(bytes)
  # Locate streams on the raw bytes: offsets in `plain` drift once NULs drop.
  find_all <- function(pat) {
    p <- charToRaw(pat); n <- length(p); hits <- integer()
    cand <- which(bytes == p[1])
    for (i in cand) if (i + n - 1L <= length(bytes) && all(bytes[i:(i + n - 1L)] == p)) hits <- c(hits, i)
    hits
  }
  s_hits <- find_all("stream"); e_hits <- find_all("endstream")
  s_hits <- setdiff(s_hits, e_hits + 3L)          # "endstream" contains "stream"
  inflated <- character()
  for (s in s_hits) {
    from <- s + 6L
    if (from <= length(bytes) && bytes[from] == as.raw(0x0d)) from <- from + 1L
    if (from <= length(bytes) && bytes[from] == as.raw(0x0a)) from <- from + 1L
    e <- e_hits[e_hits > from][1]; if (is.na(e)) next
    chunk <- bytes[from:(e - 1L)]
    out <- tryCatch(memDecompress(chunk, type = "gzip"), error = function(err) NULL)
    if (!is.null(out)) inflated <- c(inflated, as_text(out))
  }
  txt <- paste(c(plain, inflated), collapse = "\n")
  hits <- gregexpr("/Type\\s*/Page(?![s])", txt, perl = TRUE, useBytes = TRUE)[[1]]
  sum(hits > 0)
}

render_site <- function(site, sb_override = NULL) {
  b <- load_site_bundle(site)
  if (is.null(b)) stop("bundle failed to load: ", site, call. = FALSE)
  p <- prepare_site_bundle(b)
  sb <- p$sb
  if (!is.null(sb_override)) sb[names(sb_override)] <- sb_override
  f <- tempfile(sprintf("bird-report-%s-", site), fileext = ".pdf")
  res <- render_bird_report_pdf(f, site, neon_sites$name[neon_sites$site == site],
                                b$obs, p$held, b$visits, b$opportunity, b$points, p$board, sb)
  sz <- file.info(f)$size
  stopifnot(file.exists(f), is.finite(sz), sz > 2000)
  head <- rawToChar(readBin(f, "raw", 5L))
  if (!identical(head, "%PDF-")) stop(site, ": not a PDF", call. = FALSE)
  np <- pdf_page_count(f)
  if (np != 2L) stop(sprintf("%s: expected 2 pages, found %d", site, np), call. = FALSE)
  over <- res$bottoms > res$limits
  if (any(over)) stop(sprintf("%s: body runs past the footer on page(s) %s (%s vs %s)", site,
    paste(which(over), collapse = ","), paste(round(res$bottoms, 2), collapse = "/"),
    paste(round(res$limits, 2), collapse = "/")), call. = FALSE)
  cat(sprintf("  %s%s: %s bytes, %d pages, body bottoms %s of %s in\n", site,
              if (is.null(sb_override)) "" else " (variant)", format(sz, big.mark = ","), np,
              paste(sprintf("%.2f", res$bottoms), collapse = "/"),
              paste(sprintf("%.2f", res$limits), collapse = "/")))
  invisible(f)
}

t0 <- Sys.time()
files <- list()
for (site in c("SRER", "JORN", "YELL", "TOOL", "PUUM")) files[[site]] <- render_site(site)

# The unavailable-index and read-as-one branches do not occur in the loadable
# committed bundles, so exercise them on JORN's real data with the summary
# fields site_birds() would report.
f_unavail <- render_site("JORN", list(n_detections_without_count = 12L, birds_per_count = NA_real_,
                                      index_birds = NA_real_))
f_read1 <- render_site("JORN", list(n_detections_count_read_as_one = 340L,
                                    read_as_one_years = c(2017L, 2018L)))
# Read-as-one counts beside detections with no count at all: the total is NA,
# and the caveat must say so without printing NA.
f_both <- render_site("JORN", list(n_detections_count_read_as_one = 40L, read_as_one_years = 2018L,
                                   n_detections_without_count = 12L, birds_per_count = NA_real_,
                                   index_birds = NA_real_))

# Through the real Shiny download handler: load JORN, then an all-zero record
# built from it (supported counts, no eligible detection), as
# scripts/test_server_runtime.R does.
suppressPackageStartupMessages(library(shiny))
app_server <- source("server.R")$value
f_zero <- NULL
shiny::testServer(app_server, {
  jorn <- load_site_bundle("JORN")
  stopifnot(isTRUE(ingest(jorn, "JORN runtime fixture", cache_key = "JORN")))
  session$flushReact()
  f_handler <- output$reportPdf
  stopifnot(is.character(f_handler), file.exists(f_handler), pdf_page_count(f_handler) == 2L)
  cat("  JORN via output$reportPdf: 2 pages\n")

  zero <- jorn
  zero$obs <- jorn$obs[0, , drop = FALSE]
  for (tbl in c("opportunity", "visits")) {
    z <- zero[[tbl]]
    z$eligible_detection_rows <- 0L; z$eligible_birds <- 0; z$eligible_species <- 0L
    z$flyover_rows <- 0L; z$flyover_birds <- 0
    zero[[tbl]] <- z
  }
  zero$opportunity$outcome <- ifelse(zero$opportunity$supported, "supported_zero", "unavailable")
  zero$visits$support_state <- ifelse(zero$visits$valid_count, "supported", "held")
  zero$visits$outcome <- ifelse(zero$visits$valid_count, "supported_zero", "unavailable")
  zero$meta$n_supported_zero_counts <- sum(zero$visits$valid_count)
  zero$meta$n_supported_zero <- sum(zero$opportunity$supported)
  zero$meta$observer_support$by_species <- data.frame(
    scientificName = character(), n_observers = integer(), stringsAsFactors = FALSE)
  stopifnot(isTRUE(ingest(zero, "JORN all-zero fixture")), nrow(rv$board) == 0L)
  session$flushReact()
  # The download lives in a temp file Shiny removes when testServer() exits,
  # so keep a copy for the text checks below.
  f_zero <<- tempfile("bird-report-zero-", fileext = ".pdf")
  stopifnot(file.copy(output$reportPdf, f_zero), pdf_page_count(f_zero) == 2L)
  cat("  all-zero record via output$reportPdf: 2 pages\n")
})

# Text checks where pdftotext is available (local runs; optional in CI).
pdftotext <- Sys.which("pdftotext")
if (nzchar(pdftotext)) {
  txt_of <- function(f) paste(system2(pdftotext, c("-layout", shQuote(f), "-"), stdout = TRUE), collapse = "\n")
  t_srer <- txt_of(files$SRER)
  for (needle in c("SRER", "DP1.10003.001", "RELEASE-2026", "10.48443/v6hs-mx57",
                   "Top 15 species by detection frequency", "verify, not wrong",
                   "Held records", "Distance usability", "https://doi.org/10.48443/v6hs-mx57"))
    if (!grepl(needle, t_srer, fixed = TRUE)) stop("SRER report text lacks: ", needle, call. = FALSE)
  if (grepl("\\bNA\\b", t_srer)) stop("SRER report prints a literal NA", call. = FALSE)
  if (grepl("—", t_srer)) stop("SRER report contains an em dash", call. = FALSE)
  t_un <- txt_of(f_unavail)
  if (!grepl("unavailable here", t_un, fixed = TRUE)) stop("unavailable index text missing", call. = FALSE)
  t_r1 <- txt_of(f_read1)
  if (!grepl("left single birds blank", gsub("\\s+", " ", t_r1), fixed = TRUE))
    stop("read-as-one caveat missing", call. = FALSE)
  for (tt in list(t_un, t_r1, txt_of(f_both)))
    if (grepl("\\bNA\\b", tt)) stop("a report variant prints a literal NA", call. = FALSE)
  t_puum <- gsub("\\s+", " ", txt_of(files$PUUM))
  if (!grepl("left single birds blank", t_puum, fixed = TRUE) || grepl("\\bNA\\b", t_puum))
    stop("PUUM report lacks the read-as-one caveat or prints NA", call. = FALSE)
  t_zero <- txt_of(f_zero)
  if (!grepl("supported zero record", t_zero, fixed = TRUE) || grepl("\\bNA\\b", t_zero))
    stop("all-zero report text is wrong", call. = FALSE)
  cat("  pdftotext text checks passed\n")
} else cat("  pdftotext not found; text checks skipped\n")

cat(sprintf("Rendered %d reports in %.1f s\n", length(files) + 3L,
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat("OK\n")

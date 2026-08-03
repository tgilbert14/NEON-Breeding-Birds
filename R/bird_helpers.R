# ===========================================================================
# NEON Breeding Bird Explorer — bird_helpers.R
# Point-count / avian-survey analyses on DP1.10003.001. Community detections are
# interpreted against the complete brd_perpoint visit/opportunity ledger. The
# honesty backbone:
# raw point-count totals are detection-confounded (a loud flycatcher and a quiet
# sparrow at equal density give unequal counts), so the abundance axis is a
# DETECTION INDEX (birds per point-count), never "population". observerDistance
# powers the per-species detection-decay panel. See docs/neonize-playbook.md.
# ===========================================================================
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
mode_chr <- function(x){ x<-x[!is.na(x)]; if(!length(x)) return(NA_character_); names(sort(table(x),decreasing=TRUE))[1] }
short_point <- function(p) sub("^[A-Z]{4}_", "", as.character(p))

bird_community_vernacular <- function(community, reported_scientific,
                                      reported_rank, vernacular) {
  community <- as.character(community)[1]
  reported_scientific <- gsub("[[:space:]]+", " ", trimws(as.character(reported_scientific)))
  reported_rank <- tolower(trimws(as.character(reported_rank)))
  vernacular <- trimws(as.character(vernacular))
  available <- !is.na(vernacular) & nzchar(vernacular)
  parent <- available & reported_rank == "species" &
    !is.na(reported_scientific) & reported_scientific == community
  if (any(parent)) mode_chr(vernacular[parent]) else mode_chr(vernacular[available])
}

# One community unit means one biological species, even when NEON reports a
# mixture of the parent binomial and one or more subspecies.  The source values
# remain in the reported* provenance columns; every ecological metric uses the
# normalized binomial returned here.  Ambiguous nomenclatural forms fail closed.
BIRD_COMMUNITY_UNIT_STATES <- c(
  "canonical_species", "canonical_subspecies", "not_species_level",
  "missing_scientific_name", "unsafe_scientific_name")

bird_community_unit <- function(scientific, rank) {
  scientific <- as.character(scientific)
  rank <- as.character(rank)
  n <- max(length(scientific), length(rank))
  if (!length(scientific)) scientific <- rep(NA_character_, n)
  if (!length(rank)) rank <- rep(NA_character_, n)
  if (length(scientific) != n) scientific <- rep(scientific, length.out = n)
  if (length(rank) != n) rank <- rep(rank, length.out = n)

  normalized <- gsub("[[:space:]]+", " ", trimws(scientific))
  normalized[is.na(scientific) | !nzchar(normalized)] <- NA_character_
  normalized_rank <- tolower(trimws(rank))
  at_supported_rank <- !is.na(normalized_rank) &
    normalized_rank %in% c("species", "subspecies")
  state <- rep("not_species_level", n)
  state[at_supported_rank & is.na(normalized)] <- "missing_scientific_name"
  community <- rep(NA_character_, n)

  candidate <- which(at_supported_rank & !is.na(normalized))
  for (i in candidate) {
    tokens <- strsplit(normalized[[i]], " ", fixed = TRUE)[[1]]
    token_ok <- length(tokens) >= 2L && all(grepl("^[[:alpha:]][[:alpha:]'-]*$", tokens))
    rank_ok <- if (identical(normalized_rank[[i]], "species")) length(tokens) == 2L else
      length(tokens) %in% 2:3
    epithet_ok <- token_ok && grepl("^[[:lower:]]", tokens[[2]]) &&
      !tolower(tokens[[2]]) %in% c("sp", "spp", "cf", "aff", "nr")
    genus_ok <- token_ok && grepl("^[[:upper:]]", tokens[[1]])
    if (!token_ok || !rank_ok || !genus_ok || !epithet_ok) {
      state[[i]] <- "unsafe_scientific_name"
      next
    }
    genus <- paste0(toupper(substr(tokens[[1]], 1L, 1L)),
                    tolower(substr(tokens[[1]], 2L, nchar(tokens[[1]]))))
    community[[i]] <- paste(genus, tolower(tokens[[2]]))
    state[[i]] <- if (identical(normalized_rank[[i]], "subspecies"))
      "canonical_subspecies" else "canonical_species"
  }
  data.frame(
    communityScientificName = community,
    community_unit_state = state,
    is_species = state %in% c("canonical_species", "canonical_subspecies"),
    stringsAsFactors = FALSE)
}

species_level_only <- function(d){
  if (is.null(d) || !nrow(d)) return(d)
  prepared <- bird_prepare_obs(d)
  prepared[prepared$is_species %in% TRUE, , drop = FALSE]
}
# stable species->color by primary detection method (no family in the basic package).
# LUMINANCE-SEPARATED so the three core methods stay distinct under colour-vision
# deficiency: singing L=.19 (green) / visual L=.22 (rust) / calling L=.25 (blue),
# min adjacent gap .030. The old set put singing/visual within .016 L (a CVD wash).
# Hues stay on the Field Guide theme; the three are also distinguished by hue family.
METHOD_COLS <- c(singing = "#1a8a5a", calling = "#3a8fd6", visual = "#D55E00",
                 drumming = "#7a4a2a", "non-vocal" = "#7a4a2a", other = "#9aa6b2", unknown = "#9aa6b2")
# Map compound NEON methods to their dominant breeding-signal component before colouring,
# so "calling and singing"/"visual and singing" retain singing as their compact
# display channel instead of dumping into grey "other". This does not establish
# territory or breeding status.
canon_method <- function(m) {
  m <- tolower(trimws(as.character(m)))
  ifelse(is.na(m) | m == "", "unknown",
  ifelse(grepl("singing", m), "singing",
  ifelse(grepl("drumming", m), "drumming",
  ifelse(grepl("calling", m), "calling",
  ifelse(grepl("visual",  m), "visual",
  ifelse(grepl("flyover", m), "flyover", m))))))
}
method_col <- function(m) { m2 <- canon_method(m); m2 <- ifelse(m2 %in% names(METHOD_COLS), m2, "other"); unname(METHOD_COLS[m2]) }
# Redundant NON-COLOUR channel for detection method: a per-method plotly marker
# SYMBOL so the Bird Board is readable without relying on hue (the colours alone
# are deuteranope-confusable). Paired with METHOD_COLS so the legend swatch shows
# both. Glyphs (●▲■◆) below mirror these for any text/legend key.
METHOD_SYM <- c(singing = "circle", calling = "triangle-up", visual = "square",
                drumming = "diamond", "non-vocal" = "diamond", flyover = "x",
                other = "circle-open", unknown = "circle-open")
method_sym <- function(m) { m2 <- canon_method(m); m2 <- ifelse(m2 %in% names(METHOD_SYM), m2, "other"); unname(METHOD_SYM[m2]) }
# Unicode glyph for legend/text use, paired 1:1 with the plotly marker symbol above.
METHOD_GLYPH <- c(singing = "●", calling = "▲", visual = "■",
                  drumming = "◆", "non-vocal" = "◆", flyover = "✕",
                  other = "○", unknown = "○")
method_glyph <- function(m) { m2 <- canon_method(m); m2 <- ifelse(m2 %in% names(METHOD_GLYPH), m2, "other"); unname(METHOD_GLYPH[m2]) }
# Flyovers are records explicitly marked as birds passing overhead. Exclude them
# operationally from the on-point community detection index so large passing flocks
# do not distort it. Kept in the
# raw data and audit surfaces, but excluded consistently from every breeding-
# community numerator, richness/incidence surface, map, and search rank.
is_flyover <- function(x) {
  out <- grepl("flyover", tolower(as.character(x)), fixed = FALSE)
  out[is.na(out)] <- FALSE
  out
}

# Preserve compound detection methods instead of forcing each record into one
# mutually exclusive bucket. `methodCanonical` remains useful for a compact
# legend, while the component flags retain every recorded channel for exports and
# audit. Flyover is an outcome/quarantine flag, not a breeding-method category.
method_components <- function(x) {
  raw <- tolower(trimws(as.character(x)))
  missing <- is.na(x) | !nzchar(raw) | raw == "unknown"
  data.frame(
    methodCanonical = canon_method(x),
    method_singing = !missing & grepl("singing", raw, fixed = TRUE),
    method_calling = !missing & grepl("calling", raw, fixed = TRUE),
    method_visual = !missing & grepl("visual", raw, fixed = TRUE),
    method_drumming = !missing & grepl("drumming", raw, fixed = TRUE),
    method_other = !missing & !grepl("singing|calling|visual|drumming|flyover", raw),
    method_unknown = missing,
    stringsAsFactors = FALSE)
}

BIRD_DISTANCE_STATES <- c("observed", "sentinel_not_estimable", "source_missing", "invalid")
BIRD_POINT_MINUTE_STATES <- c("standard_minute", "incidental_minute_88",
                              "source_missing", "invalid_or_unknown")
BIRD_DETECTION_HOLD_REASONS <- c(
  "orphan_detection_no_visit", "detection_on_unusable_visit",
  "incidental_outside_point_count", "missing_point_count_minute",
  "invalid_point_count_minute", "invalid_cluster_size", "missing_taxon",
  "unsafe_species_canonicalization", "missing_or_unknown_detection_method")

bird_valid_cluster_size <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  is.finite(value) & value > 0 & value == floor(value)
}

# `pointCountMinute == 88` is NEON's explicit channel for an incidental bird
# observed outside the formal six-minute point count. Preserve the source token,
# but only integer minutes 1:6 can enter a standardized breeding metric.
point_count_minute_audit <- function(raw, state = NULL, in_protocol_window = NULL) {
  raw_chr <- as.character(raw)
  blank <- is.na(raw) | !nzchar(trimws(raw_chr))
  value <- suppressWarnings(as.numeric(raw_chr))
  integer_value <- suppressWarnings(as.integer(value))
  whole <- !blank & is.finite(value) & !is.na(integer_value) & value == integer_value
  minute <- ifelse(whole, integer_value, NA_integer_)
  in_window <- whole & minute %in% 1:6
  inferred <- as.character(ifelse(
    blank, "source_missing",
    ifelse(in_window, "standard_minute",
      ifelse(whole & minute == 88L, "incidental_minute_88", "invalid_or_unknown"))))
  if (!is.null(state)) {
    supplied <- as.character(state)
    bad <- is.na(supplied) | !supplied %in% BIRD_POINT_MINUTE_STATES
    if (any(bad))
      stop(sprintf("unrecognized point_count_minute_state token(s): %s",
                   paste(sort(unique(supplied[bad])), collapse = ", ")), call. = FALSE)
    if (any(supplied != inferred))
      stop("pointCountMinuteRaw and point_count_minute_state disagree", call. = FALSE)
    inferred <- supplied
  }
  if (!is.null(in_protocol_window)) {
    supplied_window <- in_protocol_window %in% TRUE
    if (any(is.na(in_protocol_window)) || any(supplied_window != in_window))
      stop("pointCountMinuteRaw and in_protocol_window disagree", call. = FALSE)
  }
  data.frame(pointCountMinuteRaw = raw_chr, pointCountMinute = minute,
             point_count_minute_state = inferred,
             in_protocol_window = in_window, stringsAsFactors = FALSE)
}

# Keep the raw observer-distance token and its state. A 999/9999 sentinel is not a
# metre measurement, while a blank source value is not evidence that a sentinel
# occurred. Older bundles already replaced both with NA; those remain honestly
# `source_missing` because their former state cannot be reconstructed.
distance_audit <- function(raw, state = NULL) {
  raw_chr <- as.character(raw)
  value <- suppressWarnings(as.numeric(raw_chr))
  sentinel <- !is.na(value) & value %in% c(999, 9999)
  inferred <- ifelse(
    sentinel, "sentinel_not_estimable",
    ifelse(is.na(raw) | !nzchar(trimws(raw_chr)), "source_missing",
      ifelse(is.finite(value) & value >= 0, "observed", "invalid")))
  if (!is.null(state)) {
    supplied <- as.character(state)
    bad <- is.na(supplied) | !supplied %in% BIRD_DISTANCE_STATES
    if (any(bad))
      stop(sprintf("unrecognized distance_state token(s): %s",
                   paste(sort(unique(supplied[bad])), collapse = ", ")), call. = FALSE)
    mismatch <- supplied != inferred
    if (any(mismatch))
      stop("observerDistanceRaw and distance_state disagree", call. = FALSE)
    inferred <- supplied
  }
  # The state and numeric value must agree; never let a sentinel/invalid token leak
  # into a numeric distance calculation.
  inferred[sentinel] <- "sentinel_not_estimable"
  metres <- ifelse(inferred == "observed" & is.finite(value) & value >= 0,
                   value, NA_real_)
  data.frame(observerDistanceRaw = raw_chr, observerDistance = metres,
             distance_state = inferred, stringsAsFactors = FALSE)
}

bird_prepare_obs <- function(obs) {
  if (is.null(obs)) return(obs)
  if (!is.data.frame(obs)) stop("bird observations must be a data frame", call. = FALSE)
  out <- obs
  required <- c("pointkey", "year", "scientificName", "clusterSize", "detectionMethod",
                "pointCountMinute")
  miss <- setdiff(required, names(out))
  if (length(miss)) stop(sprintf("bird observations lack required field(s): %s",
                                 paste(miss, collapse = ", ")), call. = FALSE)
  has_reported_scientific <- "reportedScientificName" %in% names(out)
  reported_scientific <- if (has_reported_scientific)
    as.character(out$reportedScientificName) else as.character(out$scientificName)
  reported_rank <- if ("reportedTaxonRank" %in% names(out))
    as.character(out$reportedTaxonRank) else if ("taxonRank" %in% names(out))
      as.character(out$taxonRank) else stop(
        "bird observations require reportedTaxonRank or taxonRank", call. = FALSE)
  unit <- bird_community_unit(reported_scientific, reported_rank)
  same_optional_character <- function(actual, expected) identical(
    ifelse(is.na(actual), "<NA>", as.character(actual)),
    ifelse(is.na(expected), "<NA>", as.character(expected)))
  if ("communityScientificName" %in% names(out) &&
      !same_optional_character(out$communityScientificName, unit$communityScientificName))
    stop("communityScientificName does not re-derive from reported taxonomy", call. = FALSE)
  if ("community_unit_state" %in% names(out) &&
      !same_optional_character(out$community_unit_state, unit$community_unit_state))
    stop("community_unit_state does not re-derive from reported taxonomy", call. = FALSE)
  if ("is_species" %in% names(out) &&
      !identical(out$is_species %in% TRUE, unit$is_species %in% TRUE))
    stop("is_species does not re-derive from reported taxonomy", call. = FALSE)
  if (has_reported_scientific &&
      !same_optional_character(out$scientificName, unit$communityScientificName))
    stop("scientificName is not the canonical species community unit", call. = FALSE)
  out$reportedScientificName <- reported_scientific
  out$reportedTaxonRank <- reported_rank
  out$reportedTaxonID <- if ("reportedTaxonID" %in% names(out))
    as.character(out$reportedTaxonID) else if ("taxonID" %in% names(out))
      as.character(out$taxonID) else rep(NA_character_, nrow(out))
  out$reportedVernacularName <- if ("reportedVernacularName" %in% names(out))
    as.character(out$reportedVernacularName) else if ("vernacularName" %in% names(out))
      as.character(out$vernacularName) else rep(NA_character_, nrow(out))
  out$communityScientificName <- unit$communityScientificName
  out$community_unit_state <- unit$community_unit_state
  out$is_species <- unit$is_species
  out$scientificName <- unit$communityScientificName
  out$year <- suppressWarnings(as.integer(out$year))
  out$clusterSize <- suppressWarnings(as.numeric(out$clusterSize))
  invalid_key <- is.na(out$pointkey) | !nzchar(trimws(as.character(out$pointkey))) |
    !is.finite(out$year)
  if (any(invalid_key)) stop("bird observations contain an incomplete point-year key", call. = FALSE)
  raw <- if ("observerDistanceRaw" %in% names(out)) out$observerDistanceRaw else
    if ("observerDistance" %in% names(out)) out$observerDistance else rep(NA, nrow(out))
  state <- if ("distance_state" %in% names(out)) out$distance_state else NULL
  da <- distance_audit(raw, state)
  out$observerDistanceRaw <- da$observerDistanceRaw
  out$observerDistance <- da$observerDistance
  out$distance_state <- da$distance_state
  minute_raw <- if ("pointCountMinuteRaw" %in% names(out)) out$pointCountMinuteRaw else
    out$pointCountMinute
  minute_state <- if ("point_count_minute_state" %in% names(out))
    out$point_count_minute_state else NULL
  minute_window <- if ("in_protocol_window" %in% names(out)) out$in_protocol_window else NULL
  minute_audit <- point_count_minute_audit(minute_raw, minute_state, minute_window)
  out$pointCountMinuteRaw <- minute_audit$pointCountMinuteRaw
  out$pointCountMinute <- minute_audit$pointCountMinute
  out$point_count_minute_state <- minute_audit$point_count_minute_state
  out$in_protocol_window <- minute_audit$in_protocol_window
  mc <- method_components(if ("detectionMethod" %in% names(out)) out$detectionMethod else rep(NA, nrow(out)))
  inferred_method_state <- as.character(ifelse(
    mc$method_unknown, "missing_or_unknown", "reported"))
  if ("detection_method_state" %in% names(out) &&
      !identical(as.character(out$detection_method_state), inferred_method_state))
    stop("detection_method_state does not re-derive from detectionMethod", call. = FALSE)
  for (nm in names(mc)) out[[nm]] <- mc[[nm]]
  out$detection_method_state <- inferred_method_state
  out$is_flyover <- is_flyover(if ("detectionMethod" %in% names(out)) out$detectionMethod else rep(NA, nrow(out)))
  matched <- if ("matched_visit" %in% names(out)) out$matched_visit %in% TRUE else rep(TRUE, nrow(out))
  valid <- if ("valid_count" %in% names(out)) out$valid_count %in% TRUE else rep(TRUE, nrow(out))
  out$joined_valid_visit <- matched & valid
  derived <- out$joined_valid_visit & out$in_protocol_window & out$is_species &
    !out$method_unknown & !out$is_flyover & bird_valid_cluster_size(out$clusterSize)
  if ("enters_breeding_metrics" %in% names(out) &&
      any((out$enters_breeding_metrics %in% TRUE) != derived))
    stop("enters_breeding_metrics disagrees with the shared eligible-breeding predicate", call. = FALSE)
  out$enters_breeding_metrics <- derived
  out$enters_index <- derived
  out
}

eligible_breeding_detections <- function(obs) {
  d <- bird_prepare_obs(obs)
  if (is.null(d) || !nrow(d)) return(d)
  d[d$enters_breeding_metrics %in% TRUE, , drop = FALSE]
}

BIRD_OPPORTUNITY_REQUIRED <- c("pointkey", "year", "n_valid_bouts", "outcome")
BIRD_OPPORTUNITY_SUPPORT <- c("supported", "held")
BIRD_OPPORTUNITY_OUTCOMES <- c("positive", "supported_zero", "unavailable")

bird_validate_opportunity <- function(opportunity, obs = NULL) {
  if (is.null(opportunity)) return(NULL)
  if (!is.data.frame(opportunity)) stop("bird opportunity must be a data frame", call. = FALSE)
  miss <- setdiff(BIRD_OPPORTUNITY_REQUIRED, names(opportunity))
  if (length(miss)) stop(sprintf("bird opportunity lacks required field(s): %s",
                                 paste(miss, collapse = ", ")), call. = FALSE)
  out <- opportunity
  if (!"supported" %in% names(out) && !"support_state" %in% names(out))
    stop("bird opportunity requires supported or support_state", call. = FALSE)
  if (!"supported" %in% names(out)) out$supported <- out$support_state == "supported"
  if (any(is.na(out$supported))) stop("bird opportunity has missing support state", call. = FALSE)
  out$supported <- out$supported %in% TRUE
  if ("support_state" %in% names(out)) {
    bad <- is.na(out$support_state) | !out$support_state %in% BIRD_OPPORTUNITY_SUPPORT
    if (any(bad)) stop(sprintf("unrecognized bird support_state token(s): %s",
      paste(sort(unique(out$support_state[bad])), collapse = ", ")), call. = FALSE)
    if (any((out$support_state == "supported") != out$supported))
      stop("bird opportunity supported and support_state fields disagree", call. = FALSE)
  }
  out$support_state <- ifelse(out$supported, "supported", "held")
  out$pointkey <- as.character(out$pointkey)
  out$year <- suppressWarnings(as.integer(out$year))
  n_valid_raw <- suppressWarnings(as.numeric(out$n_valid_bouts))
  out$n_valid_bouts <- suppressWarnings(as.integer(n_valid_raw))
  if (any(is.na(out$pointkey) | !nzchar(out$pointkey)) ||
      any(!is.finite(out$year)) ||
      any(!is.finite(n_valid_raw) | n_valid_raw < 0 | n_valid_raw > 2 |
          n_valid_raw != out$n_valid_bouts))
    stop("bird opportunity has an incomplete point-year key or invalid n_valid_bouts", call. = FALSE)
  bad_outcome <- is.na(out$outcome) | !out$outcome %in% BIRD_OPPORTUNITY_OUTCOMES
  if (any(bad_outcome)) stop(sprintf("unrecognized bird outcome token(s): %s",
    paste(sort(unique(out$outcome[bad_outcome])), collapse = ", ")), call. = FALSE)
  supported <- out$supported
  if (any(supported != (out$n_valid_bouts > 0L)) ||
      any(supported & !out$outcome %in% c("positive", "supported_zero")) ||
      any(!supported & out$outcome != "unavailable"))
    stop("bird opportunity support, valid-bout count, and outcome disagree", call. = FALSE)
  if ("n_held_bouts" %in% names(out)) {
    held_raw <- suppressWarnings(as.numeric(out$n_held_bouts))
    out$n_held_bouts <- suppressWarnings(as.integer(held_raw))
    if (any(!is.finite(held_raw) | held_raw < 0 | held_raw != out$n_held_bouts))
      stop("bird opportunity has invalid n_held_bouts", call. = FALSE)
    if ("n_bouts_recorded" %in% names(out)) {
      recorded <- suppressWarnings(as.numeric(out$n_bouts_recorded))
      if (any(!is.finite(recorded) | recorded < 0 | recorded != as.integer(recorded)) ||
          any(recorded != out$n_valid_bouts + out$n_held_bouts))
        stop("bird opportunity bout totals do not reconcile", call. = FALSE)
    }
  }
  key <- paste(out$pointkey, out$year, sep = "|")
  if (anyDuplicated(key)) stop("bird opportunity has duplicate pointkey x year rows", call. = FALSE)

  if (!is.null(obs)) {
    raw_obs <- bird_prepare_obs(obs)
    joined_key <- if (nrow(raw_obs)) unique(paste(
      raw_obs$pointkey[raw_obs$joined_valid_visit], raw_obs$year[raw_obs$joined_valid_visit], sep = "|")) else character()
    orphan_join <- setdiff(joined_key, key[supported])
    if (length(orphan_join))
      stop(sprintf("%d joined bird point-year(s) lack supported opportunity", length(orphan_join)), call. = FALSE)
    sp <- eligible_breeding_detections(raw_obs)
    obs_key <- if (!is.null(sp) && nrow(sp)) unique(paste(sp$pointkey, sp$year, sep = "|")) else character()
    orphan <- setdiff(obs_key, key[supported])
    if (length(orphan)) stop(sprintf("%d eligible bird point-year(s) lack supported opportunity", length(orphan)), call. = FALSE)
    expected <- ifelse(key %in% obs_key, "positive", ifelse(supported, "supported_zero", "unavailable"))
    if (any(out$outcome != expected))
      stop("bird opportunity outcome does not reconcile with eligible non-flyover detections", call. = FALSE)
    if (all(c("eligible_detection_rows", "eligible_birds", "eligible_species") %in% names(out))) {
      actual <- data.frame(key = key, rows = 0L, birds = 0, species = 0L,
                           stringsAsFactors = FALSE)
      if (!is.null(sp) && nrow(sp)) {
        skey <- paste(sp$pointkey, sp$year, sep = "|")
        by_key <- split(seq_len(nrow(sp)), skey)
        idx <- match(names(by_key), actual$key)
        actual$rows[idx] <- vapply(by_key, length, integer(1))
        actual$birds[idx] <- vapply(by_key, function(i) sum(sp$clusterSize[i]), numeric(1))
        actual$species[idx] <- vapply(by_key, function(i)
          length(unique(sp$communityScientificName[i])), integer(1))
      }
      reported_rows <- suppressWarnings(as.numeric(out$eligible_detection_rows))
      reported_birds <- suppressWarnings(as.numeric(out$eligible_birds))
      reported_species <- suppressWarnings(as.numeric(out$eligible_species))
      if (any(!is.finite(reported_rows)) || any(!is.finite(reported_birds)) ||
          any(!is.finite(reported_species)) || any(reported_rows != actual$rows) ||
          any(abs(reported_birds - actual$birds) > 1e-8) ||
          any(reported_species != actual$species))
        stop("bird opportunity eligible-detection summaries do not reconcile", call. = FALSE)
    }
    if (all(c("flyover_rows", "flyover_birds") %in% names(out))) {
      fly <- raw_obs[raw_obs$joined_valid_visit & raw_obs$in_protocol_window &
                       raw_obs$is_species & raw_obs$is_flyover &
                       bird_valid_cluster_size(raw_obs$clusterSize), , drop = FALSE]
      actual_rows <- rep(0L, nrow(out)); actual_birds <- rep(0, nrow(out))
      if (nrow(fly)) {
        fkey <- paste(fly$pointkey, fly$year, sep = "|")
        by_key <- split(seq_len(nrow(fly)), fkey)
        idx <- match(names(by_key), key)
        actual_rows[idx] <- vapply(by_key, length, integer(1))
        actual_birds[idx] <- vapply(by_key, function(i) sum(fly$clusterSize[i]), numeric(1))
      }
      reported_rows <- suppressWarnings(as.numeric(out$flyover_rows))
      reported_birds <- suppressWarnings(as.numeric(out$flyover_birds))
      if (any(!is.finite(reported_rows)) || any(!is.finite(reported_birds)) ||
          any(reported_rows != actual_rows) || any(abs(reported_birds - actual_birds) > 1e-8))
        stop("bird opportunity flyover summaries do not reconcile", call. = FALSE)
    }
  }
  out
}

BIRD_VISIT_REQUIRED <- c("survey_id", "pointkey", "year", "bout")
BIRD_VISIT_SUMMARY_FIELDS <- c(
  "eligible_detection_rows", "eligible_birds", "eligible_species",
  "flyover_rows", "flyover_birds", "support_state", "outcome")

bird_validate_visits <- function(visits, obs = NULL) {
  if (is.null(visits)) return(NULL)
  if (!is.data.frame(visits)) stop("bird visits must be a data frame", call. = FALSE)
  miss <- setdiff(BIRD_VISIT_REQUIRED, names(visits))
  if (length(miss)) stop(sprintf("bird visits lack required field(s): %s",
                                 paste(miss, collapse = ", ")), call. = FALSE)
  out <- visits
  out$survey_id <- as.character(out$survey_id)
  if (!"visit_key" %in% names(out)) out$visit_key <- out$survey_id
  if (!"valid_count" %in% names(out) && !"support_state" %in% names(out))
    stop("bird visits require valid_count or support_state", call. = FALSE)
  if (!"valid_count" %in% names(out)) out$valid_count <- out$support_state == "supported"
  if (any(is.na(out$valid_count))) stop("bird visits have missing valid_count state", call. = FALSE)
  out$valid_count <- out$valid_count %in% TRUE
  if ("support_state" %in% names(out)) {
    bad <- is.na(out$support_state) | !out$support_state %in% BIRD_OPPORTUNITY_SUPPORT
    if (any(bad)) stop("bird visits contain an unrecognized support_state", call. = FALSE)
    if (any((out$support_state == "supported") != out$valid_count))
      stop("bird visit valid_count and support_state fields disagree", call. = FALSE)
  }
  out$support_state <- ifelse(out$valid_count, "supported", "held")
  out$pointkey <- as.character(out$pointkey)
  year_num <- suppressWarnings(as.numeric(out$year))
  if (any(!is.finite(year_num) | year_num != floor(year_num)))
    stop("bird visits contain a non-finite or non-integer year", call. = FALSE)
  out$year <- as.integer(year_num)
  bout_num <- suppressWarnings(as.numeric(out$bout))
  if (any(!is.finite(bout_num) | bout_num != as.integer(bout_num) |
          !(as.integer(bout_num) %in% 1:2)))
    stop("bird visits contain a bout outside the supported 1/2 domain", call. = FALSE)
  out$bout <- as.character(as.integer(bout_num))
  if (any(is.na(out$survey_id) | !nzchar(out$survey_id)) ||
      any(as.character(out$visit_key) != out$survey_id) || anyDuplicated(out$survey_id))
    stop("bird visits require unique matching nonblank survey_id/visit_key values", call. = FALSE)
  if (any(is.na(out$pointkey) | !nzchar(trimws(out$pointkey))) ||
      any(is.na(out$bout) | !nzchar(trimws(as.character(out$bout)))))
    stop("bird visits contain an incomplete point-year or bout key", call. = FALSE)
  valid_bout_key <- paste(out$pointkey[out$valid_count], out$year[out$valid_count],
                          out$bout[out$valid_count], sep = "|")
  if (anyDuplicated(valid_bout_key))
    stop("bird visits contain duplicate valid bout labels within a point-year", call. = FALSE)

  has_summary <- BIRD_VISIT_SUMMARY_FIELDS %in% names(out)
  if (any(has_summary) && !all(has_summary))
    stop("bird visits contain an incomplete visit-level detection summary", call. = FALSE)
  if (all(has_summary)) {
    invalid_number <- function(x, integer = FALSE) {
      value <- suppressWarnings(as.numeric(x))
      !is.finite(value) | value < 0 | (integer & value != as.integer(value))
    }
    if (any(invalid_number(out$eligible_detection_rows, TRUE)) ||
        any(invalid_number(out$eligible_birds)) ||
        any(invalid_number(out$eligible_species, TRUE)) ||
        any(invalid_number(out$flyover_rows, TRUE)) ||
        any(invalid_number(out$flyover_birds)))
      stop("bird visits contain invalid visit-level detection summaries", call. = FALSE)
    expected_support <- ifelse(out$valid_count, "supported", "held")
    if (any(is.na(out$support_state) | out$support_state != expected_support))
      stop("bird visit support_state does not reconcile with valid_count", call. = FALSE)
    expected_outcome <- ifelse(!out$valid_count, "unavailable",
      ifelse(out$eligible_detection_rows > 0, "positive", "supported_zero"))
    if (any(is.na(out$outcome) | out$outcome != expected_outcome))
      stop("bird visit outcome does not reconcile with visit-level detections", call. = FALSE)
    if (any(!out$valid_count &
            (out$eligible_detection_rows != 0 | out$eligible_birds != 0 |
             out$eligible_species != 0 | out$flyover_rows != 0 |
             out$flyover_birds != 0)))
      stop("held bird visits carry detection summaries", call. = FALSE)
  }

  if (!is.null(obs)) {
    raw_obs <- bird_prepare_obs(obs)
    if (nrow(raw_obs) && !"survey_id" %in% names(raw_obs))
      stop("bird observations require survey_id for physical-count incidence", call. = FALSE)
    joined_ids <- if (nrow(raw_obs)) unique(as.character(
      raw_obs$survey_id[raw_obs$joined_valid_visit])) else character()
    orphan_join <- setdiff(joined_ids, out$survey_id[out$valid_count])
    if (length(orphan_join))
      stop(sprintf("%d joined bird detection visit(s) lack valid physical-count effort",
                   length(orphan_join)), call. = FALSE)
    if (all(has_summary)) {
      sp <- eligible_breeding_detections(raw_obs)
      fly <- raw_obs[raw_obs$joined_valid_visit & raw_obs$in_protocol_window &
        raw_obs$is_species %in% TRUE & raw_obs$is_flyover %in% TRUE &
        bird_valid_cluster_size(raw_obs$clusterSize), , drop = FALSE]
      expected <- data.frame(
        survey_id = out$survey_id,
        eligible_detection_rows = 0L, eligible_birds = 0,
        eligible_species = 0L, flyover_rows = 0L, flyover_birds = 0,
        stringsAsFactors = FALSE)
      add_groups <- function(d, flyover = FALSE) {
        if (is.null(d) || !nrow(d)) return(invisible(NULL))
        groups <- split(seq_len(nrow(d)), as.character(d$survey_id))
        index <- match(names(groups), expected$survey_id)
        if (anyNA(index)) stop("bird detection summary contains an unknown survey_id", call. = FALSE)
        if (flyover) {
          expected$flyover_rows[index] <<- vapply(groups, length, integer(1))
          expected$flyover_birds[index] <<- vapply(groups,
            function(i) sum(d$clusterSize[i]), numeric(1))
        } else {
          expected$eligible_detection_rows[index] <<- vapply(groups, length, integer(1))
          expected$eligible_birds[index] <<- vapply(groups,
            function(i) sum(d$clusterSize[i]), numeric(1))
          expected$eligible_species[index] <<- vapply(groups,
            function(i) length(unique(d$communityScientificName[i])), integer(1))
        }
        invisible(NULL)
      }
      add_groups(sp)
      add_groups(fly, flyover = TRUE)
      for (field in setdiff(names(expected), "survey_id")) {
        actual <- suppressWarnings(as.numeric(out[[field]]))
        if (any(abs(actual - expected[[field]]) > 1e-8))
          stop("bird visit-level detection summaries do not reconcile", call. = FALSE)
      }
    }
  }
  out
}

bird_validate_effort <- function(opportunity = NULL, visits = NULL, obs = NULL) {
  opp <- bird_validate_opportunity(opportunity, obs)
  vis <- bird_validate_visits(visits, obs)
  if (is.null(opp) || is.null(vis)) return(list(opportunity = opp, visits = vis))
  vkey <- paste(vis$pointkey, vis$year, sep = "|")
  by_key <- split(seq_len(nrow(vis)), vkey)
  counts <- data.frame(
    key = names(by_key),
    n_valid_bouts = as.integer(vapply(by_key, function(i) sum(vis$valid_count[i]), numeric(1))),
    n_held_bouts = as.integer(vapply(by_key, function(i) sum(!vis$valid_count[i]), numeric(1))),
    stringsAsFactors = FALSE)
  okey <- paste(opp$pointkey, opp$year, sep = "|")
  if (!setequal(okey, counts$key))
    stop("bird visit ledger and opportunity have different point-year universes", call. = FALSE)
  counts <- counts[match(okey, counts$key), , drop = FALSE]
  if (any(opp$n_valid_bouts != counts$n_valid_bouts) ||
      ("n_held_bouts" %in% names(opp) && any(opp$n_held_bouts != counts$n_held_bouts)))
    stop("bird visit ledger and opportunity disagree on point-year bout counts", call. = FALSE)
  list(opportunity = opp, visits = vis)
}

bird_valid_visit_count <- function(opportunity = NULL, visits = NULL, fallback = NULL) {
  effort <- bird_validate_effort(opportunity, visits)
  opp <- effort$opportunity; vis <- effort$visits
  from_opp <- if (!is.null(opp)) sum(opp$n_valid_bouts[opp$supported]) else NA_real_
  from_vis <- if (!is.null(vis)) sum(vis$valid_count) else NA_real_
  if (is.finite(from_opp) && is.finite(from_vis) && from_opp != from_vis)
    stop("bird visit ledger and point-year opportunity disagree on valid visit count", call. = FALSE)
  out <- if (is.finite(from_opp)) from_opp else if (is.finite(from_vis)) from_vis else suppressWarnings(as.numeric(fallback)[1])
  if (!is.finite(out) || out <= 0) return(NA_real_)
  out
}

# Public bundles carry observer support only at the least-granular level used by
# the product: one site total and one count per eligible species. Raw measuredBy
# values and deterministic row-level aliases remain outside the shipped RDS.
bird_validate_observer_support <- function(observer_support, species = character(),
                                           n_valid_visits = NULL) {
  species <- sort(unique(as.character(species[!is.na(species) & nzchar(species)])))
  unavailable <- list(
    n_observers = NA_integer_, n_valid_visits = NA_integer_,
    n_valid_visits_with_observer = NA_integer_, complete = FALSE,
    by_species = data.frame(scientificName = species,
                            n_observers = rep(NA_integer_, length(species)),
                            stringsAsFactors = FALSE))
  if (is.null(observer_support)) return(unavailable)
  if (!is.list(observer_support))
    stop("bird observer support must be an aggregate list", call. = FALSE)
  required <- c("n_observers", "n_valid_visits", "n_valid_visits_with_observer",
                "complete", "by_species")
  missing <- setdiff(required, names(observer_support))
  if (length(missing))
    stop(sprintf("bird observer support lacks required field(s): %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  counts <- suppressWarnings(as.numeric(unlist(observer_support[
    c("n_observers", "n_valid_visits", "n_valid_visits_with_observer")], use.names = FALSE)))
  if (length(counts) != 3L || any(!is.finite(counts)) || any(counts < 0) ||
      any(counts != as.integer(counts)) || counts[[3]] > counts[[2]] ||
      counts[[1]] > counts[[3]])
    stop("bird observer support has invalid aggregate visit/count values", call. = FALSE)
  complete <- observer_support$complete
  if (length(complete) != 1L || is.na(complete) || !is.logical(complete) ||
      !identical(complete, counts[[2]] > 0L && counts[[3]] == counts[[2]]))
    stop("bird observer support completeness disagrees with aggregate visit support", call. = FALSE)
  if (!is.null(n_valid_visits)) {
    expected <- suppressWarnings(as.numeric(n_valid_visits)[1])
    if (!is.finite(expected) || expected != counts[[2]])
      stop("bird observer support disagrees with valid visit effort", call. = FALSE)
  }
  by_species <- observer_support$by_species
  if (!is.data.frame(by_species) ||
      !identical(names(by_species), c("scientificName", "n_observers")))
    stop("bird observer support by_species must contain only scientificName and n_observers", call. = FALSE)
  by_species$scientificName <- as.character(by_species$scientificName)
  species_counts <- suppressWarnings(as.numeric(by_species$n_observers))
  if (any(is.na(by_species$scientificName) | !nzchar(by_species$scientificName)) ||
      anyDuplicated(by_species$scientificName) || any(!is.finite(species_counts)) ||
      any(species_counts < 0) || any(species_counts != as.integer(species_counts)) ||
      any(species_counts > counts[[1]]))
    stop("bird observer support has invalid species aggregate counts", call. = FALSE)
  by_species$n_observers <- as.integer(species_counts)
  by_species <- by_species[order(by_species$scientificName), , drop = FALSE]
  if (!identical(by_species$scientificName, species))
    stop("bird observer support species universe disagrees with eligible detections", call. = FALSE)
  list(
    n_observers = as.integer(counts[[1]]),
    n_valid_visits = as.integer(counts[[2]]),
    n_valid_visits_with_observer = as.integer(counts[[3]]),
    complete = complete,
    by_species = by_species
  )
}

# ---------------------------------------------------------------------------
# species_board(): one row per species — the Bird Board. Ubiquity is the lifetime
# point footprint. Detection frequency is the share of valid physical six-minute
# counts with an eligible detection; repeated counts at the same point are not
# described as independent. The abundance axis remains a detection index = birds
# per valid physical count (sum clusterSize / valid visits).
# ---------------------------------------------------------------------------
species_board <- function(obs, points = NULL, nvis = NULL, opportunity = NULL, visits = NULL,
                          observer_support = NULL) {
  raw <- bird_prepare_obs(obs)
  effort <- bird_validate_effort(opportunity, visits, raw)
  opp <- effort$opportunity; vis <- effort$visits
  sp <- eligible_breeding_detections(raw); if (is.null(sp) || !nrow(sp)) return(NULL)
  supported_points <- if (!is.null(vis)) unique(vis$pointkey[vis$valid_count]) else
    if (!is.null(opp)) unique(opp$pointkey[opp$supported]) else character()
  np <- if (length(supported_points)) length(supported_points) else if (!is.null(points) && "pointkey" %in% names(points))
    length(unique(points$pointkey)) else length(unique(sp$pointkey))
  n_supported_counts <- if (!is.null(vis)) sum(vis$valid_count) else NA_integer_
  nv <- bird_valid_visit_count(opp, visits, nvis)
  if (!is.finite(nv)) stop("species_board requires complete positive point-count effort", call. = FALSE)
  np <- max(1L, np)
  observer_summary <- bird_validate_observer_support(
    observer_support, unique(sp$communityScientificName), nv)
  # flyovers are summed for honest total_birds/detections, but EXCLUDED from the
  # on-point detection index (index_birds); flyovers are operationally excluded.
  fly <- raw[raw$joined_valid_visit & raw$in_protocol_window &
               raw$is_species %in% TRUE & raw$is_flyover &
               bird_valid_cluster_size(raw$clusterSize) &
               raw$communityScientificName %in% unique(sp$communityScientificName), , drop = FALSE] %>%
    dplyr::group_by(.data$communityScientificName) %>%
    dplyr::summarise(flyover_detections = dplyr::n(),
                     flyover_birds = sum(.data$clusterSize), .groups = "drop")
  out <- sp %>% dplyr::group_by(.data$communityScientificName) %>%
    dplyr::summarise(
      vernacular = bird_community_vernacular(
        dplyr::first(.data$communityScientificName), .data$reportedScientificName,
        .data$reportedTaxonRank, .data$reportedVernacularName),
      detections = dplyr::n(),
      index_birds = sum(.data$clusterSize),
      n_points = dplyr::n_distinct(.data$pointkey),
      n_detected_counts = dplyr::n_distinct(.data$survey_id),
      n_point_years = dplyr::n_distinct(paste(.data$pointkey, .data$year, sep = "|")),
      n_grids  = dplyr::n_distinct(.data$plotID),
      mean_cluster = round(mean(.data$clusterSize), 2),
      method = mode_chr(.data$methodCanonical),
      distance_n_observed = sum(.data$distance_state == "observed"),
      distance_n_used = sum(.data$distance_state == "observed" &
        is.finite(.data$observerDistance) & .data$observerDistance >= 0 &
        .data$observerDistance <= 200),
      distance_n_outside_truncation = sum(.data$distance_state == "observed" &
        is.finite(.data$observerDistance) &
        (.data$observerDistance < 0 | .data$observerDistance > 200)),
      distance_n_unavailable = sum(.data$distance_state != "observed"),
      .groups = "drop") %>%
    dplyr::rename(scientificName = "communityScientificName") %>%
    dplyr::left_join(observer_summary$by_species, by = "scientificName") %>%
    dplyr::mutate(ubiquity = round(100 * .data$n_points / np, 1),
                  n_visits = as.numeric(nv),
                  index = round(.data$index_birds / nv, 3),
                  detection_frequency = if (is.finite(n_supported_counts) && n_supported_counts > 0)
                    round(100 * .data$n_detected_counts / n_supported_counts, 1) else NA_real_,
                  distance_usable_pct = round(100 * .data$distance_n_observed /
                    pmax(1, .data$distance_n_observed + .data$distance_n_unavailable), 1),
                  observer_support_complete = observer_summary$complete,
                  opportunity_complete = !is.null(vis))
  names(fly)[names(fly) == "communityScientificName"] <- "scientificName"
  out <- dplyr::left_join(out, fly, by = "scientificName")
  out$flyover_detections[is.na(out$flyover_detections)] <- 0L
  out$flyover_birds[is.na(out$flyover_birds)] <- 0
  out$total_birds <- out$index_birds + out$flyover_birds
  out %>% dplyr::arrange(dplyr::desc(.data$index))
}

# site headline. birds_per_count is the protocol-filtered detection index — flyovers
# excluded operationally. flyover_birds carries the audited
# count so the UI can disclose it behind a click without altering the headline.
site_birds <- function(obs, points = NULL, nvis = NULL, opportunity = NULL, visits = NULL,
                       observer_support = NULL) {
  raw <- bird_prepare_obs(obs)
  effort <- bird_validate_effort(opportunity, visits, raw)
  opp <- effort$opportunity; vis <- effort$visits
  n_visits <- bird_valid_visit_count(opp, visits, nvis)
  if (!is.finite(n_visits)) return(NULL)
  brd <- species_board(raw, points, nvis, opp, vis, observer_support)
  sp <- eligible_breeding_detections(raw)
  observer_summary <- bird_validate_observer_support(
    observer_support, unique(sp$communityScientificName), n_visits)
  n_points <- if (!is.null(opp)) length(unique(opp$pointkey[opp$supported])) else
    if (!is.null(points) && "pointkey" %in% names(points)) length(unique(points$pointkey)) else
      length(unique(sp$pointkey))
  fly <- raw$joined_valid_visit & raw$in_protocol_window & raw$is_species & raw$is_flyover &
    bird_valid_cluster_size(raw$clusterSize)
  top <- NA_character_
  if (!is.null(brd) && nrow(brd)) {
    i <- which.max(brd$index)
    top <- if (!is.na(brd$vernacular[i]) && nzchar(brd$vernacular[i])) brd$vernacular[i] else brd$scientificName[i]
  }
  list(n_species = if (is.null(brd)) 0L else nrow(brd),
       birds_per_count = round(if (nrow(sp)) sum(sp$clusterSize) / n_visits else 0, 2),
       flyover_birds = sum(raw$clusterSize[fly], na.rm = TRUE),
       n_points = n_points, n_visits = n_visits,
       n_opportunities = if (!is.null(opp)) sum(opp$supported) else NA_integer_,
       n_supported_zero = if (!is.null(opp)) sum(opp$outcome == "supported_zero") else NA_integer_,
       n_observers = observer_summary$n_observers,
       top = top)
}

# A sample-incidence unit is one authoritative valid physical six-minute count,
# keyed by visits$survey_id. A point may contribute repeated counts across bouts
# and years; those repeated counts are explicit effort, not independent places.
sampling_count <- function(sp) {
  if (!"survey_id" %in% names(sp))
    stop("bird incidence detections require survey_id", call. = FALSE)
  as.character(sp$survey_id)
}

# Incidence support is the complete valid physical-count ledger, including counts
# with zero eligible in-window non-flyover detections. Detection-only legacy inputs
# remain readable, but `opportunity_complete = FALSE` prevents them from claiming
# a complete denominator.
bird_incidence_support <- function(obs, visits = NULL) {
  raw <- bird_prepare_obs(obs)
  sp <- eligible_breeding_detections(raw)
  vis <- bird_validate_visits(visits, raw)
  detected_units <- if (!is.null(sp) && nrow(sp)) sampling_count(sp) else character()
  if (is.null(vis)) {
    units <- sort(unique(detected_units))
    complete <- FALSE
  } else {
    units <- sort(unique(as.character(vis$survey_id[vis$valid_count])))
    complete <- TRUE
  }
  orphan <- setdiff(unique(detected_units), units)
  if (length(orphan)) stop(sprintf(
    "%d bird incidence detection count(s) lack valid physical-count effort",
    length(orphan)), call. = FALSE)
  Y <- if (!is.null(sp) && nrow(sp))
    tapply(detected_units, sp$communityScientificName, function(o) length(unique(o))) else numeric()
  list(Y = as.integer(Y), species = names(Y), T = length(units),
       T_counts = length(units), units = units,
       opportunity_complete = complete)
}

# ---------------------------------------------------------------------------
# Incidence-based richness estimate (bias-corrected Chao2) — species incidence
# across valid physical six-minute counts. Counts are repeated protocol samples,
# not independent places. Chao 1987; Colwell et al. 2012.
# ---------------------------------------------------------------------------
chao2_from_incidence <- function(inc, m, opportunity_complete = TRUE) {
  inc <- suppressWarnings(as.integer(inc))
  m <- suppressWarnings(as.integer(m)[1])
  if (!is.finite(m) || m < 0L || any(!is.finite(inc)) || any(inc < 1L) || any(inc > m))
    stop("invalid incidence frequencies or physical-count support", call. = FALSE)
  S <- length(inc); Q1 <- sum(inc == 1); Q2 <- sum(inc == 2)
  if (m < 2 || (S == 0 && !opportunity_complete)) return(NULL)
  if (S == 0) return(list(
    S_obs = 0L, chao2 = NA_real_, m = m, Q1 = 0L, Q2 = 0L,
    variance = NA_real_, unstable = TRUE, ci_lo = NA_real_, ci_hi = NA_real_,
    opportunity_complete = TRUE, suppressed_reason = "no eligible species detected"))
  corr <- (m - 1) / m
  chao <- S + corr * Q1 * (Q1 - 1) / (2 * (Q2 + 1))
  f0 <- max(0, chao - S)   # estimated undetected species
  # Bias-corrected variance and its asymmetric log-normal interval. The interval
  # is unavailable when singleton support is insufficient rather than reported as D-D.
  var_f0 <- if (Q2 > 0)
    corr * (Q1 * (Q1 - 1)) / (2 * (Q2 + 1)) +
      corr^2 * (Q1 * (2 * Q1 - 1)^2) / (4 * (Q2 + 1)^2) +
      corr^2 * (Q1^2 * Q2 * (Q1 - 1)^2) / (4 * (Q2 + 1)^4)
  else
    corr * (Q1 * (Q1 - 1)) / 2 + corr^2 * (Q1 * (2 * Q1 - 1)^2) / 4 -
      corr^2 * (Q1^4) / (4 * chao)
  var_f0 <- max(var_f0, 0)
  ci_lo <- ci_hi <- NA_real_
  if (f0 > 0 && var_f0 > 0) {
    K <- exp(1.96 * sqrt(log(1 + var_f0 / f0^2)))
    ci_lo <- S + f0 / K; ci_hi <- S + f0 * K
  }
  list(S_obs = S, chao2 = round(chao, 1), m = m, Q1 = Q1, Q2 = Q2, unstable = Q2 < 3,
       variance = var_f0,
       ci_lo = round(ci_lo, 1), ci_hi = round(ci_hi, 1),
       opportunity_complete = opportunity_complete,
       suppressed_reason = if (Q2 < 3) "Q2 < 3; point estimate must not lead" else NA_character_)
}

chao2_points <- function(obs, visits = NULL) {
  si <- bird_incidence_support(obs, visits)
  chao2_from_incidence(si$Y, si$T, si$opportunity_complete)
}

# Sample-based species accumulation over valid physical six-minute counts, mean
# over permutations. Repeated counts are explicit samples, not independent places.
bird_permutation_orders <- function(k, perms = 40L) {
  k <- as.integer(k); perms <- as.integer(perms)
  if (!is.finite(k) || k < 1L || !is.finite(perms) || perms < 1L)
    stop("permutation dimensions must be positive integers", call. = FALSE)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  vapply(seq_len(perms), function(seed) {
    set.seed(104729L + seed)
    sample.int(k, size = k, replace = FALSE)
  }, integer(k))
}

bird_accum <- function(obs, visits = NULL, perms = 40) {
  si <- bird_incidence_support(obs, visits); k <- si$T
  if (k < 2) return(NULL)
  sp <- eligible_breeding_detections(obs)
  byp <- stats::setNames(rep(list(character()), k), si$units)
  if (!is.null(sp) && nrow(sp)) {
    observed <- split(as.character(sp$communityScientificName), sampling_count(sp))
    for (nm in intersect(names(observed), names(byp))) byp[[nm]] <- unique(observed[[nm]])
  }
  orders <- bird_permutation_orders(k, perms)
  mat <- vapply(seq_len(ncol(orders)), function(s) {
    ord <- byp[orders[, s]]
    seen <- character(0); out <- integer(k)
    for (i in seq_len(k)) { seen <- union(seen, ord[[i]]); out[i] <- length(seen) }
    out
  }, numeric(k))
  data.frame(counts = seq_len(k), richness = round(rowMeans(mat), 1),
             opportunity_complete = si$opportunity_complete)
}

# ---------------------------------------------------------------------------
# Cross-site effort standardization (dependency-light, no iNEXT). Raw richness is
# an effort artifact, so the gradient compares richness rarefied to a common
# number of valid physical counts. Y is per-species count incidence and T is the
# complete valid-count support. Rarefaction standardizes sample-count size only;
# detectability, completeness, and spatiotemporal design can still differ. Hill
# q1/q2 below are unstandardized plug-in summaries of observed incidence.
# ---------------------------------------------------------------------------
site_incidence <- function(obs, visits = NULL) bird_incidence_support(obs, visits)
rarefy_incidence <- function(Y, T, t) {                # E[species] in t of T occasions
  if (is.na(t) || t < 1 || t > T) return(NA_real_)
  contrib <- ifelse(T - Y < t, 1, 1 - exp(lchoose(T - Y, t) - lchoose(T, t)))
  round(sum(contrib), 1)
}
coverage_incidence <- function(Y, T) {                 # sample completeness, 0–1
  U <- sum(Y); if (U == 0 || T < 2) return(NA_real_)
  Q1 <- sum(Y == 1); Q2 <- sum(Y == 2)
  A <- if (Q2 > 0) {
    (T - 1) * Q1 / ((T - 1) * Q1 + 2 * Q2)
  } else {
    numerator <- (T - 1) * max(Q1 - 1, 0)
    numerator / (numerator + 2)
  }
  1 - (Q1 / U) * A
}
# sample-coverage completeness for ONE site's detections (Chao & Jost 2012). The
# honest completeness story to lead with when the Chao2 point estimate is unstable.
site_coverage <- function(obs, visits = NULL) {
  si <- site_incidence(obs, visits); if (is.null(si)) return(NA_real_)
  coverage_incidence(si$Y, si$T)
}
hill_incidence <- function(Y) {                        # unstandardized plug-in q1/q2
  if (!length(Y) || !is.finite(sum(Y)) || sum(Y) <= 0) return(c(q1 = NA_real_, q2 = NA_real_))
  p <- Y / sum(Y); p <- p[p > 0]
  c(q1 = exp(-sum(p * log(p))), q2 = 1 / sum(p^2))
}

# ---------------------------------------------------------------------------
# Per-species detail (the Species Profile card).
# ---------------------------------------------------------------------------
species_detail <- function(obs, sci) {
  d <- bird_prepare_obs(obs)
  d <- d[d$communityScientificName == sci &
           !is.na(d$communityScientificName), , drop = FALSE]
  if (!nrow(d)) return(NULL); d
}
bird_prepare_held <- function(held) {
  if (is.null(held)) return(NULL)
  if (!is.data.frame(held) || !"hold_reason" %in% names(held))
    stop("held bird detections require an explicit hold_reason", call. = FALSE)
  d <- bird_prepare_obs(held)
  bad <- is.na(d$hold_reason) | !d$hold_reason %in% BIRD_DETECTION_HOLD_REASONS
  if (any(bad))
    stop(sprintf("unrecognized bird detection hold reason(s): %s",
                 paste(sort(unique(d$hold_reason[bad])), collapse = ", ")), call. = FALSE)
  if (any(d$enters_breeding_metrics %in% TRUE))
    stop("held bird detection satisfies the breeding-metric predicate", call. = FALSE)
  d
}
species_detection_export <- function(obs, held = NULL, sci) {
  current <- species_detail(obs, sci)
  if (is.null(current)) current <- bird_prepare_obs(obs)[0, , drop = FALSE]
  current$hold_reason <- rep(NA_character_, nrow(current))
  quarantined <- bird_prepare_held(held)
  if (!is.null(quarantined)) {
    quarantined <- quarantined[!is.na(quarantined$communityScientificName) &
      quarantined$communityScientificName == sci, , drop = FALSE]
  }
  if (!nrow(current)) return(quarantined)
  if (is.null(quarantined) || !nrow(quarantined)) return(current)
  dplyr::bind_rows(current, quarantined)
}

# Site-wide privacy-safe audit ledger. Unlike the species picker, this reaches
# flyover-only, coarse-identification-only, unsafe-taxonomy, and held rows too.
# Observer identities and free text never enter either public input table.
site_detection_audit_export <- function(obs, held = NULL) {
  current <- bird_prepare_obs(obs)
  if (is.null(current)) current <- data.frame()
  if (!"hold_reason" %in% names(current))
    current$hold_reason <- rep(NA_character_, nrow(current))
  quarantined <- bird_prepare_held(held)
  out <- if (is.null(quarantined) || !nrow(quarantined)) current else
    dplyr::bind_rows(current, quarantined)
  if (!nrow(out)) return(out)
  missing <- setdiff(SITE_AUDIT_KEEP, names(out))
  if (length(missing))
    stop(sprintf("site detection audit lacks required field(s): %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  out[, SITE_AUDIT_KEEP, drop = FALSE]
}
# Area-and-effort-standardized distance profile. This is descriptive detection
# frequency, not density, a fitted detection function, or proof of a "true"
# detectability curve. Flyovers and unavailable/sentinel distances never enter.
distance_decay <- function(obs, sci, opportunity = NULL, visits = NULL, nvis = NULL) {
  all_sp <- eligible_breeding_detections(obs)
  d <- all_sp[all_sp$communityScientificName == sci &
                !is.na(all_sp$communityScientificName), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  v <- d$observerDistance[d$distance_state == "observed" & is.finite(d$observerDistance) &
                          d$observerDistance >= 0 & d$observerDistance <= 200]
  if (length(v) < 8) return(NULL)   # n-gate: 3 detections across 6 bands is noise (Colwell/Buckland)
  effort <- bird_valid_visit_count(opportunity, visits, nvis)
  if (!is.finite(effort)) return(NULL) # do not publish a per-area rate from detection-bearing visits only
  brks <- c(0, 25, 50, 75, 100, 150, 200 + .Machine$double.eps^0.5)
  labs <- c("0–25","25–50","50–75","75–100","100–150","150–200")
  cl <- cut(v, breaks = brks, labels = labs, right = FALSE)
  tab <- as.data.frame(table(band = cl), responseName = "n")
  area_brks <- c(0, 25, 50, 75, 100, 150, 200)
  area_ha <- (pi * (area_brks[-1]^2 - area_brks[-length(area_brks)]^2)) / 10000
  out <- dplyr::left_join(data.frame(band = factor(labs, levels = labs), area_ha = area_ha), tab, by = "band")
  out$n <- ifelse(is.na(out$n), 0L, out$n)
  out$n_valid_visits <- as.numeric(effort)
  out$relative_rate <- round(out$n / (out$area_ha * effort), 6)
  out$distance_n_used <- length(v)
  out$distance_n_unavailable <- sum(d$distance_state != "observed")
  out$distance_n_outside_truncation <- sum(d$distance_state == "observed" &
    (d$observerDistance < 0 | d$observerDistance > 200))
  out$effort_complete <- TRUE
  out
}
# Annual series includes supported years with zero detections for the selected
# species. One or two bouts contribute separately to the birds-per-count and
# physical-count incidence denominators; this helper aggregates them to year only
# for the annual/audit display.
detection_by_year <- function(obs, sci, opportunity = NULL) {
  d <- eligible_breeding_detections(obs)
  d <- d[d$communityScientificName == sci &
           !is.na(d$communityScientificName), , drop = FALSE]
  det <- if (nrow(d)) d %>% dplyr::group_by(.data$year) %>%
    dplyr::summarise(birds = sum(.data$clusterSize, na.rm = TRUE), .groups = "drop") else
    data.frame(year = integer(), birds = numeric())
  opp <- bird_validate_opportunity(opportunity, obs)
  if (is.null(opp)) return(if (nrow(det)) det else NULL)
  eff <- opp %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(n_supported_point_years = sum(.data$supported),
                     n_unavailable_point_years = sum(!.data$supported),
                     n_valid_visits = sum(.data$n_valid_bouts), .groups = "drop")
  out <- dplyr::left_join(eff, det, by = "year")
  out <- out[out$n_valid_visits > 0, , drop = FALSE]
  out$surveyed <- TRUE
  out$support_state <- "supported"
  out$birds[is.na(out$birds)] <- 0
  out$birds_per_count <- out$birds / out$n_valid_visits
  out$detected <- out$birds > 0
  out
}

# Opportunity-complete annual community summary. Supported years can carry a
# true zero; years represented only by held attempts carry NA scientific metrics.
annual_bird_summary <- function(obs, opportunity, visits = NULL) {
  raw <- bird_prepare_obs(obs)
  effort <- bird_validate_effort(opportunity, visits, raw)
  opp <- effort$opportunity; vis <- effort$visits
  if (is.null(opp) || !nrow(opp)) return(NULL)
  sp <- eligible_breeding_detections(raw)
  annual <- opp %>% dplyr::group_by(.data$year) %>%
    dplyr::summarise(
      n_supported_point_years = sum(.data$supported),
      n_supported_zero_point_years = sum(.data$outcome == "supported_zero"),
      n_unavailable_point_years = sum(!.data$supported),
      n_valid_visits = sum(.data$n_valid_bouts),
      .groups = "drop")
  eligible <- if (nrow(sp)) sp %>% dplyr::group_by(.data$year) %>%
    dplyr::summarise(
      eligible_detection_rows = dplyr::n(),
      eligible_birds = sum(.data$clusterSize),
      eligible_species = dplyr::n_distinct(.data$communityScientificName),
      method_singing_pct = round(100 * mean(.data$method_singing), 1),
      method_calling_pct = round(100 * mean(.data$method_calling), 1),
      method_visual_pct = round(100 * mean(.data$method_visual), 1),
      method_drumming_pct = round(100 * mean(.data$method_drumming), 1),
      distance_n_observed = sum(.data$distance_state == "observed"),
      distance_n_unavailable = sum(.data$distance_state != "observed"),
      .groups = "drop") else data.frame(year = integer())
  fly <- raw[raw$joined_valid_visit & raw$in_protocol_window &
               raw$is_species & raw$is_flyover &
               bird_valid_cluster_size(raw$clusterSize), , drop = FALSE]
  fly <- if (nrow(fly)) fly %>% dplyr::group_by(.data$year) %>%
    dplyr::summarise(flyover_rows = dplyr::n(), flyover_birds = sum(.data$clusterSize),
                     .groups = "drop") else data.frame(year = integer())
  annual <- dplyr::left_join(annual, eligible, by = "year") %>%
    dplyr::left_join(fly, by = "year")
  zero_when_supported <- c("eligible_detection_rows", "eligible_birds", "eligible_species",
                           "distance_n_observed", "distance_n_unavailable")
  for (nm in zero_when_supported) {
    if (!nm %in% names(annual)) annual[[nm]] <- NA_real_
    annual[[nm]][is.na(annual[[nm]]) & annual$n_valid_visits > 0] <- 0
  }
  for (nm in c("method_singing_pct", "method_calling_pct", "method_visual_pct",
               "method_drumming_pct"))
    if (!nm %in% names(annual)) annual[[nm]] <- NA_real_
  for (nm in c("flyover_rows", "flyover_birds")) {
    if (!nm %in% names(annual)) annual[[nm]] <- 0
    annual[[nm]][is.na(annual[[nm]])] <- 0
  }
  annual$support_state <- ifelse(annual$n_valid_visits > 0, "supported", "unavailable")
  annual$outcome <- ifelse(annual$n_valid_visits == 0, "unavailable",
                    ifelse(annual$eligible_detection_rows > 0, "positive", "supported_zero"))
  annual$birds_per_count <- ifelse(annual$n_valid_visits > 0,
                                   annual$eligible_birds / annual$n_valid_visits, NA_real_)
  annual %>% dplyr::arrange(.data$year)
}
# primary detection method mix for a species
method_mix <- function(obs, sci) {
  d <- eligible_breeding_detections(obs)
  d <- d[d$communityScientificName == sci &
           !is.na(d$communityScientificName), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d %>% dplyr::group_by(.data$detectionMethod, .data$methodCanonical) %>%
    dplyr::summarise(n = dplyr::n(), method_singing = any(.data$method_singing),
                     method_calling = any(.data$method_calling),
                     method_visual = any(.data$method_visual),
                     method_drumming = any(.data$method_drumming), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$n))
}

# every species detected at one grid (plotID) — powers the map click panel + CSV
grid_species <- function(obs, plotid) {
  sp <- eligible_breeding_detections(obs); sp <- sp[!is.na(sp$plotID) & sp$plotID == plotid, , drop = FALSE]
  if (!nrow(sp)) return(NULL)
  sp %>% dplyr::group_by(.data$communityScientificName) %>%
    dplyr::summarise(vernacular  = bird_community_vernacular(
                       dplyr::first(.data$communityScientificName),
                       .data$reportedScientificName, .data$reportedTaxonRank,
                       .data$reportedVernacularName),
                     detections  = dplyr::n(),
                     birds       = sum(.data$clusterSize, na.rm = TRUE),
                     method      = mode_chr(.data$methodCanonical),
                     .groups = "drop") %>%
    dplyr::rename(scientificName = "communityScientificName") %>%
    dplyr::arrange(dplyr::desc(.data$birds), dplyr::desc(.data$detections))
}

# ---------------------------------------------------------------------------
# bird_qc(): the data-quality-flag system for ONE species (the Species Profile).
# The family's gold-standard feature, ported from the Small Mammal Tracker's
# individual_qc_flags()/flagged_measure_captures(). Returns ranked "verify, not
# wrong" flags PLUS the exact offending detections behind each, so the UI can
# list them (clickable) and download a QC report. Thresholds grounded in BBS /
# IMBCR / distance-sampling practice (Fauna review; see docs/neonize-playbook.md):
#   high = almost certainly an error (vernacular drift; distance > 1 km)
#   warn = worth a look (exact-0 / visual-far distance; solitary-species mega-cluster; under-effort points)
#   info = a note (missing-distance share; flocking-species log-scale outliers)
# Flagged-far / large-cluster records are RETAINED for modelling (truncated only
# at analysis, per Buckland) — a flag means "review", never "delete".
# Returns list(flags = <list(level,title,key,n,detail)>, sets = <named list of data.frames>).
# ---------------------------------------------------------------------------
bird_qc <- function(obs, sci, points = NULL) {
  out <- list(flags = list(), sets = list())
  d <- species_detail(obs, sci); if (is.null(d) || !nrow(d)) return(out)
  cols <- intersect(c("vernacularName","survey_id","pointkey","plotID","year","bout","observerDistanceRaw",
                      "scientificName","communityScientificName","reportedScientificName",
                      "reportedTaxonRank","reportedTaxonID","reportedVernacularName",
                      "community_unit_state",
                      "observerDistance","distance_state","detectionMethod","detection_method_state",
                      "methodCanonical",
                      "method_singing","method_calling","method_visual","method_drumming",
                      "method_other","method_unknown","pointCountMinuteRaw","pointCountMinute",
                      "point_count_minute_state","in_protocol_window","clusterSize","is_flyover",
                      "joined_valid_visit","enters_breeding_metrics","enters_index"), names(d))
  # tidy carries flag (title) + flag_key + flag_level so the QC report CSV is
  # self-describing and re-derivable per the codebook (no orphan codebook rows).
  tidy <- function(rows, label, key, level) {
    x <- d[rows, cols, drop = FALSE]; if (!nrow(x)) return(NULL)
    x$flag <- label; x$flag_key <- key; x$flag_level <- level; x }
  add <- function(level, title, key, rows, detail) {
    rows <- rows[!is.na(rows)]; n <- length(rows); if (!n) return(invisible())
    out$flags[[length(out$flags) + 1L]] <<- list(level = level, title = title, key = key, n = n, detail = detail)
    out$sets[[key]] <<- tidy(rows, title, key, level)
  }
  od   <- if ("observerDistance" %in% names(d)) suppressWarnings(as.numeric(d$observerDistance)) else rep(NA_real_, nrow(d))
  cs   <- if ("clusterSize"      %in% names(d)) suppressWarnings(as.numeric(d$clusterSize))      else rep(NA_real_, nrow(d))
  meth <- if ("detectionMethod"  %in% names(d)) as.character(d$detectionMethod)                  else rep(NA_character_, nrow(d))
  eligible <- d$enters_index %in% TRUE

  # 1 — common-name drift is assessed within an exact reported taxon. Different
  # subspecies may legitimately carry different common names after they pool to
  # one canonical species community unit.
  if (all(c("reportedScientificName", "reportedVernacularName") %in% names(d))) {
    reported_sci <- as.character(d$reportedScientificName)
    reported_vn <- trimws(as.character(d$reportedVernacularName))
    groups <- split(seq_len(nrow(d)), reported_sci)
    drift_groups <- Filter(function(rows) {
      vn <- unique(reported_vn[rows][!is.na(reported_vn[rows]) & nzchar(reported_vn[rows])])
      length(vn) > 1L
    }, groups)
    if (length(drift_groups)) {
      rows <- sort(unique(unlist(drift_groups, use.names = FALSE)))
      add("high", "Two common names for one reported scientific name", "vernacular", rows,
          sprintf("%d exact reported taxon name(s) map to multiple reported common names. Review a source taxonomy join or revision; expected parent/subspecies name differences are not flagged.",
                  length(drift_groups)))
    }
  }
  # 2 — implausibly far (> 1 km): a units / transcription error, not a real far bird
  add("high", "Non-flyover detection beyond 1 km", "far", which(eligible & is.finite(od) & od > 1000),
      "A non-flyover landbird was recorded beyond 1 km. Review the source distance and units; the descriptive distance profile truncates at 200 m but the raw record is retained.")
  # 3 — exact-0 distance (heaping at the origin / placeholder entry)
  add("warn", "Distance recorded as exactly 0 m", "zero", which(eligible & is.finite(od) & od == 0),
      "A non-flyover distance of exactly 0 m may be a real at-point detection or heaping/default entry. Retain it, but verify the source before fitting a distance model.")
  # 4 — visual ID at long range. 500 m (not 250) so it doesn't cry wolf on open
  # grassland, where conspicuous birds ARE legitimately seen far — past ~500 m an
  # unaided visual species ID of a landbird is not credible regardless of habitat.
  add("warn", "Visual-component ID at long range (> 500 m)", "visualfar", which(eligible & d$method_visual & is.finite(od) & od > 500),
      "A non-flyover record with a visual detection component was recorded beyond 500 m. Review the identification and distance; compound methods are included in this check.")
  # 5 — clusterSize: solitary-species mega-cluster (warn) OR flocking-species log-outlier (info).
  # "Effectively solitary" = 99% of detections are 1–3 birds (p99 <= 3): this keeps a genuine
  # flocking species (which is USUALLY counted as singletons on a breeding point count but has a
  # real tail of flocks, e.g. Red-winged Blackbird) OUT of the solitary branch — its p99 is high.
  csf <- cs[eligible & is.finite(cs)]
  if (length(csf) >= 10) {
    p99 <- stats::quantile(csf, 0.99, names = FALSE)
    if (is.finite(p99) && p99 <= 3) {
      add("warn", "Large flock for a typically solitary species", "cluster", which(eligible & is.finite(cs) & cs >= 6),
          "99% of this species' detections are 1–3 birds, yet these report 6+ in one cluster. Review for a possible transcription error, species-identification issue, or genuine unusual flock.")
    } else {
      l <- log1p(csf); mads <- stats::mad(l); thr <- stats::median(l) + 5 * mads
      if (is.finite(thr) && mads > 0)
        add("info", "Unusually large flock (vs this species)", "cluster", which(eligible & is.finite(cs) & log1p(cs) > thr),
            "Flock size far above this species' own typical range (judged on the log scale, so ordinary flocks don't flag). Often genuine for gregarious species, noted for review, not presumed wrong.")
    }
  }
  # 6 — missing distance: only surface when it's a MEANINGFUL share (>=10%). A handful of
  # NA-distance flyovers is routine and not worth a flag (that just cries wolf on every species).
  miss <- which(eligible & d$distance_state != "observed")
  pct <- if (sum(eligible)) round(100 * length(miss) / sum(eligible)) else 0
  if (length(miss) && pct >= 25)
    add("info", sprintf("Unavailable distance on %d%% of eligible detections", pct), "missing", miss,
        "A large share of eligible non-flyover detections lack an observed distance. Sentinel-not-estimable, source-missing, and invalid states remain distinct in the export and cannot enter the distance profile.")
  # 7 — missing methods fail closed. This remains a defensive QC channel for a
  # legacy/noncanonical obs table; current schema-v4 bundles place such rows in held.
  method_missing <- which(d$detection_method_state == "missing_or_unknown")
  if (length(method_missing))
    add("info", "Detection method unavailable", "methodmissing", method_missing,
        "These rows lack a usable detection method. They do not enter breeding metrics and current bundles retain them in the held/site-audit ledger.")
  # 8 — under-effort points (within-site robust MAD): low effort biases detection
  if (!is.null(points) && all(c("n_visits","pointkey") %in% names(points)) && "pointkey" %in% names(d)) {
    nv <- suppressWarnings(as.numeric(points$n_visits)); good <- is.finite(nv)
    if (sum(good) >= 5) {
      smed <- stats::median(nv[good]); smad <- stats::mad(nv[good])
      if (is.finite(smad) && smad > 0) {
        low_pts <- points$pointkey[good & nv < smed - 3 * smad]
        add("warn", "Detected at under-sampled point(s)", "loweffort", which(eligible & d$pointkey %in% low_pts),
            sprintf("Some detections fall on points visited far less than the site norm (< %.0f visits vs a site median of %.0f). Under-effort points under-detect species, biasing richness and any across-point comparison.", smed - 3 * smad, smed))
      }
    }
  }
  out
}

# every flagged detection for a species, across all flag types (the QC report CSV)
bird_qc_report <- function(obs, sci, points = NULL) {
  q <- bird_qc(obs, sci, points); if (!length(q$sets)) return(NULL)
  do.call(rbind, c(q$sets, list(make.row.names = FALSE)))
}

# ---------------------------------------------------------------------------
# EXPORT KEEP-VECTORS — the single source of truth for what each CSV download
# emits AND for the codebook. The codebook (below) is GENERATED by iterating the
# union of these vectors against BIRD_COL_DICT, so a column can never be exported
# without a documented codebook entry, and the codebook can never list a column no
# export emits (the codebook-from-keep-vector standard). Edit a keep-vector here
# and the codebook follows automatically; an undocumented column stop()s the boot.
# ---------------------------------------------------------------------------
SPCSV_KEEP <- c("scientificName","communityScientificName","community_unit_state",
                "reportedScientificName",
                "reportedTaxonRank","reportedTaxonID","reportedVernacularName",
                "vernacularName","survey_id","pointkey","plotID","year","bout",
                "pointCountMinuteRaw","pointCountMinute","point_count_minute_state",
                "in_protocol_window",
                "observerDistanceRaw","observerDistance","distance_state","detectionMethod",
                "detection_method_state",
                "methodCanonical","method_singing","method_calling","method_visual",
                "method_drumming","method_other","method_unknown","clusterSize","is_flyover",
                "joined_valid_visit","enters_breeding_metrics","enters_index","hold_reason")
BOARD_KEEP <- c("scientificName","vernacular","method","index","ubiquity","detections",
                "detection_frequency","total_birds","index_birds","flyover_detections",
                "flyover_birds","n_points","n_detected_counts","n_point_years","n_grids","n_visits",
                "mean_cluster","n_observers","distance_n_observed","distance_n_used",
                "distance_n_outside_truncation","distance_n_unavailable",
                "distance_usable_pct","observer_support_complete","opportunity_complete")
GRADIENT_KEEP <- c(
  "site","name","state","biome_lab",
  "analysis_year_min","analysis_year_max","bird_year_min","bird_year_max",
  "T_counts","n_visits_window","n_points_window","n_birds_window",
  "n_positive_counts_window","n_supported_zero_counts_window",
  "S_obs","U_incidence","Q1_incidence","Q2_incidence","S_rare","t_used",
  "coverage","hill_q1","hill_q2","mean_detection_frequency","mean_ubiquity",
  "pct_singing","birds_per_count_window","top_species_window",
  "breeding_temp_c","n_realized_months","n_supported_realized_months",
  "count_months","count_months_lab","precip_annual_mm","n_precip_months",
  "n_complete_precip_years","env_year_min","env_year_max")
GRIDCSV_KEEP <- c("scientificName","vernacularName","eligible_birds","detections","primary_method")
REPORT_KEEP <- c(
  "row_type", "site", "release", "site_year_min", "site_year_max", "site_n_species",
  "site_n_points", "site_n_valid_counts", "site_n_supported_zero_counts",
  "site_n_supported_point_years", "site_n_supported_zero_point_years",
  "site_birds_per_count", "site_flyover_birds", BOARD_KEEP)
SITE_AUDIT_KEEP <- c(
  "site","survey_id","occasion_id","eventID","pointkey","plotID","pointID","year","bout",
  "scientificName","communityScientificName","community_unit_state",
  "reportedScientificName","reportedTaxonRank","reportedTaxonID","reportedVernacularName",
  "pointCountMinuteRaw","pointCountMinute","point_count_minute_state","in_protocol_window",
  "observerDistanceRaw","observerDistance","distance_state","detectionMethod",
  "detection_method_state","methodCanonical",
  "method_singing","method_calling","method_visual","method_drumming","method_other",
  "method_unknown","clusterSize","is_flyover","matched_visit","valid_count",
  "joined_valid_visit","enters_breeding_metrics","enters_index","hold_reason")
# QC report / inspector exports carry these flag columns (added by bird_qc tidy()):
QC_KEEP    <- c("scientificName","communityScientificName","community_unit_state",
                "reportedScientificName",
                "reportedTaxonRank","reportedTaxonID","reportedVernacularName",
                "vernacularName","survey_id","pointkey","plotID","year","bout","observerDistanceRaw",
                "pointCountMinuteRaw","pointCountMinute","point_count_minute_state",
                "in_protocol_window",
                "observerDistance","distance_state","detectionMethod","methodCanonical",
                "detection_method_state",
                "method_singing","method_calling","method_visual","method_drumming",
                "method_other","method_unknown","clusterSize","is_flyover","joined_valid_visit",
                "enters_breeding_metrics","enters_index","flag","flag_key","flag_level")

# Master column dictionary: every column ANY export can emit -> units + NA-semantics.
# One row per column name (keyed); the codebook is a lookup over the keep-vectors.
BIRD_COL_DICT <- list(
  scientificName  = c("", "Canonical biological-species community unit used by every metric; the normalized genus + species binomial."),
  communityScientificName = c("", "Explicit copy of the canonical genus + species binomial used for richness, incidence, profiles, maps, search, and exports."),
  community_unit_state = c("category", "Canonicalization audit state: canonical_species / canonical_subspecies / not_species_level / missing_scientific_name / unsafe_scientific_name. Only canonical states enter species metrics."),
  reportedScientificName = c("source value", "Exact scientific name reported by NEON. A subspecies trinomial is retained here while its parent binomial is the community unit."),
  reportedTaxonRank = c("source value", "Exact NEON taxonRank associated with reportedScientificName."),
  reportedTaxonID = c("source value", "Exact NEON taxon identifier associated with the reported detection."),
  reportedVernacularName = c("source value", "Exact common name reported by NEON for this detection."),
  vernacularName  = c("", "Common (English) name reported for the detection; source spelling is also explicit in reportedVernacularName. NA = no common name recorded."),
  vernacular      = c("", "Community-unit display name. Prefers a reported parent-binomial species-row common name; otherwise uses a deterministic reported-name fallback. NA = none recorded."),
  pointkey        = c("", "Point identifier = plotID_pointID; the fixed spot an observer stands for a 6-minute count."),
  survey_id       = c("", "Authoritative physical-count sample-incidence key from siteID + eventID + plotID + pointID; contains no observer identity."),
  occasion_id     = c("", "Point-year opportunity key from siteID + plotID + pointID + year."),
  eventID         = c("source value", "NEON event identifier used to join a detection to its physical visit."),
  plotID          = c("", "NEON plot (grid) identifier."),
  pointID         = c("", "Point identifier within a plot."),
  year            = c("year", "Calendar year of the point-count."),
  bout            = c("bout #", "Bout (visit) number within the breeding season; a point may be counted 1–2x per year."),
  pointCountMinuteRaw = c("source token", "Raw within-count minute token from brd_countdata. Minute 88 denotes an incidental observation outside the formal count; missing and unknown tokens remain explicit."),
  pointCountMinute = c("minute #", "Parsed integer minute when the raw token is an integer. Only minutes 1–6 are inside the formal point-count window; NA means the token was missing or invalid."),
  point_count_minute_state = c("category", "Protocol-window provenance: standard_minute / incidental_minute_88 / source_missing / invalid_or_unknown."),
  in_protocol_window = c("logical", "TRUE only for standard point-count minutes 1–6. FALSE rows remain auditable but cannot enter protocol-filtered community metrics or flyover summaries."),
  observerDistanceRaw = c("source token", "Raw observer-distance token. Values 999/9999 are retained here as sentinel provenance and never treated as metres."),
  observerDistance= c("metres", "Finite nonnegative observer-estimated distance in metres when distance_state is observed. NA means unavailable, never zero."),
  distance_state  = c("category", "Distance provenance: observed / sentinel_not_estimable (raw 999/9999) / source_missing / invalid."),
  detectionMethod = c("category", "How the bird was first detected (singing / calling / visual / drumming / flyover / compound, e.g. 'visual and singing'). NA = not recorded."),
  detection_method_state = c("category", "reported when detectionMethod is nonblank and not unknown; missing_or_unknown otherwise. Missing/unknown methods fail closed for breeding metrics."),
  methodCanonical = c("category", "Canonical display channel chosen from the raw detectionMethod. Compound information remains in the component flags."),
  method_singing  = c("logical", "TRUE when the raw detection method contains a singing component; compound methods can have multiple TRUE flags."),
  method_calling  = c("logical", "TRUE when the raw detection method contains a calling component; compound methods can have multiple TRUE flags."),
  method_visual   = c("logical", "TRUE when the raw detection method contains a visual component; compound methods can have multiple TRUE flags."),
  method_drumming = c("logical", "TRUE when the raw detection method contains a drumming component; compound methods can have multiple TRUE flags."),
  method_other    = c("logical", "TRUE for a recorded non-flyover method without singing, calling, visual, or drumming components."),
  method_unknown  = c("logical", "TRUE when detectionMethod is blank, missing, or explicitly unknown."),
  clusterSize     = c("# birds", "Source count of birds in the detection (a flock is one row with clusterSize > 1). Only positive finite integer counts enter metrics; other values remain held."),
  matched_visit   = c("logical", "TRUE when the source detection matched exactly one authoritative brd_perpoint visit."),
  valid_count     = c("logical", "TRUE when the matched visit has samplingImpractical == OK."),
  is_species      = c("logical", "TRUE only when the reported species/subspecies name can be safely mapped to a canonical binomial community unit."),
  is_flyover      = c("logical", "TRUE if this detection was recorded as a bird passing overhead. It remains auditable but is operationally excluded from every on-point community metric."),
  joined_valid_visit = c("logical", "TRUE when the detection joins unambiguously to a valid brd_perpoint physical visit."),
  enters_breeding_metrics = c("logical", "Shared eligibility predicate: valid joined visit + formal point-count minute 1–6 + safely canonicalized species/subspecies + known detection method + positive finite integer clusterSize + non-flyover."),
  enters_index    = c("logical", "Backwards-compatible alias for enters_breeding_metrics. Re-derive the index as sum(clusterSize[enters_index]) / valid physical visits."),
  hold_reason     = c("category", "Why a source detection is held outside app metrics. NA denotes an auditable obs row; minute-specific values include incidental_outside_point_count, missing_point_count_minute, and invalid_point_count_minute."),
  primary_method  = c("category", "Modal canonical detection channel across eligible non-flyover records for a species at the grid. NA = none recorded."),
  method          = c("category", "Modal canonical detection channel across eligible non-flyover records for the species."),
  index           = c("birds / point-count", "Detection index = sum(clusterSize, FLYOVERS EXCLUDED) / point-counts run. A relative detection index, NOT a population. Detectability differs by species."),
  ubiquity        = c("% of supported points", "Lifetime point detection footprint = % of supported points where the species was ever detected. It is effort- and detectability-dependent context, not occupancy."),
  detection_frequency = c("% valid physical counts", "Percentage of the complete valid six-minute physical-count universe with an eligible detection of the species. Repeated counts are protocol samples, not independent places; this is neither geographic spread nor detection-corrected occupancy."),
  detections      = c("# detection rows", "Number of eligible non-flyover detection rows for the species; a count of records, not birds."),
  total_birds     = c("# birds", "Eligible in-window non-flyover birds plus audited flyover birds for a species that has at least one eligible on-point record; flyover-only taxa do not enter the Board."),
  index_birds     = c("# birds", "Birds entering the index = sum(clusterSize) with flyovers EXCLUDED."),
  flyover_detections = c("# detection rows", "Audited flyover rows for a species with at least one eligible on-point record; excluded operationally from community metrics."),
  flyover_birds   = c("# birds", "Birds in flyover detections (quarantined from the index). total_birds = index_birds + flyover_birds."),
  eligible_birds  = c("# birds", "Eligible protocol-window non-flyover birds represented by the exported row. In the grid export this is the sum across the canonical species at that grid."),
  mean_cluster    = c("# birds", "Mean clusterSize across the species' detections."),
  n_points        = c("# points", "Number of distinct supported points where the species was detected (per-species) or with valid effort (per-site)."),
  n_detected_counts = c("# valid physical counts", "Number of distinct valid survey_id physical counts with an eligible detection of the species; the numerator of detection_frequency."),
  n_point_years   = c("# detected point-years", "Annual/audit context: number of distinct point x year records with an eligible detection of the species. This is not the incidence denominator."),
  n_grids         = c("# grids", "Number of distinct grids (plots) where the species was detected."),
  n_visits        = c("# valid physical counts", "Number of valid brd_perpoint physical visits (point x year x bout) used as the birds-per-count denominator."),
  n_observers     = c("# observers", "Aggregate number of distinct observers supporting the eligible species at this site. Row-level observer values and pseudonyms are not published."),
  observer_support_complete = c("logical", "TRUE when every valid site visit supplied observer support. Only aggregate counts are published."),
  distance_n_observed = c("# detections", "Eligible detections with finite nonnegative observer distance."),
  distance_n_used = c("# detections", "Eligible detections with finite observed distance inside the fixed inclusive 0–200 m distance-profile window."),
  distance_n_outside_truncation = c("# detections", "Eligible detections with finite observed distance outside the fixed 0–200 m distance-profile window. Retained in the audit but excluded from plotted bars."),
  distance_n_unavailable = c("# detections", "Eligible detections whose distance state is sentinel_not_estimable, source_missing, or invalid."),
  distance_usable_pct = c("% eligible detections", "Percent of eligible detections with distance_state = observed."),
  opportunity_complete = c("logical", "TRUE when the metric used the explicit brd_perpoint-derived valid physical-count ledger, including supported-zero counts, rather than a detection-only legacy floor."),
  flag            = c("category", "Which data-quality review flag this row tripped (the flag's title). 'verify, not wrong'; flagged rows are RETAINED, never deleted."),
  flag_key        = c("", "Machine key of the QC flag (vernacular / far / zero / visualfar / cluster / missing / methodmissing / loweffort)."),
  flag_level      = c("category", "Severity of the QC flag: high (almost certainly an error) / warn (worth a look) / info (a note)."),
  n_species       = c("# species", "Observed biological-species richness at the site: distinct canonical binomial community units (= S_obs); subspecies never add richness."),
  S_obs           = c("# species", "Observed biological-species richness: distinct canonical binomial community units detected."),
  chao2           = c("# species", "Bias-corrected Chao2 sample-incidence richness extrapolation across valid physical counts: Sobs + ((T-1)/T) * Q1 * (Q1-1) / (2 * (Q2+1)). The point estimate does not lead when Q2 < 3."),
  coverage        = c("0-1", "Chao/Jost incidence coverage across valid physical counts: estimated share of total incidence probability represented by already-detected species. This does not correct detectability or survey design."),
  S_rare          = c("# species", "Species richness rarefied to t_used valid physical counts within the stated analysis window. Rarefaction standardizes sample-count support only; it does not equalize coverage, detectability, space, timing, or design."),
  t_used          = c("# valid physical counts", "Common valid physical-count support used for S_rare; the minimum T_counts across the exact 47-site 2017-2024 roster."),
  birds_per_count = c("birds / point-count", "Site detection index = eligible in-window non-flyover birds / valid point-counts run."),
  pct_singing     = c("% eligible detections", "Share of eligible detections whose raw method includes a singing component; compound methods are included."),
  hill_q1         = c("# species", "Unstandardized plug-in Hill q1 = exp(Shannon) of the observed physical-count incidence frequencies. It is not rarefied, effort-robust, or detection-corrected."),
  hill_q2         = c("# species", "Unstandardized plug-in Hill q2 (inverse Simpson) of the observed physical-count incidence frequencies. It is not rarefied, effort-robust, or detection-corrected."),
  mean_detection_frequency = c("% valid physical counts", "Arithmetic mean across detected species of their valid-count detection_frequency within the stated analysis window; descriptive and not detection-corrected."),
  mean_ubiquity   = c("% of supported points", "Arithmetic mean across detected species of their point detection footprint within the stated analysis window. It is effort- and detectability-dependent context, not occupancy."),
  analysis_year_min = c("year", "Inclusive lower bound of the fixed cross-site bird analysis window (2017)."),
  analysis_year_max = c("year", "Inclusive upper bound of the fixed cross-site bird analysis window (2024)."),
  bird_year_min   = c("year", "Earliest year containing a valid physical bird count for this site within the fixed analysis window."),
  bird_year_max   = c("year", "Latest year containing a valid physical bird count for this site within the fixed analysis window."),
  T_counts        = c("# valid physical counts", "Complete sample-incidence support: number of distinct valid survey_id physical counts in the fixed 2017-2024 cross-site window, including supported zeros."),
  n_visits_window = c("# valid physical counts", "Audit copy of the valid physical-count total in the fixed analysis window; it must equal T_counts."),
  n_points_window = c("# points", "Number of distinct pointkey locations with at least one valid physical count in the fixed analysis window."),
  n_birds_window  = c("# birds", "Eligible in-window non-flyover birds summed across valid physical counts in the fixed analysis window."),
  n_positive_counts_window = c("# valid physical counts", "Valid physical counts in the fixed analysis window with at least one eligible in-window non-flyover detection."),
  n_supported_zero_counts_window = c("# valid physical counts", "Valid physical counts in the fixed analysis window with no eligible in-window non-flyover detection. Together with positive counts these conserve T_counts."),
  U_incidence     = c("# species-count incidences", "Sum of species incidence frequencies across valid physical counts in the fixed analysis window."),
  Q1_incidence    = c("# species", "Number of species detected on exactly one valid physical count in the fixed analysis window."),
  Q2_incidence    = c("# species", "Number of species detected on exactly two valid physical counts in the fixed analysis window; sparse Q2 makes Chao2 unstable."),
  birds_per_count_window = c("birds / valid physical count", "Window-specific detection index = eligible in-window non-flyover birds / valid physical counts in 2017-2024. It is not abundance, density, occupancy, or population size."),
  top_species_window = c("", "Canonical species with the greatest eligible bird total in the fixed analysis window; ties are resolved alphabetically. This is a detection result, not dominance or abundance."),
  breeding_temp_c = c("degrees C", "Equal-weight mean of coverage-qualified RELEASE-2026 calendar-month temperature climatologies for the distinct months in which valid 2017-2024 bird counts occurred. NA when even one realized count month lacks support; the site is then omitted only from the temperature gradient and no value is imputed. Environmental context only; not a bird measurement."),
  n_realized_months = c("# calendar months", "Number of distinct calendar months (1-12) containing valid bird counts in the fixed 2017-2024 analysis window."),
  n_supported_realized_months = c("# calendar months", "Number of realized bird-count calendar months with a coverage-qualified RELEASE-2026 temperature climatology. When it is smaller than n_realized_months, breeding_temp_c is NA and the exact support boundary remains explicit."),
  count_months    = c("month numbers", "Comma-separated distinct calendar-month numbers containing valid bird counts in the fixed 2017-2024 analysis window."),
  count_months_lab = c("calendar months", "Human-readable labels for count_months."),
  precip_annual_mm= c("mm / year", "Mean across complete 12-month annual precipitation totals in the pinned RELEASE-2026 context record. NA means no complete year; support is partial, missing values are never imputed, and this is not a bird response."),
  n_precip_months = c("# months", "Number of RELEASE-2026 environmental months with reportable precipitation at the site; it does not by itself establish a complete year."),
  n_complete_precip_years = c("# years", "Number of complete 12-month calendar years supporting precip_annual_mm. Zero means precipitation remains NA."),
  env_year_min    = c("year", "Earliest year represented in the pinned RELEASE-2026 environmental context bundle."),
  env_year_max    = c("year", "Latest year represented in the pinned RELEASE-2026 environmental context bundle."),
  site            = c("", "NEON 4-letter site code."),
  name            = c("", "NEON site name."),
  state           = c("", "US state / territory of the site."),
  biome_lab       = c("", "Biome class used for the gradient colour/legend (Forest / Grassland / Desert / Tundra / Tropical dry forest)."),
  top_species     = c("", "Most-detected species at the site across the full release record (by the detection index); not used in the fixed-window cross-site comparison."),
  row_type        = c("", "Site-report record type: species for a Bird Board row, or site_summary when a fully sampled site has no eligible species detections."),
  release         = c("", "Pinned NEON release identity for the exported record."),
  site_year_min   = c("year", "Earliest supported point-count year represented in the site report."),
  site_year_max   = c("year", "Latest supported point-count year represented in the site report."),
  site_n_species  = c("# species", "Site-wide observed canonical-species richness repeated on each species row of the site report."),
  site_n_points   = c("# points", "Site-wide number of point locations with valid effort, repeated on each species row of the site report."),
  site_n_valid_counts = c("# valid physical counts", "Site-wide valid physical-count denominator repeated on each species row of the site report."),
  site_n_supported_zero_counts = c("# valid physical counts", "Site-wide valid physical counts with no eligible non-flyover detections, repeated on each species row of the site report."),
  site_n_supported_point_years = c("# point-years", "Site-wide supported point-year total retained for annual/audit context and repeated on each species row of the site report."),
  site_n_supported_zero_point_years = c("# point-years", "Site-wide supported point-years with no eligible non-flyover detections, repeated on each species row of the site report."),
  site_birds_per_count = c("birds / valid physical count", "Site-wide eligible non-flyover bird detection index repeated on each species row of the site report; not abundance or population size."),
  site_flyover_birds = c("# birds", "Site-wide audited flyover birds excluded from community metrics, repeated on each species row of the site report.")
)

# ---------------------------------------------------------------------------
# bird_codebook(): machine-readable FAIR data dictionary, GENERATED from the export
# keep-vectors so it cannot drift from the columns actually emitted. Iterates the
# union of every keep-vector (plus the QC flag_key/flag_level meta-columns) against
# BIRD_COL_DICT; an emitted column missing a dictionary entry stop()s (caught at
# boot / build), so a new export column can never ship undocumented.
# ---------------------------------------------------------------------------
bird_codebook <- function() {
  cols <- unique(c(SPCSV_KEEP, BOARD_KEEP, GRADIENT_KEEP, GRIDCSV_KEEP, REPORT_KEEP,
                   SITE_AUDIT_KEEP, QC_KEEP,
                   "flag_key", "flag_level",
                   "S_obs","chao2","coverage"))   # site-summary columns surfaced elsewhere
  missing <- setdiff(cols, names(BIRD_COL_DICT))
  if (length(missing))
    stop(sprintf("bird_codebook(): %d exported column(s) have no BIRD_COL_DICT entry: %s",
                 length(missing), paste(missing, collapse = ", ")), call. = FALSE)
  cb <- data.frame(
    column      = cols,
    units       = vapply(cols, function(c) BIRD_COL_DICT[[c]][1], character(1)),
    description = vapply(cols, function(c) BIRD_COL_DICT[[c]][2], character(1)),
    row.names   = NULL, stringsAsFactors = FALSE)
  # Provenance / license row (NEON CC BY 4.0), first so it's visible on open.
  lic <- data.frame(
    column      = "_source",
    units       = "license",
    description = "Source: NEON DP1.10003.001, CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/); aggregated and derived by this app.",
    stringsAsFactors = FALSE)
  rbind(lic, cb)
}

# per-point summary for the map. A supported point with no eligible detection is
# zero; a point with no valid visit remains unavailable (NA), not zero.
point_summary <- function(obs, points, opportunity = NULL) {
  if (is.null(points) || !is.data.frame(points) || !"pointkey" %in% names(points))
    stop("point_summary requires a points table with pointkey", call. = FALSE)
  if (any(is.na(points$pointkey) | !nzchar(trimws(as.character(points$pointkey)))) ||
      anyDuplicated(as.character(points$pointkey)))
    stop("points require unique nonblank pointkey values", call. = FALSE)
  sp <- eligible_breeding_detections(obs)
  per <- sp %>% dplyr::group_by(.data$pointkey) %>%
    dplyr::summarise(richness = dplyr::n_distinct(.data$communityScientificName),
                     birds = sum(.data$clusterSize), .groups = "drop")
  opp <- bird_validate_opportunity(opportunity, obs)
  if (!is.null(opp)) {
    if (!setequal(as.character(points$pointkey), as.character(opp$pointkey)))
      stop("points and opportunity have different point universes", call. = FALSE)
    effort <- opp %>% dplyr::group_by(.data$pointkey) %>%
      dplyr::summarise(n_visits_ledger = sum(.data$n_valid_bouts),
                       n_supported_point_years = sum(.data$supported),
                       n_unavailable_point_years = sum(!.data$supported), .groups = "drop")
    out <- dplyr::left_join(points, effort, by = "pointkey")
    if (any(is.na(out$n_visits_ledger)))
      stop("points contain a point absent from the opportunity ledger", call. = FALSE)
    if ("n_visits" %in% names(out)) {
      point_effort <- suppressWarnings(as.numeric(out$n_visits))
      if (any(!is.finite(point_effort)) || any(point_effort != out$n_visits_ledger))
        stop("points and opportunity disagree on valid visit effort", call. = FALSE)
    }
    out$n_visits <- out$n_visits_ledger
    out$n_visits_ledger <- NULL
    out$opportunity_complete <- TRUE
  } else {
    if (!"n_visits" %in% names(points))
      stop("point_summary requires opportunity or points$n_visits", call. = FALSE)
    out <- points
    out$opportunity_complete <- FALSE
  }
  out <- dplyr::left_join(out, per, by = "pointkey")
  supported <- suppressWarnings(as.numeric(out$n_visits)) > 0
  out$richness[is.na(out$richness) & supported] <- 0L
  out$birds[is.na(out$birds) & supported] <- 0
  out$per_visit <- ifelse(out$n_visits > 0, round(out$birds / out$n_visits, 1), NA_real_)
  out
}

suppressPackageStartupMessages(library(dplyr))
source("R/bird_helpers.R")
source("R/site_metadata.R")

n_checks <- 0L
check <- function(ok, label) {
  n_checks <<- n_checks + 1L
  if (length(ok) != 1L || is.na(ok) || !isTRUE(ok))
    stop(sprintf("CHECK %d FAILED: %s", n_checks, label), call. = FALSE)
  invisible(TRUE)
}
check_equal <- function(actual, expected, label, tolerance = 1e-8) {
  check(isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE)), label)
}
expect_error <- function(expr, pattern, label) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  check(inherits(err, "error") && grepl(pattern, conditionMessage(err), ignore.case = TRUE), label)
}

# A compact schema-v4 site fixture. Its seven valid physical counts contain four
# positives and three supported zeros (ordinary empty, flyover-only, and coarse-
# identification-only); two held visits remain unavailable. Point-year summaries
# are retained for annual/audit views, but A_1 x 2020 has two valid bouts and both
# remain separate sample-incidence count units.
visits <- data.frame(
  survey_id = paste0("survey-", 1:9),
  pointkey = c("PLOT_A_01", "PLOT_A_01", "PLOT_A_02", "PLOT_A_03",
               "PLOT_A_01", "PLOT_A_02", "PLOT_A_03", "PLOT_A_04", "PLOT_A_05"),
  year = c(2020L, 2020L, 2020L, 2020L, 2021L, 2021L, 2021L, 2021L, 2022L),
  bout = c("1", "2", "1", "1", "1", "1", "1", "1", "1"),
  valid_count = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE)

opportunity <- data.frame(
  pointkey = c("PLOT_A_01", "PLOT_A_02", "PLOT_A_03", "PLOT_A_01",
               "PLOT_A_02", "PLOT_A_03", "PLOT_A_04", "PLOT_A_05"),
  plotID = c("PLOT_A", "PLOT_A", "PLOT_A", "PLOT_A", "PLOT_A", "PLOT_A", "PLOT_A", "PLOT_A"),
  year = c(2020L, 2020L, 2020L, 2021L, 2021L, 2021L, 2021L, 2022L),
  n_bouts_recorded = c(2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  n_valid_bouts = c(2L, 1L, 1L, 1L, 0L, 1L, 1L, 0L),
  n_held_bouts = c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L),
  n_surveyed_minutes = c(12, 6, 6, 6, 0, 6, 6, 0),
  supported = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
  outcome = c("positive", "supported_zero", "supported_zero", "positive",
              "unavailable", "supported_zero", "positive", "unavailable"),
  eligible_detection_rows = c(10L, 0L, 0L, 2L, 0L, 0L, 2L, 0L),
  eligible_birds = c(10, 0, 0, 3, 0, 0, 2, 0),
  eligible_species = c(1L, 0L, 0L, 2L, 0L, 0L, 2L, 0L),
  flyover_rows = c(1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L),
  flyover_birds = c(5, 0, 20, 0, 0, 0, 0, 0),
  stringsAsFactors = FALSE)

alpha_methods <- c("singing", "calling and singing", "visual and singing", "visual",
                   "calling", "drumming", "singing", "singing", "singing", "singing")
obs <- data.frame(
  survey_id = c(rep(c("survey-1", "survey-2"), 5), "survey-1", "survey-4",
                "survey-5", "survey-5", "survey-7", "survey-8", "survey-8"),
  pointkey = c(rep("PLOT_A_01", 11), "PLOT_A_03", rep("PLOT_A_01", 2),
               "PLOT_A_03", rep("PLOT_A_04", 2)),
  plotID = "PLOT_A",
  year = c(rep(2020L, 12), 2021L, 2021L, 2021L, 2021L, 2021L),
  bout = c(rep(c("1", "2"), 5), "1", "1", "1", "1", "1", "1", "1"),
  eventID = paste0("event-", seq_len(17)),
  scientificName = c(rep("Alpha avis", 11), "Flyus only", "Alpha avis", "Beta avis",
                     "Corvus sp.", "Beta avis", "Gamma avis"),
  vernacularName = c(rep("Alpha bird", 11), "Fly-only bird", "Alpha bird", "Beta bird",
                     "Coarse crow", "Beta bird", "Gamma bird"),
  taxonRank = c(rep("species", 14), "speciesGroup", "species", "subspecies"),
  is_species = c(rep(TRUE, 14), FALSE, TRUE, TRUE),
  pointCountMinute = rep(1:6, length.out = 17),
  observerDistanceRaw = c("0", "10", "600", "50", "75", "100", "150", "199", "200", "999",
                          "40", "999", "9999", "", "35", "-1", NA),
  detectionMethod = c(alpha_methods, "flyover", "flyover", "calling", "calling",
                      "calling", "visual and singing", "visual"),
  clusterSize = c(rep(1, 10), 5, 20, 2, 1, 1, 1, 1),
  matched_visit = TRUE,
  valid_count = TRUE,
  enters_incidence = c(rep(TRUE, 10), FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
  hold_reason = NA_character_,
  stringsAsFactors = FALSE)

add_visit_summaries <- function(ledger, detections) {
  raw <- bird_prepare_obs(detections)
  out <- ledger
  out$eligible_detection_rows <- 0L
  out$eligible_birds <- 0
  out$eligible_species <- 0L
  out$flyover_rows <- 0L
  out$flyover_birds <- 0
  fill <- function(rows, flyover = FALSE) {
    if (!nrow(rows)) return(invisible(NULL))
    groups <- split(seq_len(nrow(rows)), as.character(rows$survey_id))
    idx <- match(names(groups), out$survey_id)
    stopifnot(!anyNA(idx))
    if (flyover) {
      out$flyover_rows[idx] <<- vapply(groups, length, integer(1))
      out$flyover_birds[idx] <<- vapply(groups, function(i) sum(rows$clusterSize[i]), numeric(1))
    } else {
      out$eligible_detection_rows[idx] <<- vapply(groups, length, integer(1))
      out$eligible_birds[idx] <<- vapply(groups,
        function(i) sum(rows$clusterSize[i][rows$enters_index[i] %in% TRUE]), numeric(1))
      out$eligible_species[idx] <<- vapply(groups,
        function(i) length(unique(rows$communityScientificName[i])), integer(1))
    }
    invisible(NULL)
  }
  fill(eligible_breeding_detections(raw))
  fly <- raw[raw$joined_valid_visit & raw$in_protocol_window & raw$is_species %in% TRUE &
    raw$is_flyover %in% TRUE & bird_valid_cluster_size(raw$clusterSize), , drop = FALSE]
  fill(fly, flyover = TRUE)
  out$support_state <- ifelse(out$valid_count, "supported", "held")
  out$outcome <- ifelse(!out$valid_count, "unavailable",
    ifelse(out$eligible_detection_rows > 0L, "positive", "supported_zero"))
  out
}
visits <- add_visit_summaries(visits, obs)

observer_support <- list(
  n_observers = 6L,
  n_valid_visits = 7L,
  n_valid_visits_with_observer = 7L,
  complete = TRUE,
  by_species = data.frame(
    scientificName = c("Alpha avis", "Beta avis", "Gamma avis"),
    n_observers = c(2L, 2L, 1L),
    stringsAsFactors = FALSE)
)

points <- data.frame(
  pointkey = paste0("PLOT_A_0", 1:5), plotID = "PLOT_A",
  n_visits = c(3L, 1L, 2L, 1L, 0L),
  lat = 42 + 1:5 / 100, lng = -72 - 1:5 / 100,
  stringsAsFactors = FALSE)

# Raw channel preparation and the one shared breeding predicate.
prepared <- bird_prepare_obs(obs)
check(sum(prepared$distance_state == "sentinel_not_estimable") == 3L,
      "999/9999 sentinels retain their own distance state")
check(sum(prepared$distance_state == "source_missing") == 2L,
      "blank and NA distance tokens are source_missing")
check(sum(prepared$distance_state == "invalid") == 1L,
      "negative distance is invalid")
check(all(is.na(prepared$observerDistance[prepared$distance_state != "observed"])),
      "only observed distance states receive numeric metres")
check(prepared$observerDistanceRaw[10] == "999", "raw sentinel token is preserved")
check(prepared$method_singing[2] && prepared$method_calling[2],
      "calling-and-singing retains both method components")
check(prepared$method_singing[3] && prepared$method_visual[3],
      "visual-and-singing retains both method components")
check(prepared$methodCanonical[3] == "singing", "compound method has stable display channel")
eligible <- eligible_breeding_detections(obs)
check(nrow(eligible) == 14L, "shared predicate selects exactly the eligible records")
check(setequal(unique(eligible$scientificName), c("Alpha avis", "Beta avis", "Gamma avis")),
      "flyover-only and coarse taxa never enter breeding metrics")
expect_error(distance_audit("999", "observed"), "disagree", "distance raw/state conflict fails closed")

# Minute 88 is NEON's incidental channel outside the formal six-minute count;
# missing and unparseable minute values are also fail-closed but remain auditable.
protocol_rows <- obs[rep(1L, 3L), , drop = FALSE]
protocol_rows$eventID <- paste0("protocol-event-", seq_len(nrow(protocol_rows)))
protocol_rows$pointCountMinute <- c("88", NA, "unknown")
protocol_rows$enters_incidence <- FALSE
protocol_rows$hold_reason <- c("incidental_outside_point_count", "missing_point_count_minute",
                               "invalid_point_count_minute")
protocol_prepared <- bird_prepare_obs(protocol_rows)
check(identical(protocol_prepared$point_count_minute_state,
                c("incidental_minute_88", "source_missing", "invalid_or_unknown")),
      "minute 88, missing, and unknown values retain distinct audit states")
check(!any(protocol_prepared$in_protocol_window) &&
      !nrow(eligible_breeding_detections(protocol_rows)),
      "only point-count minutes 1-6 can enter breeding metrics")
check(protocol_prepared$pointCountMinuteRaw[1] == "88" &&
      is.na(protocol_prepared$pointCountMinuteRaw[2]) &&
      protocol_prepared$pointCountMinuteRaw[3] == "unknown" &&
      all(is.na(protocol_prepared$pointCountMinute[2:3])),
      "raw point-count minute tokens survive while invalid numeric values remain NA")
expect_error(point_count_minute_audit("88", "standard_minute"), "disagree",
             "point-count minute raw/state conflict fails closed")
expect_error(point_count_minute_audit("88", in_protocol_window = TRUE), "disagree",
             "point-count minute raw/window conflict fails closed")
obs_with_protocol_holds <- rbind(obs, protocol_rows)

unknown_method_row <- obs[1, , drop = FALSE]
unknown_method_row$eventID <- "unknown-method-event"
unknown_method_row$detectionMethod <- NA_character_
unknown_method_row$enters_incidence <- FALSE
unknown_method_row$hold_reason <- "missing_or_unknown_detection_method"
unknown_method_prepared <- bird_prepare_obs(unknown_method_row)
check(unknown_method_prepared$method_unknown &&
      unknown_method_prepared$detection_method_state == "missing_or_unknown" &&
      !unknown_method_prepared$enters_incidence &&
      !nrow(eligible_breeding_detections(unknown_method_row)),
      "missing detectionMethod fails closed with an explicit audit state")
unknown_literal <- unknown_method_row
unknown_literal$detectionMethod <- "unknown"
check(!nrow(eligible_breeding_detections(unknown_literal)),
      "literal unknown detectionMethod also fails closed")
check(all(eligible_breeding_detections(obs)$detectionMethod != "unknown"),
      "known compound methods remain eligible while unknown methods do not")

fractional_cluster_row <- obs[1, , drop = FALSE]
fractional_cluster_row$eventID <- "fractional-cluster-event"
fractional_cluster_row$clusterSize <- 1.5
fractional_cluster_row$enters_incidence <- TRUE
fractional_cluster_row$hold_reason <- NA_character_
fractional_prepared <- bird_prepare_obs(fractional_cluster_row)
check(nrow(eligible_breeding_detections(fractional_cluster_row)) == 1L &&
      fractional_prepared$enters_incidence && !fractional_prepared$enters_index,
      "a fractional clusterSize keeps the detection in incidence but out of the integer-count index")

# Visit/opportunity validation, including normalized alias support and failures.
opp <- bird_validate_opportunity(opportunity, obs)
vis <- bird_validate_visits(visits, obs)
check(all(opp$support_state == ifelse(opp$supported, "supported", "held")),
      "opportunity support alias normalizes deterministically")
check(all(vis$support_state == ifelse(vis$valid_count, "supported", "held")),
      "visit support alias normalizes deterministically")
check_equal(bird_valid_visit_count(opportunity, visits), 7, "valid physical visit denominator is complete")
effort <- bird_validate_effort(opportunity, visits, obs)
check(nrow(effort$opportunity) == 8L && nrow(effort$visits) == 9L,
      "visit and point-year ledgers reconcile")

opp_alias <- opportunity
opp_alias$support_state <- ifelse(opp_alias$supported, "supported", "held")
opp_alias$supported <- NULL
check(sum(bird_validate_opportunity(opp_alias, obs)$supported) == 6L,
      "documented support_state-only opportunity schema is accepted")

dup_opp <- rbind(opportunity, opportunity[1, , drop = FALSE])
expect_error(bird_validate_opportunity(dup_opp), "duplicate", "duplicate point-year fails closed")
bad_support <- opportunity; bad_support$supported[5] <- TRUE
expect_error(bird_validate_opportunity(bad_support), "disagree", "support and valid bouts cannot conflict")
bad_outcome <- opportunity; bad_outcome$outcome[2] <- "positive"
expect_error(bird_validate_opportunity(bad_outcome, obs), "reconcile", "supported-zero outcome is executable")
bad_summary <- opportunity; bad_summary$eligible_birds[1] <- 999
expect_error(bird_validate_opportunity(bad_summary, obs), "summaries", "opportunity conservation totals are executable")
orphan_obs <- obs; orphan_obs$pointkey[1] <- "ORPHAN_01"
expect_error(bird_validate_opportunity(opportunity, orphan_obs), "lack supported opportunity",
             "eligible detection without opportunity fails closed")
dup_vis <- rbind(visits, visits[1, , drop = FALSE])
expect_error(bird_validate_visits(dup_vis), "unique", "duplicate physical visit fails closed")
blank_vis <- visits; blank_vis$survey_id[1] <- ""
expect_error(bird_validate_visits(blank_vis), "nonblank", "blank physical visit ID fails closed")
fractional_year_vis <- visits; fractional_year_vis$year[1] <- 2020.5
expect_error(bird_validate_visits(fractional_year_vis), "year", "noninteger visit year fails closed")
dup_bout_vis <- visits
dup_bout_vis$bout[2] <- "1"
expect_error(bird_validate_visits(dup_bout_vis), "duplicate valid bout",
             "duplicate valid bout within a point-year fails closed")
bad_bout_vis <- visits
bad_bout_vis$bout[1] <- "3"
expect_error(bird_validate_visits(bad_bout_vis), "bout outside",
             "visit bout outside the protocol domain fails closed")
bad_vis <- visits; bad_vis$valid_count[1] <- FALSE; bad_vis$support_state[1] <- "held"
bad_vis$outcome[1] <- "unavailable"
bad_vis[1, c("eligible_detection_rows", "eligible_birds", "eligible_species",
             "flyover_rows", "flyover_birds")] <- 0
expect_error(bird_validate_effort(opportunity, bad_vis), "bout counts",
             "visit/opportunity effort conflict fails closed")
bad_visit_summary <- visits; bad_visit_summary$eligible_birds[1] <- 999
expect_error(bird_validate_visits(bad_visit_summary, obs), "summaries",
             "visit-level detection conservation fails closed")
orphan_survey_obs <- obs; orphan_survey_obs$survey_id[1] <- "survey-orphan"
expect_error(bird_validate_visits(visits, orphan_survey_obs), "lack valid physical-count effort|unknown survey_id",
             "eligible detection without a valid survey ID fails closed")

# Board/headline metrics use all valid physical visits; point-years remain audit fields.
board <- species_board(obs, points, 7, opportunity, visits, observer_support)
check(nrow(board) == 3L, "Bird Board contains eligible taxa only")
check(all(c("detection_frequency", "n_observers", "distance_usable_pct") %in% names(board)),
      "Board exposes opportunity, observer, and distance support")
alpha <- board[board$scientificName == "Alpha avis", , drop = FALSE]
check_equal(alpha$index_birds, 12, "Alpha eligible birds exclude its flyover")
check_equal(alpha$flyover_birds, 5, "Alpha flyover remains auditable")
check_equal(alpha$total_birds, 17, "audited raw total conserves eligible plus flyover birds")
check_equal(alpha$index, round(12 / 7, 3), "detection index uses all seven valid physical visits")
check(alpha$n_detected_counts == 3L, "duplicate Alpha rows collapse within count but two bouts remain distinct")
check_equal(alpha$detection_frequency, round(100 * 3 / 7, 1),
            "frequency denominator includes every valid count and supported zero count")
check(alpha$n_point_years == 2L && alpha$n_visits == 7,
      "point-year remains annual/audit context rather than incidence grain")
check(alpha$n_observers == 2L && alpha$observer_support_complete,
      "observer support is summarized without publishing identifiers")
incomplete_observer_support <- observer_support
incomplete_observer_support$n_valid_visits_with_observer <- 6L
incomplete_observer_support$complete <- FALSE
board_missing_observer <- species_board(
  obs, points, 7, opportunity, visits, incomplete_observer_support)
alpha_missing_observer <- board_missing_observer[board_missing_observer$scientificName == "Alpha avis", ]
check(!alpha_missing_observer$observer_support_complete && alpha_missing_observer$n_observers == 2L,
      "incomplete aggregate observer support retains a documented minimum")
bad_observer_support <- observer_support
bad_observer_support$by_species$n_observers[1] <- 7L
expect_error(species_board(obs, points, 7, opportunity, visits, bad_observer_support),
             "invalid species aggregate", "observer aggregates cannot exceed the site total")
check(!any(c("observer_id", "measuredBy", "source_row") %in% c(names(obs), names(visits))),
      "public-schema fixture contains no row-level observer or source identifiers")
check(alpha$distance_n_observed == 9L && alpha$distance_n_unavailable == 2L,
      "Board distance support separates observed and unavailable")
check(alpha$distance_n_used == 8L && alpha$distance_n_outside_truncation == 1L &&
      alpha$distance_n_observed == alpha$distance_n_used + alpha$distance_n_outside_truncation,
      "Board export conserves observed distances across the inclusive 0–200 m truncation")
check(!"Flyus only" %in% board$scientificName, "flyover-only species is quarantined from search/board surfaces")
board_with_protocol_holds <- species_board(
  obs_with_protocol_holds, points, 7, opportunity, visits, observer_support)
board_with_protocol_holds <- board_with_protocol_holds[order(board_with_protocol_holds$scientificName), ]
board_protocol_reordered <- species_board(
  obs_with_protocol_holds[rev(seq_len(nrow(obs_with_protocol_holds))), , drop = FALSE],
  points, 7, opportunity, visits, observer_support)
board_protocol_reordered <- board_protocol_reordered[order(board_protocol_reordered$scientificName), ]
board_base <- board[order(board$scientificName), ]
metric_columns <- c("scientificName", "detections", "index_birds", "index",
                    "n_detected_counts", "detection_frequency", "n_point_years")
check_equal(board_with_protocol_holds[, metric_columns], board_base[, metric_columns],
            "held minute-88/missing/unknown rows cannot change Board metrics")
check_equal(board_protocol_reordered[, metric_columns], board_with_protocol_holds[, metric_columns],
            "protocol-window exclusion is invariant to detection-row ordering")

headline <- site_birds(obs, points, 7, opportunity, visits, observer_support)
check(headline$n_species == 3L && headline$n_opportunities == 6L,
      "site headline uses the supported opportunity universe")
check(headline$n_supported_zero == 3L, "ordinary, flyover-only, and coarse-only zeros are supported")
check_equal(headline$birds_per_count, round(15 / 7, 2), "site detection index excludes all flyovers")
check_equal(headline$flyover_birds, 25, "site headline audits flyover-only taxa separately")

# Incidence estimators and accumulation retain empty sampled physical counts.
inc <- site_incidence(obs, visits)
inc_named <- stats::setNames(inc$Y, inc$species)
check(inc$T == 7L && inc$T_counts == 7L && inc$opportunity_complete,
      "incidence T is the complete valid physical-count universe")
check_equal(unname(inc_named[c("Alpha avis", "Beta avis", "Gamma avis")]), c(3L, 2L, 1L),
            "two valid bouts count separately while duplicate rows within a count collapse")
ch <- chao2_points(obs, visits)
check(ch$S_obs == 3L && ch$m == 7L && ch$Q1 == 1L && ch$Q2 == 1L,
      "Chao2 singleton/doubleton support is correct")
check(ch$unstable && grepl("Q2 < 3", ch$suppressed_reason),
      "unstable Chao2 point estimate is explicitly suppressed")
acc <- bird_accum(obs, visits, perms = 12)
check(nrow(acc) == 7L && tail(acc$richness, 1) == 3 && all(acc$opportunity_complete),
      "accumulation traverses supported zero counts and ends at observed richness")
orders <- bird_permutation_orders(7L, 12L)
check(length(unique(apply(orders, 2, paste, collapse = ","))) > 1L &&
      any(vapply(seq_len(ncol(orders)), function(i) !identical(orders[, i], seq_len(6L)), logical(1))),
      "accumulation uses multiple non-identity permutations")
acc_reordered <- bird_accum(obs[rev(seq_len(nrow(obs))), , drop = FALSE],
                            visits = visits, perms = 12)
check_equal(acc_reordered$richness, acc$richness,
            "accumulation is invariant to detection-row ordering")
check_equal(site_coverage(obs, visits), 0.875,
            "coverage uses the complete valid-count incidence ledger")
check_equal(rarefy_incidence(inc$Y, inc$T, inc$T), 3, "rarefaction at full support equals observed richness")
check_equal(coverage_incidence(1L, 10L), 1,
            "incidence coverage uses the explicit Q2=0 correction")
expect_error(chao2_from_incidence(c(8L, 11L), 10L), "invalid incidence",
             "incidence Y greater than T fails closed")

chao_q2 <- chao2_from_incidence(c(rep(1L, 4), rep(2L, 3), rep(3L, 13)), 10L)
check_equal(chao_q2$chao2, 21.4, "bias-corrected Chao2 point estimate uses Q2+1 correction")
check_equal(chao_q2$variance, 4.17234375,
            "bias-corrected Chao2 Q2-positive variance regression", tolerance = 1e-10)
check_equal(c(chao_q2$ci_lo, chao_q2$ci_hi), c(20.2, 31.5),
            "bias-corrected Chao2 Q2-positive interval regression")
chao_no_q2 <- chao2_from_incidence(c(rep(1L, 4), rep(3L, 16)), 10L)
check_equal(chao_no_q2$chao2, 25.4, "bias-corrected Chao2 Q2-zero point regression")
check_equal(chao_no_q2$variance, 43.0490551,
            "bias-corrected Chao2 Q2-zero variance regression", tolerance = 1e-7)
check_equal(c(chao_no_q2$ci_lo, chao_no_q2$ci_hi), c(20.8, 54.9),
            "bias-corrected Chao2 Q2-zero interval regression")
chao_no_q1 <- chao2_from_incidence(rep(3L, 20), 10L)
check(chao_no_q1$chao2 == 20 && is.na(chao_no_q1$ci_lo) && is.na(chao_no_q1$ci_hi),
      "Q1-zero Chao2 interval is unavailable rather than a degenerate interval")

# The shared cross-site window is inclusive at 2017/2024 and excludes lifetime
# records on both sides. Unique outside-window species and points make leakage
# into S_obs, T_counts, or n_points_window immediately visible.
window_obs <- obs[rep(1L, 4L), , drop = FALSE]
window_obs$survey_id <- paste0("window-", c(2016, 2017, 2024, 2025))
window_obs$pointkey <- paste0("WINDOW_", c(2016, 2017, 2024, 2025))
window_obs$plotID <- "WINDOW"
window_obs$year <- c(2016L, 2017L, 2024L, 2025L)
window_obs$bout <- "1"
window_obs$eventID <- paste0("window-event-", c(2016, 2017, 2024, 2025))
window_obs$scientificName <- paste("Window", c("sixteen", "seventeen", "twentyfour", "twentyfive"))
window_obs$vernacularName <- window_obs$scientificName
window_obs$taxonRank <- "species"
window_obs$is_species <- TRUE
window_obs$clusterSize <- 1L
window_obs$enters_incidence <- TRUE
window_visits <- data.frame(
  survey_id = window_obs$survey_id, pointkey = window_obs$pointkey,
  year = window_obs$year, bout = "1", valid_count = TRUE,
  stringsAsFactors = FALSE)
window_visits <- add_visit_summaries(window_visits, window_obs)
window_keep <- window_visits$valid_count &
  window_visits$year >= BIRD_CROSS_SITE_YEAR_MIN &
  window_visits$year <= BIRD_CROSS_SITE_YEAR_MAX
window_visits_kept <- window_visits[window_keep, , drop = FALSE]
window_obs_kept <- window_obs[window_obs$survey_id %in% window_visits_kept$survey_id, , drop = FALSE]
window_inc <- site_incidence(window_obs_kept, window_visits_kept)
check(identical(sort(window_visits_kept$year), c(2017L, 2024L)) &&
      window_inc$T_counts == 2L && length(window_inc$Y) == 2L &&
      length(unique(window_visits_kept$pointkey)) == 2L,
      "cross-site window includes 2017/2024 and excludes unique 2016/2025 species and points")

# A parent binomial plus any number of reported subspecies are one biological
# species community unit. Source-reported taxonomy remains row-level provenance.
canonical_obs <- obs[c(1, 13, 16, 17), , drop = FALSE]
canonical_obs$scientificName <- c(
  "Alpha avis", "Alpha avis minor", "Alpha avis major", "Beta avis")
canonical_obs$vernacularName <- c(
  "Alpha bird", "Lesser Alpha bird", "Greater Alpha bird", "Beta bird")
canonical_obs$taxonRank <- c("species", "subspecies", "subspecies", "species")
canonical_obs$is_species <- TRUE
canonical_obs$detectionMethod <- c("singing", "calling", "visual", "singing")
canonical_obs$clusterSize <- 1
canonical_obs$pointCountMinute <- 1:4
canonical_obs$enters_incidence <- TRUE
canonical_obs$hold_reason <- NA_character_
canonical_prepared <- bird_prepare_obs(canonical_obs)
check(identical(canonical_prepared$communityScientificName,
                c("Alpha avis", "Alpha avis", "Alpha avis", "Beta avis")),
      "parent species and multiple subspecies normalize to one biological species unit")
check(identical(canonical_prepared$reportedScientificName,
                c("Alpha avis", "Alpha avis minor", "Alpha avis major", "Beta avis")) &&
      identical(canonical_prepared$reportedTaxonRank,
                c("species", "subspecies", "subspecies", "species")),
      "canonicalization preserves exact reported scientific names and ranks")

canonical_opportunity <- opportunity
canonical_opportunity$eligible_detection_rows <- 0L
canonical_opportunity$eligible_birds <- 0
canonical_opportunity$eligible_species <- 0L
canonical_opportunity$flyover_rows <- 0L
canonical_opportunity$flyover_birds <- 0
canonical_opportunity$outcome <- ifelse(canonical_opportunity$supported,
                                        "supported_zero", "unavailable")
canonical_keys <- paste(canonical_opportunity$pointkey, canonical_opportunity$year, sep = "|")
canonical_detection_keys <- paste(canonical_prepared$pointkey, canonical_prepared$year, sep = "|")
for (key in unique(canonical_detection_keys)) {
  oi <- which(canonical_keys == key)
  di <- which(canonical_detection_keys == key)
  canonical_opportunity$eligible_detection_rows[oi] <- length(di)
  canonical_opportunity$eligible_birds[oi] <- sum(canonical_prepared$clusterSize[di])
  canonical_opportunity$eligible_species[oi] <-
    length(unique(canonical_prepared$communityScientificName[di]))
  canonical_opportunity$outcome[oi] <- "positive"
}
canonical_observer_support <- list(
  n_observers = 3L, n_valid_visits = 7L, n_valid_visits_with_observer = 7L,
  complete = TRUE,
  by_species = data.frame(
    scientificName = c("Alpha avis", "Beta avis"), n_observers = c(2L, 1L),
    stringsAsFactors = FALSE))
canonical_visits <- add_visit_summaries(visits[, setdiff(names(visits), BIRD_VISIT_SUMMARY_FIELDS), drop = FALSE],
                                        canonical_obs)
canonical_opp_checked <- bird_validate_opportunity(canonical_opportunity, canonical_obs)
check(sum(canonical_opp_checked$eligible_species) == 4L &&
      canonical_opp_checked$eligible_species[canonical_keys == "PLOT_A_04|2021"] == 2L,
      "opportunity species counts collapse subspecies while preserving distinct species")
canonical_board <- species_board(
  canonical_obs, points, 7, canonical_opportunity, canonical_visits,
  canonical_observer_support)
check(identical(sort(canonical_board$scientificName), c("Alpha avis", "Beta avis")) &&
      canonical_board$detections[canonical_board$scientificName == "Alpha avis"] == 3L &&
      canonical_board$n_observers[canonical_board$scientificName == "Alpha avis"] == 2L,
      "Bird Board, search source, and observer aggregates use canonical species units")
check(canonical_board$vernacular[canonical_board$scientificName == "Alpha avis"] ==
        "Alpha bird",
      "canonical species display prefers the parent-binomial vernacular")
canonical_inc <- site_incidence(canonical_obs, canonical_visits)
canonical_inc_named <- stats::setNames(canonical_inc$Y, canonical_inc$species)
check_equal(unname(canonical_inc_named[c("Alpha avis", "Beta avis")]), c(3L, 1L),
            "incidence merges parent and subspecies without merging a distinct species")
check(chao2_points(canonical_obs, canonical_visits)$S_obs == 2L &&
      rarefy_incidence(canonical_inc$Y, canonical_inc$T, canonical_inc$T) == 2,
      "Chao2 and full-support rarefaction count biological species units")
canonical_acc <- bird_accum(canonical_obs, canonical_visits, perms = 12)
check(tail(canonical_acc$richness, 1) == 2,
      "species accumulation ends at canonical biological-species richness")
canonical_annual <- annual_bird_summary(canonical_obs, canonical_opportunity, canonical_visits)
check(canonical_annual$eligible_species[canonical_annual$year == 2020L] == 1L &&
      canonical_annual$eligible_species[canonical_annual$year == 2021L] == 2L,
      "annual richness collapses subspecies and retains distinct species")
canonical_points <- point_summary(canonical_obs, points, canonical_opportunity)
check(canonical_points$richness[canonical_points$pointkey == "PLOT_A_01"] == 1L &&
      canonical_points$richness[canonical_points$pointkey == "PLOT_A_04"] == 2L,
      "map richness uses canonical biological species units")
canonical_grid <- grid_species(canonical_obs, "PLOT_A")
check(identical(sort(canonical_grid$scientificName), c("Alpha avis", "Beta avis")),
      "grid/search-facing species rows are canonical and unique")
check(canonical_grid$vernacular[canonical_grid$scientificName == "Alpha avis"] ==
        "Alpha bird",
      "grid display cannot relabel a parent binomial with a subspecies vernacular")
canonical_export <- species_detection_export(canonical_obs, NULL, "Alpha avis")
check(nrow(canonical_export) == 3L &&
      identical(sort(canonical_export$reportedScientificName),
                c("Alpha avis", "Alpha avis major", "Alpha avis minor")) &&
      length(unique(canonical_export$communityScientificName)) == 1L,
      "species export retains every source-reported taxon behind one community unit")
canonical_qc <- bird_qc(canonical_obs, "Alpha avis", points)
check(!"vernacular" %in% names(canonical_qc$sets),
      "legitimate parent/subspecies common-name variation is not a taxonomy-drift flag")

unsafe_obs <- canonical_obs[1, , drop = FALSE]
unsafe_obs$scientificName <- "Alpha avis (Linnaeus)"
unsafe_obs$enters_incidence <- FALSE
unsafe_obs$is_species <- FALSE
unsafe_prepared <- bird_prepare_obs(unsafe_obs)
check(!unsafe_prepared$is_species && is.na(unsafe_prepared$communityScientificName) &&
      unsafe_prepared$community_unit_state == "unsafe_scientific_name" &&
      !nrow(eligible_breeding_detections(unsafe_obs)),
      "unsafe nomenclatural forms fail closed instead of inflating species metrics")

empty_obs <- obs[0, , drop = FALSE]
zero_opp <- opportunity[c(2, 6), , drop = FALSE]
zero_vis <- visits[c(3, 7), , drop = FALSE]
zero_points <- points[c(2, 3), , drop = FALSE]
zero_points$n_visits <- 1L
zero_observer_support <- list(
  n_observers = 2L, n_valid_visits = 2L, n_valid_visits_with_observer = 2L,
  complete = TRUE,
  by_species = data.frame(scientificName = character(), n_observers = integer(),
                          stringsAsFactors = FALSE))
zero_headline <- site_birds(
  empty_obs, zero_points, 2, zero_opp, zero_vis, zero_observer_support)
check(zero_headline$n_species == 0L && zero_headline$birds_per_count == 0,
      "an all-zero sampled site is supported, not unavailable")
zero_inc <- site_incidence(empty_obs, zero_vis)
check(zero_inc$T == 2L && !length(zero_inc$Y),
      "all-zero valid-count incidence retains its full denominator")
zero_chao <- chao2_points(empty_obs, zero_vis)
check(zero_chao$S_obs == 0L && zero_chao$m == 2L && zero_chao$opportunity_complete,
      "empty incidence columns retain complete opportunity support")
check(is.null(chao2_points(empty_obs, zero_vis[1, , drop = FALSE])),
      "fewer than two valid physical counts do not support Chao2")

# Distance profile is effort- and area-standardized but never labelled density.
dd <- distance_decay(obs, "Alpha avis", opportunity, visits)
check(nrow(dd) == 6L && sum(dd$n) == 8L, "distance profile applies the 0-200 m truncation")
check(dd$n[as.character(dd$band) == "150–200"] == 3L,
      "exactly 200 m is retained in the terminal distance band")
check(all(dd$n_valid_visits == 7) && all(dd$effort_complete),
      "distance profile exposes the full visit denominator")
check_equal(dd$relative_rate[1], dd$n[1] / (dd$area_ha[1] * 7),
            "relative distance rate divides by annulus area and valid visits", tolerance = 1e-6)
check(!"density" %in% names(dd), "descriptive relative distance rate is not mislabeled density")
check(dd$distance_n_used[1] == 8L && dd$distance_n_unavailable[1] == 2L &&
      dd$distance_n_outside_truncation[1] == 1L,
      "distance used/unavailable/truncated channels remain auditable")
check(is.null(distance_decay(obs, "Alpha avis")),
      "distance rate is suppressed without complete effort support")

# Annual zeros are emitted only for supported years; held-only 2022 is absent.
gamma_year <- detection_by_year(obs, "Gamma avis", opportunity)
check(identical(gamma_year$year, c(2020L, 2021L)) && all(gamma_year$surveyed),
      "annual series exposes supported years only")
check_equal(gamma_year$birds, c(0, 1), "sampled annual non-detection is zero")
check(!2022L %in% gamma_year$year, "held-only year is absent rather than zero")
annual <- annual_bird_summary(obs, opportunity, visits)
check(nrow(annual) == 3L && annual$support_state[annual$year == 2022L] == "unavailable",
      "annual community audit retains held-only support state")
check(is.na(annual$eligible_birds[annual$year == 2022L]),
      "held-only annual community metric is unavailable, not zero")
check(annual$n_supported_zero_point_years[annual$year == 2020L] == 2L,
      "annual audit counts flyover-only and ordinary supported zeros")

# Point/map, method, and QC helpers share the same eligibility predicate.
pt <- point_summary(obs, points, opportunity)
check(pt$richness[pt$pointkey == "PLOT_A_02"] == 0L &&
      pt$richness[pt$pointkey == "PLOT_A_03"] == 0L,
      "supported empty/flyover/coarse points map to zero richness")
check(is.na(pt$richness[pt$pointkey == "PLOT_A_05"]) &&
      is.na(pt$per_visit[pt$pointkey == "PLOT_A_05"]),
      "held-only point remains unavailable on the map")
grid <- grid_species(obs, "PLOT_A")
check(setequal(grid$scientificName, c("Alpha avis", "Beta avis", "Gamma avis")),
      "grid species list excludes flyover and coarse records")
mix <- method_mix(obs, "Alpha avis")
compound <- mix[mix$detectionMethod == "calling and singing", , drop = FALSE]
check(nrow(compound) == 1L && compound$method_calling && compound$method_singing,
      "method mix preserves compound components")
alpha_qc <- bird_qc(obs, "Alpha avis", points)
check(all(c("zero", "visualfar") %in% names(alpha_qc$sets)),
      "QC catches exact-zero and compound-visual long-distance records")
gamma_qc <- bird_qc(obs, "Gamma avis", points)
check("missing" %in% names(gamma_qc$sets) && !"methodmissing" %in% names(gamma_qc$sets),
      "QC keeps distance provenance while unknown methods fail before metrics")

# Every emitted column is documented and every keep-vector column is generated.
all_held_rows <- rbind(protocol_rows, unknown_method_row)
detail <- species_detection_export(obs, all_held_rows, "Alpha avis")
check(!length(setdiff(SPCSV_KEEP, names(detail))), "species export generates every declared column")
held_detail <- detail[!is.na(detail$hold_reason), , drop = FALSE]
check(nrow(held_detail) == 4L &&
      setequal(held_detail$hold_reason,
               c("incidental_outside_point_count", "missing_point_count_minute",
                 "invalid_point_count_minute", "missing_or_unknown_detection_method")) &&
      !any(held_detail$enters_incidence),
      "species export retains protocol/method held rows and explicit reasons")
held_only_detail <- species_detection_export(obs[0, , drop = FALSE], protocol_rows, "Alpha avis")
check(nrow(held_only_detail) == 3L && all(!held_only_detail$enters_incidence),
      "species export supports a held-only detection ledger without inventing metrics")
bad_held <- protocol_rows; bad_held$hold_reason[1] <- "mystery"
expect_error(bird_prepare_held(bad_held), "unrecognized", "unknown held reason fails closed")
check(!length(setdiff(BOARD_KEEP, names(board))), "Board export generates every declared column")
qc_report <- bird_qc_report(obs, "Alpha avis", points)
check(!length(setdiff(QC_KEEP, names(qc_report))), "QC export generates every declared column")
grid_export <- data.frame(
  scientificName = grid$scientificName, vernacularName = grid$vernacular,
  eligible_birds = grid$birds, detections = grid$detections, primary_method = grid$method)
check(!length(setdiff(GRIDCSV_KEEP, names(grid_export))), "grid export generates every declared column")
audit_obs <- obs
audit_held <- all_held_rows
for (table_name in c("audit_obs", "audit_held")) {
  table <- get(table_name)
  table$site <- "TEST"
  table$survey_id <- paste0("survey-audit-", seq_len(nrow(table)))
  table$occasion_id <- paste(table$site, table$pointkey, table$year, sep = "|")
  table$pointID <- sub("^.*_", "", table$pointkey)
  assign(table_name, table)
}
site_audit <- site_detection_audit_export(audit_obs, audit_held)
check(identical(names(site_audit), SITE_AUDIT_KEEP) &&
      nrow(site_audit) == nrow(obs) + nrow(all_held_rows),
      "site-wide audit export conserves every obs and held detection row")
check(any(site_audit$reportedScientificName == "Flyus only" & site_audit$is_flyover) &&
      any(site_audit$reportedScientificName == "Corvus sp." &
            site_audit$community_unit_state == "not_species_level") &&
      sum(!is.na(site_audit$hold_reason)) == 4L &&
      any(site_audit$hold_reason == "missing_or_unknown_detection_method") &&
      !any(site_audit$hold_reason %in% "invalid_cluster_size"),
      "site-wide audit reaches flyover-only, coarse-only, protocol- and method-held detections; no row is held for its count")
# A held count names why it is not a valid six-minute count. Since the
# incidence/index split (audit item 16) a missing clusterSize never withdraws a
# count, so the retired withdrawal reason must fail closed.
held_count_visits <- data.frame(
  survey_id = c("h-1", "h-2"), pointkey = c("PUUM_001_21", "PUUM_001_22"), year = 2021L,
  bout = "1", samplingImpractical = c("OK", "high wind"), valid_count = c(TRUE, FALSE),
  count_hold_reason = c(NA, "sampling_impractical"),
  eligible_detection_rows = c(1L, 0L), eligible_birds = c(0, 0),
  eligible_species = c(1L, 0L), flyover_rows = 0L, flyover_birds = 0,
  support_state = c("supported", "held"), outcome = c("positive", "unavailable"),
  stringsAsFactors = FALSE)
validated_held_counts <- bird_validate_visits(held_count_visits)
check(identical(validated_held_counts$support_state, c("supported", "held")),
      "an impractical count is held and named; a valid count carries no hold reason")
withdraw_one <- function(d, reason) {
  d$valid_count[1] <- FALSE; d$support_state[1] <- "held"; d$outcome[1] <- "unavailable"
  d$eligible_detection_rows[1] <- 0L; d$eligible_species[1] <- 0L; d$count_hold_reason[1] <- reason
  d
}
expect_error(bird_validate_visits(withdraw_one(held_count_visits, "detections_without_cluster_size")),
             "unrecognized count_hold_reason",
             "the retired cluster withdrawal cannot reappear")
expect_error(bird_validate_visits(withdraw_one(held_count_visits, "sampling_impractical")),
             "disagrees with samplingImpractical",
             "a protocol-OK count cannot be mislabelled as sampling-impractical")
relabelled <- held_count_visits; relabelled$count_hold_reason[1] <- "sampling_impractical"
expect_error(bird_validate_visits(relabelled), "count_hold_reason disagrees with valid_count",
             "a valid count cannot carry a hold reason")

# Incidence/index split (audit item 16): a detection without a usable count
# still counts toward incidence, but every birds-per-count figure in its scope
# is NA, never zero.
split_visits <- data.frame(
  survey_id = c("s-1", "s-2", "s-3"),
  pointkey = c("SPLT_001_21", "SPLT_001_22", "SPLT_001_23"),
  year = 2021L, bout = "1", valid_count = TRUE, stringsAsFactors = FALSE)
split_obs_template <- data.frame(
  survey_id = c("s-1", "s-2", "s-2", "s-3"),
  pointkey = c("SPLT_001_21", "SPLT_001_22", "SPLT_001_22", "SPLT_001_23"),
  plotID = "SPLT_001", year = 2021L, bout = "1",
  scientificName = c("Alpha avis", "Alpha avis", "Beta avis", "Beta avis"),
  vernacularName = c("Alpha bird", "Alpha bird", "Beta bird", "Beta bird"),
  taxonRank = "species", clusterSize = c(NA, 2, 3, 1), detectionMethod = "singing",
  pointCountMinute = "2", observerDistance = "30", stringsAsFactors = FALSE)
split_obs <- split_obs_template
split_visits <- add_visit_summaries(split_visits, split_obs)
check(identical(split_visits$outcome, rep("positive", 3L)) &&
      identical(split_visits$eligible_birds, c(0, 5, 1)),
      "a count whose only detection lacks a count is positive, with zero counted birds recorded")
split_prepared <- bird_prepare_obs(split_obs)
check(identical(split_prepared$enters_incidence, rep(TRUE, 4L)) &&
      identical(split_prepared$enters_index, c(FALSE, TRUE, TRUE, TRUE)),
      "a detection without a count enters incidence but not the detection index")
split_inc <- bird_incidence_support(split_obs, split_visits)
check(split_inc$T == 3L && setequal(split_inc$species, c("Alpha avis", "Beta avis")) &&
      sum(split_inc$Y) == 4L,
      "incidence, and so every count is positive, uses the detection without a count")
split_board <- species_board(split_obs, nvis = 3, visits = split_visits)
split_alpha <- split_board[split_board$scientificName == "Alpha avis", , drop = FALSE]
split_beta <- split_board[split_board$scientificName == "Beta avis", , drop = FALSE]
check(nrow(split_board) == 2L && split_alpha$detections == 2L &&
      split_alpha$detections_without_count == 1L && is.na(split_alpha$index) &&
      is.na(split_alpha$index_birds) && is.na(split_alpha$total_birds) &&
      is.na(split_alpha$mean_cluster) && split_alpha$detection_frequency > 0,
      "a species with a detection lacking a count keeps incidence but its bird figures are NA")
check(split_beta$detections_without_count == 0L &&
      isTRUE(all.equal(split_beta$index, round(4 / 3, 3))),
      "a species whose detections all carry counts keeps its detection index")
split_site <- site_birds(split_obs, nvis = 3, visits = split_visits)
check(split_site$n_species == 2L && is.na(split_site$birds_per_count) &&
      split_site$n_detections_without_count == 1L && is.na(split_site$top),
      "the site index is NA with the number of detections lacking a count stated")
split_year <- detection_by_year(split_obs, "Alpha avis")
check(nrow(split_year) == 1L && split_year$detections == 2L && is.na(split_year$birds),
      "a species-year with a detection lacking a count reports NA birds")
split_grid <- grid_species(split_obs, "SPLT_001")
check(is.na(split_grid$birds[split_grid$scientificName == "Alpha avis"]) &&
      split_grid$birds[split_grid$scientificName == "Beta avis"] == 4,
      "grid bird totals are NA only for the species with a missing count")
split_opportunity <- data.frame(
  pointkey = split_visits$pointkey, year = 2021L, n_valid_bouts = 1L, n_held_bouts = 0L,
  n_bouts_recorded = 1L, supported = TRUE, outcome = "positive", stringsAsFactors = FALSE)
split_annual <- annual_bird_summary(split_obs, split_opportunity, split_visits)
check(nrow(split_annual) == 1L && is.na(split_annual$eligible_birds) &&
      is.na(split_annual$birds_per_count) && split_annual$eligible_species == 2L &&
      split_annual$eligible_detection_rows == 4L && split_annual$outcome == "positive",
      "annual birds are NA when a detection lacks a count, while incidence still counts it")
split_beta_year <- detection_by_year(split_obs, "Beta avis", split_opportunity)
split_alpha_year <- detection_by_year(split_obs, "Alpha avis", split_opportunity)
check(split_beta_year$birds == 4 && isTRUE(all.equal(split_beta_year$birds_per_count, 4 / 3)) &&
      is.na(split_alpha_year$birds_per_count) && split_alpha_year$detected,
      "species-year birds per count is NA only for the species with a missing count")
split_points <- data.frame(pointkey = split_visits$pointkey, plotID = "SPLT_001",
                           n_visits = 1L, stringsAsFactors = FALSE)
split_point_summary <- point_summary(split_obs, split_points, split_opportunity)
missing_point <- split_point_summary[split_point_summary$pointkey == "SPLT_001_21", ]
counted_point <- split_point_summary[split_point_summary$pointkey == "SPLT_001_23", ]
check(is.na(missing_point$birds) && is.na(missing_point$per_visit) &&
      missing_point$richness == 1L && counted_point$birds == 1,
      "a point whose only detection lacks a count reports NA birds, not zero")
qc_split <- split_obs
qc_split$observerDistance[1] <- "0"
qc_split_flags <- bird_qc(qc_split, "Alpha avis")
qc_split_keys <- vapply(qc_split_flags$flags, function(f) f$key, character(1))
check("zero" %in% qc_split_keys && any(is.na(qc_split_flags$sets$zero$clusterSize)),
      "distance QC still reviews a detection that lacks a count")

check(identical(bird_birds_or_na(c(1, 2), c(TRUE, TRUE)), 3) &&
      is.na(bird_birds_or_na(c(NA, 2), c(FALSE, TRUE))) &&
      identical(bird_birds_or_na(numeric(), logical()), 0),
      "bird totals are NA whenever a count in scope is missing")

# Cluster-size states (wren's ruling, 2026-10-21): a blank count on a bird
# record is read as one bird only in a listed site-year with the recording
# signature; invalid present values are never read as one.
cs_state <- bird_cluster_size_state(
  raw_token = c("3", " 4 ", "2.0", "0", "-1", "1.5", "abc", NA, "", NA, NA),
  names_bird = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
  site = c(rep("PUUM", 9), "PUUM", "SRER"),
  year = c(rep(2021L, 9), 2019L, 2021L))
check(identical(cs_state, c("recorded", "recorded", "recorded", "invalid", "invalid", "invalid",
                            "invalid", "blank_no_bird", "blank_read_as_one",
                            "blank_unresolved", "blank_unresolved")),
      "cluster-size states separate recorded, invalid, no-bird, read-as-one and unresolved")
check(identical(bird_cluster_size_used(c("3", "", "0", NA), c("recorded", "blank_read_as_one", "invalid", "blank_unresolved")),
                c(3, 1, NA, NA)),
      "the used count is the recorded value, one for a read blank, and NA otherwise")
puum_obs <- split_obs_template
puum_obs$site <- "PUUM"; puum_obs$pointkey <- sub("SPLT", "PUUM", puum_obs$pointkey)
puum_obs$plotID <- "PUUM_001"
puum_prepared <- bird_prepare_obs(puum_obs)
check(puum_prepared$cluster_size_state[1] == "blank_read_as_one" && puum_prepared$clusterSize[1] == 1 &&
      all(puum_prepared$enters_index),
      "in a listed PUUM year a blank bird count is read as one and enters the index")
puum_tampered <- puum_prepared; puum_tampered$cluster_size_state[1] <- "recorded"
expect_error(bird_prepare_obs(puum_tampered), "cluster_size_state does not re-derive",
             "a forged cluster-size state fails closed")
puum_visits <- data.frame(survey_id = c("s-1", "s-2", "s-3"),
  pointkey = c("PUUM_001_21", "PUUM_001_22", "PUUM_001_23"), year = 2021L, bout = "1",
  valid_count = TRUE, stringsAsFactors = FALSE)
puum_visits <- add_visit_summaries(puum_visits, puum_obs)
puum_site <- site_birds(puum_obs, nvis = 3, visits = puum_visits)
check(puum_site$n_detections_count_read_as_one == 1L && puum_site$n_detections_without_count == 0L &&
      isTRUE(all.equal(puum_site$birds_per_count, round(7 / 3, 2))) &&
      identical(puum_site$read_as_one_years, 2021L) && puum_site$index_birds == 7,
      "a read-as-one count enters the PUUM index and is stated beside it")
puum_board <- species_board(puum_obs, nvis = 3, visits = puum_visits)
check(puum_board$detections_count_read_as_one[puum_board$scientificName == "Alpha avis"] == 1L &&
      puum_board$detections_count_read_as_one[puum_board$scientificName == "Beta avis"] == 0L,
      "the Board states each species' detections read as one")
check(grepl("(1 of 7 birds)", bird_cluster_read_caveat("PUUM", c(2018L, 2021L, 2024L), 1L, 7), fixed = TRUE) &&
      grepl("2018, 2021 and 2024", bird_cluster_read_caveat("PUUM", c(2024L, 2018L, 2021L), 881L, 1137), fixed = TRUE) &&
      grepl("881 of 1,137 birds", bird_cluster_read_caveat("PUUM", 2021L, 881L, 1137), fixed = TRUE) &&
      is.null(bird_cluster_read_caveat("SRER", integer(), 0L, 10)),
      "the read-as-one caveat states years and the share of birds, and only where it applies")

# Accumulation verdict (wren's ruling): Q1/T bands and reachability.
check(bird_accumulation_verdict(25, 100)$band == "still rising" && bird_accumulation_verdict(25, 100)$per_n == 4L &&
      bird_accumulation_verdict(4, 100)$band == "still rising" &&
      bird_accumulation_verdict(1, 100)$band == "levelling off" &&
      bird_accumulation_verdict(1, 200)$band == "nearly flat" &&
      bird_accumulation_verdict(0, 90)$band == "flat" &&
      grepl("flat: every detected species", bird_accumulation_verdict(0, 90)$text, fixed = TRUE),
      "accumulation verdict bands are reachable and match the rule")

# Spearman interval and exact permutation p (wren's ruling).
ci45 <- bird_spearman_ci(0.351, 45)
check(isTRUE(all.equal(round(ci45, 2), c(0.05, 0.59))) && all(is.na(bird_spearman_ci(0.5, 9))),
      "Bonett-Wright interval matches the ruling and is withheld below n = 10")
check(isTRUE(all.equal(bird_spearman_exact_p(1:7, 1:7), 2 / factorial(7))) &&
      isTRUE(all.equal(bird_spearman_exact_p(1:6, c(2, 1, 4, 3, 6, 5)),
                       suppressWarnings(stats::cor.test(1:6, c(2, 1, 4, 3, 6, 5), method = "spearman", exact = TRUE)$p.value))) &&
      is.na(bird_spearman_exact_p(1:10, 1:10)),
      "exact permutation p matches enumeration and R's exact test, and is limited to n <= 9")

# Chao2 stability gate (wren's ruling): extrapolation and upper-bound gates.
gate_extrap <- chao2_from_incidence(c(rep(1L, 20), rep(2L, 3), rep(5L, 10)), 100)
check(gate_extrap$unstable && grepl("more than half", gate_extrap$suppressed_reason, fixed = TRUE),
      "a Chao2 estimate that adds more than half the observed richness does not lead")
gate_ok <- chao2_from_incidence(c(rep(1L, 2), rep(2L, 5), rep(8L, 30)), 100)
check(!gate_ok$unstable && is.na(gate_ok$suppressed_reason),
      "a well-supported Chao2 estimate still leads")

# Ubiquity after the 2018 grid cut (wren's ruling): points last counted before
# 2018 leave the denominator.
ubq_visits <- data.frame(survey_id = c("u-1", "u-2", "u-3"),
  pointkey = c("UBQ_001_21", "UBQ_001_22", "UBQ_001_23"), year = c(2017L, 2019L, 2019L),
  bout = "1", valid_count = TRUE, stringsAsFactors = FALSE)
ubq_obs <- data.frame(survey_id = c("u-1", "u-2"), pointkey = c("UBQ_001_21", "UBQ_001_22"),
  plotID = "UBQ_001", year = c(2017L, 2019L), bout = "1", scientificName = c("Alpha avis", "Beta avis"),
  vernacularName = c("Alpha bird", "Beta bird"), taxonRank = "species", clusterSize = 1,
  detectionMethod = "singing", pointCountMinute = "2", observerDistance = "30", stringsAsFactors = FALSE)
ubq_visits <- add_visit_summaries(ubq_visits, ubq_obs)
ubq_board <- species_board(ubq_obs, nvis = 3, visits = ubq_visits)
check(ubq_board$ubiquity[ubq_board$scientificName == "Alpha avis"] == 0 &&
      ubq_board$ubiquity[ubq_board$scientificName == "Beta avis"] == 50 &&
      all(ubq_board$ubiquity_points == 2L),
      "ubiquity counts only points sampled since 2018")
ubq_notes <- bird_site_design_notes("DELA", ubq_visits)
check(any(grepl("1 point here was last counted before 2018", ubq_notes, fixed = TRUE)) &&
      any(grepl("closed this site at the end of 2025", ubq_notes, fixed = TRUE)) &&
      !any(grepl("before 2018", bird_site_design_notes("SRER", ubq_visits[2:3, ]), fixed = TRUE)),
      "site design notes report the grid cut only where points were dropped")

# Audit 2026-10-18: both robust-scale QC flags were unreachable because MAD = 0
# on integer-clumped data. These fixtures mirror the measured shapes so neither
# flag can silently die again.
qc_row <- function(n, pointkey, cluster, name = "Flockus gregarius") data.frame(
  survey_id = paste0("qc-", pointkey, "-", seq_len(n)), pointkey = pointkey,
  plotID = sub("_[^_]*$", "", pointkey), year = 2022L, bout = "1",
  scientificName = name, taxonRank = "species", vernacularName = "Test flocker",
  clusterSize = cluster, detectionMethod = "calling", pointCountMinute = "2",
  observerDistance = "40", stringsAsFactors = FALSE)
flock_obs <- rbind(qc_row(30, "QCPL_001_21", rep(1, 30)), qc_row(1, "QCPL_001_22", 200),
                   qc_row(4, "QCPL_001_23", rep(2, 4)))
flock_qc <- bird_qc(flock_obs, "Flockus gregarius")
flock_keys <- vapply(flock_qc$flags, function(f) f$key, character(1))
check("cluster" %in% flock_keys && nrow(flock_qc$sets$cluster) == 1L &&
        flock_qc$sets$cluster$clusterSize == 200,
      "floored flock flag fires on a 200-bird flock of a singleton-counted species and not on pairs")
srer_points <- data.frame(
  pointkey = c(sprintf("SRER_018_%02d", 1:9), sprintf("SRER_020_%02d", 1:9),
               sprintf("SRER_%03d_%02d", rep(1:10, each = 9), rep(1:9, 10))),
  n_visits = c(rep(1, 18), rep(8, 90)), stringsAsFactors = FALSE)
check(stats::mad(srer_points$n_visits) == 0, "SRER-shaped point effort has MAD = 0")
effort_obs <- rbind(qc_row(3, "SRER_018_01", rep(1, 3), "Campylorhynchus brunneicapillus"),
                    qc_row(9, "SRER_001_01", rep(1, 9), "Campylorhynchus brunneicapillus"))
effort_qc <- bird_qc(effort_obs, "Campylorhynchus brunneicapillus", srer_points)
effort_keys <- vapply(effort_qc$flags, function(f) f$key, character(1))
check("loweffort" %in% effort_keys && nrow(effort_qc$sets$loweffort) == 3L &&
        all(effort_qc$sets$loweffort$pointkey == "SRER_018_01"),
      "floored under-effort flag fires on one-visit points against a site median of 8")
even_points <- data.frame(pointkey = sprintf("EVEN_001_%02d", 1:9), n_visits = 8,
                          stringsAsFactors = FALSE)
even_qc <- bird_qc(qc_row(5, "EVEN_001_01", rep(1, 5), "Campylorhynchus brunneicapillus"),
                   "Campylorhynchus brunneicapillus", even_points)
check(!"loweffort" %in% vapply(even_qc$flags, function(f) f$key, character(1)),
      "under-effort flag stays silent when every point has equal effort")

codebook <- bird_codebook()
declared <- unique(c(SPCSV_KEEP, BOARD_KEEP, GRADIENT_KEEP, GRIDCSV_KEEP, REPORT_KEEP,
                     SITE_AUDIT_KEEP, QC_KEEP,
                     "S_obs", "chao2", "coverage"))
check(setequal(codebook$column[codebook$column != "_source"], declared),
      "codebook and all export keep-vectors have exact parity")
check(!anyDuplicated(codebook$column), "codebook has one row per emitted column")

cat(sprintf("OK: %d fail-closed bird science helper checks passed.\n", n_checks))

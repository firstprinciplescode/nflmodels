# ============================================================
# RECEIVING -- CURRENCY THREE, ROUND ONE PORT
# Written 2026-08-18 on Andy's go: "RECEIVING STUFF ... IT WAS
# JUST GRADE AND YPRR? AND IDEALLY WE SEPARATE ... POSITION /
# MAN OR ZONE."
#
# RULINGS CARRIED (the signed round-one template,
# league_pass_rush_evaluating_currency_three.R):
#   - RANK SPACE, not residuals. Blowout compression is a feature.
#   - NO NEW STATISTICS: the only new columns are within-opponent
#     percent_rank_avg ranks (n()==1 -> 0.5 singleton guard) of
#     grade / yprr columns that already exist in the feed.
#   - MEDIAN season aggregation.
#   - FIREWALL: currency three NEVER feeds any SoS slate.
#
# RECEIVING LAW CARRIED (canon Phase 5,
# new_england_opp_receiving_schedule.R, stamped 2026-08-15/16):
#   - BANDS: the six alignment clusters ARE the bands, NO MERGES
#     EVER (Andy 08-15, twice): WWR / WSWR / SWR / ITE / STE / RB.
#     Band identity = canon's rec_band_season (align_cluster_name,
#     one per player-season by routes slice_max). Identity vs
#     currency are two different questions; the modal map is
#     identity, the gates below are currency entry.
#   - CURRENCY METRICS: grade + YPRR anchor. tgt_rate pctls are
#     context only; ypa_oe demoted; OE is overall-only by
#     construction and cannot enter a per-split c3 (do not invent
#     man_pbp_xypa). So c3 ranks TWO canon columns per split:
#     split grade and split yprr.
#   - SPLITS: man / zone, priced separately, NEVER recombined.
#     The neither-bucket (routes with no man/zone label) lives
#     only in the Phase 5 combined currency; its share prints as
#     a receipt below.
#   - SEASON GATE: canon rec_qual (X_QUAL = 8 overall routes in a
#     game, G_MIN = 6 qualifying games), IMPORTED not re-derived.
#   - SPLIT SEASON FLOOR: a split pctl exists only where season
#     split routes >= SPLIT_RTE_MIN = 50 (canon man_ok/zone_ok
#     law, ported one-for-one).
#   - POOLS: percentiles rank within (band, split) over a ROLLING
#     3-SEASON window, POOL_W = 3 (stamped 08-15: single-season
#     SWR=27/ITE=33 broke the floor). c3 inherits the receiving
#     pool law; the window-roll caveat inherits with it.
#   - OPPONENT KEY: receiving is an OFFENSIVE unit, so game ranks
#     are within def_ssn (the DEFENSE faced), per the opponent-key
#     symmetry law. "Your day vs every same-band receiver who
#     faced the same defense that season."
#
# PROPOSED -- Kimi, UNSIGNED (two receiving-specific adaptations;
#   everything else is the signed template verbatim):
#   1. X_SPLIT_RTE = 5: a game enters a split's pool only with
#      >= 5 routes IN THAT SPLIT. Coverage canon stamps split snap
#      floors (X_MAN=6 / X_ZONE=13); receiving canon stamps only
#      the overall X_QUAL=8 game gate. This is the coverage-mirror
#      floor, unsigned. A sensitivity grid (floors 3/5/8) prints
#      so the ruling lands on receipts.
#   2. RAW FEED: the weekly canon base (receiving_func_base) has
#      no per-split grades -- "documented asymmetry" (canon
#      header). Game-level split grades/routes/yards are sourced
#      by LOCAL ARCHAEOLOGY ONLY (Andy 08-18, twice: ZERO Athena
#      in this file, ever -- the data is static): in-session
#      objects first (vw_receiving_enriched has been sitting in
#      the session, full SELECT *, since the build), then local
#      .rds caches; every attempt prints its resolution or its
#      failure reasons. If nothing local resolves, the file stops
#      loud with a one-time hand-run fill snippet for the ONE
#      table nobody has ever pulled (game-grain scheme); Andy
#      runs that by hand when logged in, and the file never
#      dials out itself. Whatever feed wins is joined onto
#      receiving_func_base (canon def_ssn + overall routes) by
#      player x season x week, game_id fallback.
#
# SOURCE ORDER: source new_england_opp_receiving_schedule.R FIRST
#   (needs its session objects: receiving_func_base, rec_name,
#   rec_band_season, rec_qual, rec_season_pctl_sos, and the
#   stamped constants), then this file, top to bottom. Soft joins
#   (skipped silently if absent): rec_season_pctl_sos is the c1
#   comparison; no league object is touched (c3 never feeds one).
#   Re-source WHOLE file after any edit. ASCII only.
# ============================================================

needed_c3r <- c("receiving_func_base", "rec_name", "rec_band_season",
                "rec_qual", "rec_season_pctl_sos",
                "X_QUAL", "G_MIN", "POOL_W", "REC_BANDS",
                "SPLIT_RTE_MIN", "run_athena_query")
missing_c3r <- needed_c3r[!vapply(needed_c3r, exists, logical(1))]
if (length(missing_c3r)) stop("missing session objects: ",
                              paste(missing_c3r, collapse = ", "),
                              " -- source new_england_opp_receiving_schedule.R first")

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

library(dplyr)

receiving_func_base <- tibble::as_tibble(receiving_func_base)
rec_name            <- tibble::as_tibble(rec_name)
rec_band_season     <- tibble::as_tibble(rec_band_season)
rec_qual            <- tibble::as_tibble(rec_qual)

# band-identity wall: canon's identity map carries exactly the six
# stamped bands, NO MERGES EVER (Andy 08-15, twice)
stopifnot(all(rec_band_season$band %in% REC_BANDS))

# ------------------------------------------------------------
# 0. FEED -- game-grain split grades. ZERO ATHENA IN THIS FILE,
#    EVER (Andy 08-18, twice). The data is static and the
#    receiving workspace was built from pulls already paid for,
#    so the feed is LOCAL ARCHAEOLOGY ONLY -- three rungs, every
#    rung printing receipts, no network anything:
#      rung 1: receiving_scheme_c3 already resolved this session
#      rung 2: DECLARED in-session objects from the receiving /
#              exploration workspaces, tried in order
#      rung 3: local .rds caches on disk (getwd + home)
#    Season-grain frames (receiver_scheme, receiver_scheme_final)
#    FAIL the week/game_id slot on purpose -- c3 ranks GAMES
#    within opponent; a season agg cannot feed it, and the
#    receipt says so to your face.
#    If nothing resolves: LOUD STOP with the exact shopping list
#    below. The one thing that has never been pulled is the
#    game-grain scheme table; ONE hand-run pull when you are
#    logged in, one cache file, and this file never dials out.
# ------------------------------------------------------------

# ONE-TIME FILL, BY HAND, WHEN LOGGED IN -- commented out, never
# executes inside this file:
#   rs_once <- run_athena_query("SELECT * FROM nfl_data.receiver_scheme")
#   saveRDS(rs_once, "receiver_scheme_cache_c3.rds")
# (if receiver_scheme itself errors, try vw_receiving_enriched_scheme
#  or stg_pff__receiver_scheme under the same cache name)

FEED_OBJECTS <- c("vw_receiving_enriched",      # SELECT * weekly pull,
                  # in session since the build
                  "vw_receiving_enriched_scheme",
                  "receiver_scheme",            # exploration pull (season
                  # agg -- fails week, receipt)
                  "receiver_scheme_final",      # canon season frame (same)
                  "receiving_depth_weekly")
FEED_CACHE_RE <- "receiv.*(scheme|cache).*\\.rds$|receiver_scheme.*\\.rds$"

# non-fatal resolver: exact prefs, then a single fuzzy hit, else NA
try_resolve <- function(df, prefs, fuzz) {
  hit <- prefs[prefs %in% names(df)][1]
  if (!is.na(hit)) return(hit)
  fz <- grep(fuzz, names(df), ignore.case = TRUE, value = TRUE)
  if (length(fz) == 1) return(fz)
  NA_character_
}

attempt_card <- function(df) {
  list(jkey    = try_resolve(df, c("week"), "^week$"),
       gid     = try_resolve(df, c("game_id"), "^game_id$"),
       m_grade = try_resolve(df, c("weighted_avg_man_grade",
                                   "man_grades_pass_route",
                                   "man_grade", "grades_man"),
                             "man.*grade|grade.*man"),
       m_routes = try_resolve(df, c("man_routes",
                                    "man_snap_counts_routes"),
                              "man.*route"),
       m_yards = try_resolve(df, c("man_yards", "man_receiving_yards"),
                             "man.*yard"),
       z_grade = try_resolve(df, c("weighted_avg_zone_grade",
                                   "zone_grades_pass_route",
                                   "zone_grade", "grades_zone"),
                             "zone.*grade|grade.*zone"),
       z_routes = try_resolve(df, c("zone_routes",
                                    "zone_snap_counts_routes"),
                              "zone.*route"),
       z_yards = try_resolve(df, c("zone_yards", "zone_receiving_yards"),
                             "zone.*yard"))
}

card_fails <- function(cc, df) {
  c(if (!all(c("player_id", "season") %in% names(df))) "player_id/season",
    if (is.na(cc$jkey) && is.na(cc$gid)) "week/game_id",
    if (is.na(cc$m_grade))  "man grade",
    if (is.na(cc$m_routes)) "man routes",
    if (is.na(cc$m_yards))  "man yards",
    if (is.na(cc$z_grade))  "zone grade",
    if (is.na(cc$z_routes)) "zone routes",
    if (is.na(cc$z_yards))  "zone yards")
}

card_ok <- function(cc, df) length(card_fails(cc, df)) == 0

if (exists("receiving_scheme_c3") && exists("REC_CARD_C3")) {
  cat("feed rung 1: in-session receiving_scheme_c3 (already resolved)\n")
  rec_scheme <- tibble::as_tibble(receiving_scheme_c3)
  card     <- REC_CARD_C3
  tbl_used <- REC_TBL_C3
} else {
  rec_scheme <- NULL
  card <- NULL
  tbl_used <- NULL
  col_menus <- list()
  
  cat("--- feed rung 2: in-session objects (zero network) ---\n")
  for (ob in FEED_OBJECTS) {
    if (!exists(ob)) {
      cat(sprintf("  %-28s not in session\n", ob)); next
    }
    cand <- tibble::as_tibble(get(ob))
    col_menus[[paste0("object:", ob)]] <- names(cand)
    cc <- attempt_card(cand)
    if (card_ok(cc, cand)) {
      rec_scheme <- cand
      card <- cc
      tbl_used <- paste0("session:", ob)
      cat(sprintf("  %-28s RESOLVED\n", ob))
      break
    }
    cat(sprintf("  %-28s fails: %s\n", ob,
                paste(card_fails(cc, cand), collapse = ", ")))
  }
  
  if (is.null(rec_scheme)) {
    cat("--- feed rung 3: local .rds caches, getwd + home (zero network) ---\n")
    cache_paths <- unique(c(
      list.files(getwd(), pattern = FEED_CACHE_RE,
                 full.names = TRUE, ignore.case = TRUE),
      list.files(path.expand("~"), pattern = FEED_CACHE_RE,
                 full.names = TRUE, ignore.case = TRUE)))
    if (!length(cache_paths)) cat("  no receiving-ish .rds caches found\n")
    for (cf in cache_paths) {
      cand <- tryCatch(tibble::as_tibble(readRDS(cf)),
                       error = function(e) NULL)
      if (is.null(cand)) {
        cat(sprintf("  %-50s unreadable\n", basename(cf))); next
      }
      col_menus[[paste0("cache:", cf)]] <- names(cand)
      cc <- attempt_card(cand)
      if (card_ok(cc, cand)) {
        rec_scheme <- cand
        card <- cc
        tbl_used <- paste0("cache:", cf)
        cat(sprintf("  %-50s RESOLVED\n", basename(cf)))
        break
      }
      cat(sprintf("  %-50s fails: %s\n", basename(cf),
                  paste(card_fails(cc, cand), collapse = ", ")))
    }
  }
  
  if (is.null(rec_scheme)) {
    cat("\nNO LOCAL FEED RESOLVED. The one thing nobody has ever\n")
    cat("pulled is the game-grain receiving scheme table. ONE hand-run\n")
    cat("pull when you are logged in (at the top of this file, commented):\n\n")
    cat('  rs_once <- tryCatch(run_athena_query("SELECT * FROM nfl_data.receiver_scheme"),\n')
    cat('                      error = function(e) { cat(e$message, "\\n"); NULL })\n')
    cat('  if (is.null(rs_once)) rs_once <- run_athena_query("SELECT * FROM nfl_data.vw_receiving_enriched_scheme")\n')
    cat('  saveRDS(rs_once, "receiver_scheme_cache_c3.rds")\n\n')
    cat("then re-source -- rung 3 picks it up and this file NEVER\n")
    cat("dials out itself. Column menus of everything tried:\n")
    for (nm in names(col_menus)) {
      cat("--", nm, ":\n"); print(col_menus[[nm]])
    }
    stop("receiving c3 feed: no local game-grain split feed -- ",
         "hand-run the one-time fill at the top of the file")
  }
  receiving_scheme_c3 <- rec_scheme
  REC_CARD_C3 <- card
  REC_TBL_C3  <- tbl_used
}

RM <- c(grade = card$m_grade, routes = card$m_routes, yards = card$m_yards)
RZ <- c(grade = card$z_grade, routes = card$z_routes, yards = card$z_yards)
JOIN_KEY <- if (!is.na(card$jkey)) "week" else "game_id"
KEY_RAW  <- if (JOIN_KEY == "week") card$jkey else card$gid

cat("--- c3 receiving feed resolution (live) ---\n")
cat("table:", tbl_used, "| join key:", JOIN_KEY, "\n")
cat("man  grade:", unname(RM["grade"]), "| man  routes:", unname(RM["routes"]),
    "| man  yards:", unname(RM["yards"]), "\n")
cat("zone grade:", unname(RZ["grade"]), "| zone routes:", unname(RZ["routes"]),
    "| zone yards:", unname(RZ["yards"]), "\n")

# ------------------------------------------------------------
# 0b. GAME FRAME -- the grain is player x GAME. TEAM is in the
#     join key on both sides (standard code patch), so a real
#     two-game week (mid-week team change) prices each game vs
#     the defense IT faced. The 7 dup player-weeks in the 2017-25
#     base proved to be SAME-TEAM identical pairs (first-run
#     receipt): canon build fan-out, collapsed losslessly below.
#     Canon base carries def_ssn + overall routes; the raw feed
#     carries the split columns.
# ------------------------------------------------------------

REC_TEAM_PATCH <- c("ARI" = "ARZ", "AZ" = "ARZ", "BAL" = "BLT",
                    "CLE" = "CLV", "HOU" = "HST", "LAR" = "LA",
                    "JAC" = "JAX", "WSH" = "WAS")
stopifnot("team" %in% names(rec_scheme))

scheme_keys <- rec_scheme %>%
  mutate(team_c = dplyr::coalesce(REC_TEAM_PATCH[team], team)) %>%
  # unname() is LOAD-BEARING: RM/RZ are NAMED character vectors and
  # all_of() on a named input lets the inner name eat the rename.
  select(player_id, season, jkey0 = KEY_RAW, team_c,
         man_grade = unname(RM["grade"]), man_routes = unname(RM["routes"]),
         man_yards = unname(RM["yards"]),
         zone_grade = unname(RZ["grade"]), zone_routes = unname(RZ["routes"]),
         zone_yards = unname(RZ["yards"])) %>%
  mutate(jkey = paste(jkey0, team_c, sep = "|"))

dup_raw <- scheme_keys %>% count(player_id, season, jkey) %>% filter(n > 1)
if (nrow(dup_raw)) {
  cat("RAW FAN-OUT: player-game keys with > 1 raw row (team in key):\n")
  print(dup_raw, n = 25)
  print(scheme_keys %>% inner_join(dup_raw %>% select(player_id, season, jkey),
                                   by = c("player_id", "season", "jkey")),
        n = 40)
  stop("raw scheme feed is not one-row-per-player-game -- see offenders")
}

gid_base <- if ("game_id" %in% names(receiving_func_base)) "game_id" else
  grep("^game_id", names(receiving_func_base), value = TRUE)[1]
if (JOIN_KEY == "game_id" && is.na(gid_base)) {
  stop("raw joins by game_id but receiving_func_base has no game_id ",
       "column -- report")
}

base_keys <- receiving_func_base %>%
  filter(!is.na(player_id)) %>%
  mutate(team_c = dplyr::coalesce(REC_TEAM_PATCH[posteam], posteam)) %>%
  select(player_id, season, week, posteam, def_ssn, routes, team_c)
if (JOIN_KEY == "week") base_keys$jkey0 <- base_keys$week else
  base_keys$jkey0 <- base_keys[[gid_base]]
base_keys <- base_keys %>% mutate(jkey = paste(jkey0, team_c, sep = "|"))

dup_base <- base_keys %>% count(player_id, season, jkey) %>% filter(n > 1)
if (nrow(dup_base)) {
  # FIRST-RUN RECEIPT (2017-25): the 7 dup keys are SAME-TEAM,
  # byte-identical pairs -- not two-game weeks. That is the
  # cluster_join fan-out in the canon build (the Akers x4 family:
  # cluster_join is multi-row per player-team-season, joined raw in
  # receiving_stats_build_AWS.R). Collapse law: identical pairs on
  # every carried column -> distinct() is lossless for c3; same-team
  # pairs that DIFFER die loud; different-team pairs (a real two-game
  # week) never reach this wall because team is in the key.
  # CANON IMPACT, for Andy's ruling: rec_qual counted each of these
  # player-seasons TWICE toward G_MIN (all carried routes >= 8). The
  # source fix = dedupe cluster_join one-per-player-team-season in
  # the build file (the Phase 5 scheme_p5 law, applied upstream).
  cat("BASE DUP RECEIPT: doubled player-game rows in",
      "receiving_func_base (canon build fan-out):\n")
  chk <- base_keys %>%
    inner_join(dup_base %>% select(player_id, season, jkey),
               by = c("player_id", "season", "jkey"))
  print(chk %>% arrange(player_id, season, jkey), n = 40)
  non_ident <- chk %>% distinct() %>%
    count(player_id, season, jkey) %>% filter(n > 1)
  if (nrow(non_ident)) {
    cat("SAME-TEAM PAIRS THAT DIFFER on carried columns:\n")
    print(non_ident)
    stop("dup player-game rows differ -- report, do not collapse")
  }
  cat("all", nrow(dup_base), "pairs identical on carried columns --",
      "distinct() lossless; canon rec_qual counted these twice\n")
  base_keys <- base_keys %>% distinct()
}

rec_game_c3 <- base_keys %>%
  inner_join(scheme_keys, by = c("player_id", "season", "jkey"))

cat(sprintf("--- join yield (key = %s + team): base player-games with split grades attached, by season ---\n",
            JOIN_KEY))
print(base_keys %>% count(season, name = "base_rows") %>%
        left_join(rec_game_c3 %>% count(season, name = "matched"),
                  by = "season") %>%
        mutate(match_share = round(matched / base_rows, 3)), n = 12)

# unmatched diagnostic: base rows with real routes but no raw row.
# A systematic team-code break shows up here by name; scattered
# small-route misses are raw-feed gaps, priced nowhere, honest.
cat("--- unmatched base rows (routes > 0, no raw row), top 10 by routes ---\n")
print(base_keys %>% filter(routes > 0) %>%
        anti_join(scheme_keys, by = c("player_id", "season", "jkey")) %>%
        arrange(desc(routes)) %>%
        select(player_id, season, week, posteam, def_ssn, routes) %>%
        head(10))

# TWO-GAME-WEEK RECEIPT -- post-collapse, only REAL team changers
# (different teams, same week) can appear here; each game ranks vs
# the defense it faced. Expect 0 in 2017-25 (the 7 dups were
# same-team identicals, collapsed above). Nonzero is legitimate.
two_game <- rec_game_c3 %>% count(player_id, season, week) %>% filter(n > 1)
cat("--- real two-game weeks (different teams, same week):",
    nrow(two_game), "player-weeks ---\n")
print(rec_game_c3 %>% inner_join(two_game %>% select(player_id, season, week),
                                 by = c("player_id", "season", "week")) %>%
        left_join(rec_name, by = "player_id") %>%
        arrange(season, week, player_id) %>%
        select(player, season, week, posteam, def_ssn, routes,
               man_routes, zone_routes), n = 20)

# neither-bucket receipt: routes with no man/zone label live only
# in the Phase 5 combined currency, never here (split law)
cat("--- neither-share receipt: man+zone routes vs overall routes, by season ---\n")
print(rec_game_c3 %>%
        group_by(season) %>%
        summarise(routes = sum(routes, na.rm = TRUE),
                  man_routes = sum(man_routes, na.rm = TRUE),
                  zone_routes = sum(zone_routes, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(neither_share = round(1 - (man_routes + zone_routes) /
                                       pmax(routes, 1), 3)), n = 12)

# ------------------------------------------------------------
# 0c. FLOOR SENSITIVITY GRID -- PROPOSED X_SPLIT_RTE = 5, unsigned.
#     Receipts first, ruling after: player-games at floors 3/5/8.
# ------------------------------------------------------------

X_SPLIT_RTE <- 5   # PROPOSED -- Kimi, UNSIGNED (coverage-mirror split floor)

cat("--- floor grid (PROPOSED): split player-games surviving each floor ---\n")
print(bind_rows(
  rec_game_c3 %>% filter(!is.na(man_routes)) %>%
    summarise(split = "man", f3 = sum(man_routes >= 3),
              f5 = sum(man_routes >= 5), f8 = sum(man_routes >= 8)),
  rec_game_c3 %>% filter(!is.na(zone_routes)) %>%
    summarise(split = "zone", f3 = sum(zone_routes >= 3),
              f5 = sum(zone_routes >= 5), f8 = sum(zone_routes >= 8))))

# ------------------------------------------------------------
# 1. THE STANZA -- one split at a time. Template-faithful:
#    split floor -> band identity -> rank within def_ssn x band ->
#    median season -> canon season gate -> rolling band pools.
#    TWO canon columns ranked per game: split grade, split yprr.
# ------------------------------------------------------------

c3_one_split_rec <- function(sp) {
  G_COL   <- if (sp == "man") "man_grade"  else "zone_grade"
  RT_COL  <- if (sp == "man") "man_routes" else "zone_routes"
  YD_COL  <- if (sp == "man") "man_yards"  else "zone_yards"
  
  # THE NEW COLUMNS -- within-opponent game ranks, per split
  game <- rec_game_c3 %>%
    filter(.data[[RT_COL]] >= X_SPLIT_RTE, !is.na(.data[[G_COL]])) %>%
    mutate(sp_yprr = ifelse(.data[[RT_COL]] > 0,
                            .data[[YD_COL]] / .data[[RT_COL]], NA_real_)) %>%
    select(player_id, season, week, posteam, def_ssn, routes,
           sp_routes = RT_COL, sp_grade = G_COL, sp_yprr) %>%
    # band identity vs currency entry: canon modal band, joined at
    # GAME grain here only to build the pools (the pool key),
    # re-joined after the season summarise for the currency frame
    inner_join(rec_band_season, by = c("player_id", "season")) %>%
    group_by(def_ssn, band) %>%
    mutate(n_pool = dplyr::n(),
           vsopp_rank_grade = dplyr::case_when(
             dplyr::n() == 1 ~ 0.5,
             TRUE ~ percent_rank_avg(sp_grade)),
           vsopp_rank_yprr = dplyr::case_when(
             dplyr::n() == 1 ~ 0.5,
             TRUE ~ percent_rank_avg(sp_yprr))) %>%
    ungroup()
  
  # honesty: the split floor at work. Two-step, no same-summarise
  # shadowing: a later summarise() expression can see a JUST-CREATED
  # column instead of the upstream vector. Counts in summarise,
  # ratios in mutate, uniquely named intermediates.
  uni <- rec_game_c3 %>%
    mutate(zero_split = is.na(.data[[RT_COL]]) | .data[[RT_COL]] == 0,
           sub_floor  = !zero_split & .data[[RT_COL]] < X_SPLIT_RTE,
           grade_na   = !zero_split & .data[[RT_COL]] >= X_SPLIT_RTE &
             is.na(.data[[G_COL]])) %>%
    group_by(season) %>%
    summarise(rows = dplyr::n(),
              n_zero = sum(zero_split),
              n_played = sum(!zero_split),
              n_sub = sum(sub_floor),
              n_qual = sum(!zero_split & !sub_floor),
              n_grade_na = sum(grade_na),
              .groups = "drop") %>%
    mutate(zero_split = round(n_zero / rows, 3),
           sub_floor_of_played = round(n_sub / pmax(n_played, 1), 3),
           grade_na_among_quals = round(n_grade_na / pmax(n_qual, 1), 4)) %>%
    select(season, rows, zero_split, sub_floor_of_played,
           grade_na_among_quals)
  cat(sprintf("--- honesty (%s): split-zero / sub-floor / grade-NA shares by season ---\n", sp))
  print(uni, n = 12)
  
  # honesty: within-opponent pool sizes + the singleton share
  cat(sprintf("--- honesty (%s): within-defense pool sizes (receiver-games per pool) ---\n", sp))
  print(game %>% distinct(def_ssn, band, n_pool) %>%
          group_by(band) %>%
          summarise(pools = dplyr::n(),
                    singleton_share = round(mean(n_pool == 1), 3),
                    p10 = quantile(n_pool, .10),
                    med = median(n_pool),
                    p90 = quantile(n_pool, .90), .groups = "drop"))
  
  # season grain -- MEDIAN of vsopp ranks. Season entry = canon
  # rec_qual membership (imported overall gate); the split season
  # floor (SPLIT_RTE_MIN season split routes) gates the PCTL below,
  # exactly canon's man_ok/zone_ok law.
  ssn <- game %>%
    group_by(player_id, season) %>%
    summarise(pool_g = dplyr::n(),
              sp_routes = sum(sp_routes),
              med_vsopp_grade = median(vsopp_rank_grade, na.rm = TRUE),
              med_vsopp_yprr = median(vsopp_rank_yprr, na.rm = TRUE),
              .groups = "drop") %>%
    inner_join(rec_qual, by = c("player_id", "season")) %>%
    # template law: the season band comes from canon's identity map,
    # re-joined AFTER the aggregation -- summarise() drops
    # non-grouping columns, and band must come home here
    inner_join(rec_band_season, by = c("player_id", "season")) %>%
    inner_join(rec_name, by = "player_id") %>%
    mutate(split_ok = sp_routes >= SPLIT_RTE_MIN)
  stopifnot(anyDuplicated(ssn[, c("player_id", "season")]) == 0)
  
  cat(sprintf("--- split-floor coverage (%s): gated seasons with split routes >= %d ---\n",
              sp, SPLIT_RTE_MIN))
  print(ssn %>% group_by(band) %>%
          summarise(n = dplyr::n(), split_ok = sum(split_ok),
                    .groups = "drop"))
  
  # THE CURRENCY -- rolling POOL_W-season pools within (band,
  # split), stamped receiving pool law. Singleton-lean cells get
  # the 0.5 guard from percent_rank_avg; cell sizes print.
  pool_c3 <- function(s) {
    ssn %>% filter(season %in% (s - POOL_W + 1):s) %>%
      group_by(band) %>%
      mutate(c3_grade_pctl = percent_rank_avg(
        ifelse(split_ok, med_vsopp_grade, NA_real_)),
        c3_yprr_pctl = percent_rank_avg(
          ifelse(split_ok, med_vsopp_yprr, NA_real_))) %>%
      ungroup() %>% filter(season == s)
  }
  pctl <- purrr::map_dfr(2018:2025, pool_c3)
  
  cat(sprintf("--- pooled cell sizes (%s): band x 3-season window ---\n", sp))
  print(ssn %>% filter(season >= 2016, split_ok) %>%
          mutate(window = paste0(season - 2, "-", season)) %>%
          count(window, band) %>%
          group_by(band) %>%
          summarise(min_cell = min(n), med_cell = median(n),
                    .groups = "drop"))
  
  cat(sprintf("--- eyeball (%s): 2025 top-10 by c3 grade pctl (should read like a WR1-vs-scheme list) ---\n", sp))
  print(pctl %>% filter(season == 2025) %>% arrange(desc(c3_grade_pctl)) %>%
          select(player, band, pool_g, sp_routes, med_vsopp_grade,
                 c3_grade_pctl, c3_yprr_pctl) %>% head(10))
  cat(sprintf("--- eyeball (%s): 2025 bottom-10 (gated, so real rotation receivers) ---\n", sp))
  print(pctl %>% filter(season == 2025) %>% arrange(c3_grade_pctl) %>%
          select(player, band, pool_g, sp_routes, med_vsopp_grade,
                 c3_grade_pctl, c3_yprr_pctl) %>% head(10))
  
  list(game = game %>% mutate(split = sp),
       season = ssn %>% mutate(split = sp),
       pctl = pctl %>% mutate(split = sp))
}

run_man  <- c3_one_split_rec("man")
run_zone <- c3_one_split_rec("zone")

rec_c3_game   <- bind_rows(run_man$game,   run_zone$game)
rec_c3_season <- bind_rows(run_man$season, run_zone$season)
rec_c3_pctl   <- bind_rows(run_man$pctl,   run_zone$pctl)

# split-law wall: the long frames carry `split`; a player-season can
# appear once per split (gates both ways) and NEVER twice within one
stopifnot(anyDuplicated(rec_c3_pctl[, c("player_id", "season",
                                        "split")]) == 0)

# ------------------------------------------------------------
# 2. SOFT CONTEXT -- the c1 comparison. Lights up only if
#    rec_season_pctl_sos is in session (it gates the needs list,
#    so this always lights after canon; kept soft on principle).
# ------------------------------------------------------------

if (exists("rec_season_pctl_sos")) {
  c1_long <- tibble::as_tibble(rec_season_pctl_sos) %>%
    select(player_id, season, band,
           man_grade_pctl, man_yprr_pctl,
           zone_grade_pctl, zone_yprr_pctl) %>%
    tidyr::pivot_longer(cols = -c(player_id, season, band),
                        names_to = c("split", "metric"),
                        names_pattern = "(man|zone)_(grade|yprr)_pctl",
                        values_to = "c1_pctl")
  
  c1_c3 <- rec_c3_pctl %>%
    tidyr::pivot_longer(cols = c(c3_grade_pctl, c3_yprr_pctl),
                        names_to = "metric", values_to = "c3_pctl",
                        names_pattern = "c3_(grade|yprr)_pctl") %>%
    inner_join(c1_long, by = c("player_id", "season", "band",
                               "split", "metric"))
  
  cat("\n--- c1 vs c3: how much does slate-adjustment reorder?",
      "(per split x metric -- never recombined) ---\n")
  print(c1_c3 %>% group_by(split, metric, season) %>%
          summarise(n = dplyr::n(),
                    r = round(cor(c1_pctl, c3_pctl,
                                  use = "complete.obs"), 3),
                    .groups = "drop"), n = 48)
  
  cat("\n--- 2025 biggest movers per split, grade lens, c3 - c1",
      "(slate-flattered < 0 < slate-punished) ---\n")
  for (sp in c("man", "zone")) {
    cat(sprintf("  [%s]\n", sp))
    print(c1_c3 %>% filter(season == 2025, split == sp,
                           metric == "grade") %>%
            mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
            arrange(desc(abs(d))) %>%
            select(player, band, c1_pctl, c3_pctl, d) %>%
            head(10))
  }
  
  # WHO FACED NE -- the slate NE's secondary actually saw. d > 0:
  # the league's raw read on him vs NE was held down by his slate;
  # d < 0: he was slate-flattered coming in.
  cat("\n--- 2025 receivers who FACED NE (def_ssn == NE2025), grade lens ---\n")
  faced_ne_ids <- rec_c3_game %>%
    filter(season == 2025, def_ssn == "NE2025") %>%
    distinct(player_id)
  print(c1_c3 %>% filter(season == 2025, metric == "grade") %>%
          semi_join(faced_ne_ids, by = "player_id") %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(split, band, desc(c3_pctl)) %>%
          select(split, player, band, pool_g, c1_pctl, c3_pctl, d),
        n = Inf)
  
  # NE's OWN room, both lenses -- context only; the unit here is
  # opponent receivers, but the room always gets eyeballed.
  cat("\n--- NE's OWN 2025 pass catchers, c1 vs c3, grade lens ---\n")
  ne_own_ids <- rec_c3_game %>%
    filter(season == 2025, posteam == "NE") %>%
    distinct(player_id)
  print(c1_c3 %>% filter(season == 2025, metric == "grade") %>%
          semi_join(ne_own_ids, by = "player_id") %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(split, band, desc(c3_pctl)) %>%
          select(split, player, band, pool_g, c1_pctl, c3_pctl, d),
        n = Inf)
} else {
  cat("\n[soft skip] rec_season_pctl_sos not in session --",
      "source canon Phase 5 for the c1 comparison\n")
}

# ------------------------------------------------------------
# THE COLUMN LEGEND -- prints at the end of every run
# ------------------------------------------------------------

legend_c3r <- c(
  "",
  "============================================================",
  "HOW TO READ WHAT YOU JUST BUILT -- RECEIVING CURRENCY THREE",
  "============================================================",
  "rec_c3_pctl: one row per player_id x season x split x band.",
  "Splits NEVER recombine (canon Phase 5 law). Columns:",
  "",
  "  pool_g           games that entered this split's pools:",
  "                   split routes >= X_SPLIT_RTE (PROPOSED 5,",
  "                   UNSIGNED) with a non-NA split grade.",
  "  sp_routes        total split routes that season. The split",
  "                   pctl exists only at sp_routes >= 50 (canon",
  "                   SPLIT_RTE_MIN, the man_ok/zone_ok law).",
  "  qual_g           canon's OVERALL qualifying games (X_QUAL = 8",
  "                   routes), imported via rec_qual -- season entry",
  "                   into the currency, same gate as c1.",
  "  med_vsopp_grade  MEDIAN of the game's within-opponent grade",
  "                   ranks: each pooled game ranked among every",
  "                   same-band receiver-game against the SAME",
  "                   defense (def_ssn) that season -- your day vs",
  "                   everyone who faced that defense. Singleton",
  "                   pools score 0.5.",
  "  med_vsopp_yprr   same, on game split yards-per-route-run",
  "                   (split yards / split routes -- the canon",
  "                   YPRR anchor reconstructed from raw counts).",
  "  c3_grade_pctl    the currency: percent_rank of the season",
  "                   median within band x split over the rolling",
  "                   3-season window (canon POOL_W = 3 law).",
  "                   Higher = better receiver = HARDER for NE's",
  "                   secondary. No inversions anywhere.",
  "  c3_yprr_pctl     same, on the YPRR anchor.",
  "",
  "Season entry = canon rec_qual (X_QUAL = 8 overall routes in a",
  "game, G_MIN = 6 games), imported. Pools roll 3 seasons, so",
  "2016-17 seasons feed windows but are never scored themselves.",
  "",
  "Reading the c1 vs c3 movers: d = c3 - c1. d > 0 = slate-",
  "PUNISHED (his raw percentile was held down by a hard slate of",
  "defenses; the common-opponent comparison hands it back).",
  "d < 0 = slate-flattered.",
  "",
  "FIREWALL: currency three measures PLAYERS, slate-adjusted. It",
  "never feeds any SoS slate -- the SoS measures schedules with",
  "UNadjusted quality. No league object is touched here.",
  "============================================================"
)
cat(legend_c3r, sep = "\n")

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# save.image("~/receiving_c3_workspace.RData")
# system('aws s3 cp ~/receiving_c3_workspace.RData s3://nfl-pff-data-lucas/workspaces/')
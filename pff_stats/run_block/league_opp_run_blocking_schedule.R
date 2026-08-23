# ============================================================
# NE 2026 -- OPPONENT RUN BLOCK SoS -- LEAGUE MODE (Phase 1 league
# lens, RUN-BLOCK HALF). Written 2026-08-18.
#
# WHY THIS FILE EXISTS (Andy's ruling, 2026-08-18): pass blocking
# and run blocking are TWO DIFFERENT THINGS -- separate files, no
# fused OL lens. The fused run's own numbers back the split:
# cor(d_tps, d_gap) = 0.239 and cor(d_tps, d_zone) = -0.066 across
# 32 -- the two schedules barely correlate. The pass-pro half (tps)
# lives in league_opp_pass_block_schedule.
#
# WHAT IT IS: the league engine pointed at opponent RUN BLOCKING,
# two scheme lenses (canon facets): gap and zone. Per focal, per lens:
#   v26     = mean of the focal's 17 2026 opponents' run-block unit
#             values (each opponent = mean of its five slot values).
#   faced25 = the 2025 starters the focal's FRONT actually faced
#             (canon construction: one blocker per week x team x slot
#             by pass-block snaps -- the canon NE file priced gap/zone
#             off the same pass-snap-picked body; kept), fill-law
#             priced.
#   d       = v26 - faced25. d > 0 = the 2026 slate's run-block lines
#             are better than the lines this front beat on in 2025.
#
# CANON SOURCES (imported, not re-derived -- source
# new_england_opp_ol_schedule.R FIRST): the currency is Andy's
# PRE-COMPUTED season pctls (ol_season_pctl / ol_season_pctl_24 via
# build_pctl) -- canon law for this unit, kept. Plus rookie_prior,
# ol_2025_snaps, ol_2024_profile, blend2, in_season, ol_pos_levels,
# id_xwalk, pff_team_lookup.
#
# SIGNED LAW KEPT: canon final-value rungs (rookie: blend else
# prior; non-rookie: blend else raw25); FACED FILL LAW (Andy's
# 2026-08-17 ruling) with earned / slot_prior / unit_scalar tiers;
# phantom law (phases 3/6b) as the FINAL membership backstop;
# playoffs IN on the faced side (in_season); 2026 REG weeks only on
# the slate side (playoff opponents unknowable); def_ssn-native
# codes; code wall; 544-row / 17-games walls; 32-focal wall.
#
# PROPOSED -- Kimi, UNSIGNED (new in this file, awaiting the stamp):
#   P1. VALUE-CHAIN LAST RESORTS: the v26 value chain is extended to
#       coalesce(blend, raw25, SLOT PRIOR, UNIT SCALAR) for
#       non-rookies (and blend, prior, unit scalar for rookies) --
#       the same three-tier philosophy Andy signed for the FACED
#       side on 2026-08-17, applied to the v26 side. REASON: the
#       fused run silently dropped slots whose pick had no
#       qualifying season pctl (KC's gap value came off TWO slots,
#       ARZ zone off TWO, and prior_share reported 0 while it
#       happened). Every slot now prices, the rescue tier is counted
#       in prior_share, and every rescued slot is printed by name.
#       n_scored == 5 is now a hard wall, not a receipt.
#   P2. FLEX-FILL RUNG: a slot with NO modal-slot candidate now
#       takes the best unassigned rostered body on that team
#       (pmax('25,'24) pass-block usage, player_id tie-break, each
#       body used once), priced at HIS earned pctls, flagged
#       flex_fill with his native slot printed. This is the data
#       version of what Andy's hand tribble does when it moves a
#       player across slots (e.g. Jordan Morgan to GB LT). Phantom
#       law remains the backstop when the pool is empty. An
#       alternative Andy may prefer: restrict flex to position
#       families (IOL<->IOL, T<->T). UNRULED -- flex is unbounded
#       here and every cross-slot fill prints slot_from -> slot.
#   P3. UNPICKED-STARTER AUDIT threshold: 400 pass-block snaps
#       (2025). Prints every heavy-usage 2025 OL the engine picked
#       NOWHERE, with the disposition (off the 2026 roster per
#       nflreadr / rostered but no PFF id / lost the slot battle).
#
# MEMBERSHIP BLIND SPOTS (printed, not hidden): 2026 rookies carry
# no NFL snaps, so they are invisible to rung 1 and enter only as
# last-resort flex bodies; position-switchers surface via flex or
# not at all. For NE's 14 opponents the canon hand tribble remains
# the authority -- R3a prints every slot where this engine disagrees
# with it. The fused file's note stands: canon's Wanya Morris KC
# one-off filter and 14-team name-dupe wall were tribble machinery,
# dropped here; this file walls team x slot dupe picks instead.
# ============================================================

needed_ol <- c("all_pass_block_summary", "ol_season_pctl",
               "ol_season_pctl_24", "ol_2025_snaps", "ol_2024_profile",
               "rookie_prior", "id_xwalk", "pff_team_lookup",
               "in_season", "blend2", "ol_pos_levels")
missing_ol <- needed_ol[!vapply(needed_ol, exists, logical(1))]
if (length(missing_ol)) stop("missing session objects: ",
                             paste(missing_ol, collapse = ", "),
                             " -- source new_england_opp_ol_schedule.R first")

library(dplyr)

all_pass_block_summary <- tibble::as_tibble(all_pass_block_summary)
ol_season_pctl         <- tibble::as_tibble(ol_season_pctl)
ol_season_pctl_24      <- tibble::as_tibble(ol_season_pctl_24)
ol_2025_snaps          <- tibble::as_tibble(ol_2025_snaps)
ol_2024_profile        <- tibble::as_tibble(ol_2024_profile)
rookie_prior           <- tibble::as_tibble(rookie_prior)

# --- the 2026 schedule frame (three-tier robust load; game_type is
#     dead, week keys REG; identical law to the receiving league file)
load_sched_ol <- function(season) {
  ok <- function(df) is.data.frame(df) &&
    all(c("season", "week", "home_team", "away_team") %in% names(df)) &&
    nrow(df) > 200
  s <- tryCatch(nflreadr::load_schedules(season), error = function(e) NULL)
  if (!ok(s)) {
    cat("nflreadr::load_schedules(", season,
        ") returned a bad frame -- pulling raw nflverse games.rds direct\n", sep = "")
    s <- tryCatch(readRDS(url(
      "https://github.com/nflverse/nflverse-data/releases/download/schedules/games.rds",
      "rb")), error = function(e) NULL)
    if (is.data.frame(s) && "season" %in% names(s))
      s <- s %>% filter(season == .env$season)
  }
  if (!ok(s)) {
    cat("games.rds unreachable -- trying the games.csv mirror\n")
    s <- tryCatch(utils::read.csv(url(
      "https://github.com/nflverse/nflverse-data/releases/download/schedules/games.csv")),
      error = function(e) NULL)
    if (is.data.frame(s) && "season" %in% names(s))
      s <- s %>% filter(season == .env$season)
  }
  if (!ok(s)) {
    cat("schedule load failed all three ways; last attempt class/names:\n")
    print(class(s)); print(names(s))
    stop("no usable 2026 schedule -- slate blocked")
  }
  s
}

# nflverse codes the canon lookup predates or never carried: new-style
# AZ/LAR/JAC/WSH AND old-style ARI/BAL/CLE/HOU (the raw 2026 file uses
# the old style). canon lookup wins first; patch fills gaps.
lookup_patch_ol <- c("ARI" = "ARZ", "AZ" = "ARZ", "BAL" = "BLT",
                     "CLE" = "CLV", "HOU" = "HST", "LAR" = "LA",
                     "JAC" = "JAX", "WSH" = "WAS")
to_pff_ol <- function(x) unname(dplyr::coalesce(pff_team_lookup[x],
                                                lookup_patch_ol[x], x))

# the PFF 32, def_ssn-native (plain TEAM2025 -- proven clean)
pff32_ol <- all_pass_block_summary %>%
  filter(season == 2025, in_season(week)) %>%
  pull(def_ssn) %>% stringr::str_remove("2025$") %>%
  unique() %>% sort()
stopifnot(length(pff32_ol) == 32L)

sch26 <- load_sched_ol(2026) %>%
  filter(week <= 18)   # REG weeks (2026 playoff opponents unknowable)

sched26_teams_ol <- sort(unique(to_pff_ol(c(sch26$home_team,
                                            sch26$away_team))))
if (!setequal(sched26_teams_ol, pff32_ol)) {
  cat("2026 schedule-vs-currency code mismatch:\n")
  cat("schedule-only:", setdiff(sched26_teams_ol, pff32_ol), "\n")
  cat("currency-only:", setdiff(pff32_ol, sched26_teams_ol), "\n")
  stop("2026 schedule code alignment failed")
}

opp_map_ol <- bind_rows(
  sch26 %>% transmute(focal = home_team, opp = away_team),
  sch26 %>% transmute(focal = away_team, opp = home_team)) %>%
  mutate(focal = to_pff_ol(focal), opp = to_pff_ol(opp))
opp_n_ol <- opp_map_ol %>% dplyr::count(focal, name = "n_games")
stopifnot(nrow(opp_map_ol) == 544L, all(opp_n_ol$n_games == 17L))

# ------------------------------------------------------------
# A. 32-team roster frame -- canon section 3, team filter removed
# ------------------------------------------------------------

ol_2026_lg <- nflreadr::load_rosters(2026) %>%
  filter(position %in% c("T", "G", "C", "OL", "OT", "OG")) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = to_pff_ol(team),
            entry_year, years_exp, roster_pos = position) %>%
  left_join(id_xwalk %>% filter(!is.na(gsis_id)) %>%
              select(player_id, gsis_id), by = "gsis_id") %>%
  left_join(ol_2025_snaps, by = "player_id") %>%
  left_join(ol_season_pctl, by = c("player_id", "det_position")) %>%
  left_join(ol_2024_profile, by = "player_id") %>%
  mutate(w25 = pmin(dplyr::coalesce(g_2025_total, 0L) / 10, 1),
         tps_grade_bl = blend2(tps_grade, tps_grade_24, w25),
         gap_bl       = blend2(gap,       gap_24,       w25),
         zone_bl      = blend2(zone,      zone_24,      w25),
         status = case_when(
           entry_year == 2026                     ~ "rookie",
           is.na(player_id)                       ~ "no_pff_id",
           is.na(snaps_2025) & !is.na(snaps_2024) ~ "data_2024_only",
           is.na(snaps_2025)                      ~ "no_recent_ol_snaps",
           TRUE                                   ~ "has_data"))

# roster code wall: all 32 and nothing else
ros_teams_ol <- sort(unique(ol_2026_lg$team_name))
if (!setequal(ros_teams_ol, pff32_ol)) {
  cat("2026 roster-vs-currency code mismatch:\n")
  cat("roster-only:", setdiff(ros_teams_ol, pff32_ol), "\n")
  cat("currency-only:", setdiff(pff32_ol, ros_teams_ol), "\n")
  stop("2026 roster code alignment failed")
}

# ------------------------------------------------------------
# B. AUTO-MEMBERSHIP -- three rungs, each louder than the last.
#    rung 1 MODAL (the league-engine swap): per team x slot, the
#      rostered player with the most pmax('25,'24) pass-block snaps
#      at that slot (slot = '25 modal, else '24 modal).
#    rung 2 FLEX-FILL (PROPOSED -- Kimi, UNSIGNED, P2): a slot with
#      NO modal candidate takes the best unassigned rostered body on
#      that team, priced at his own earned pctls, native slot
#      printed. Deterministic: slot order LT..RT, usage desc,
#      player_id tie-break, each body used once.
#    rung 3 PHANTOM (signed law, phases 3/6b): pool empty -> slot
#      entry-year prior, flagged phantom.
# ------------------------------------------------------------

cand_ol <- ol_2026_lg %>%
  mutate(row_id = dplyr::row_number(),
         slot = dplyr::coalesce(det_position, det_position_24),
         usage_ord = pmax(dplyr::coalesce(snaps_2025, 0),
                          dplyr::coalesce(snaps_2024, 0)))

modal_pick <- cand_ol %>%
  filter(!is.na(slot), slot %in% ol_pos_levels) %>%
  arrange(team_name, slot, desc(usage_ord), player_id) %>%
  group_by(team_name, slot) %>%
  slice_max(usage_ord, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(rung = "modal", slot_from = NA_character_)

dup_pick <- modal_pick %>% dplyr::count(team_name, slot) %>% filter(n > 1)
if (nrow(dup_pick)) { print(dup_pick); stop("dupe team x slot picks") }

grid_ol <- tidyr::expand_grid(team_name = pff32_ol, slot = ol_pos_levels)
open_slots <- grid_ol %>%
  anti_join(modal_pick, by = c("team_name", "slot")) %>%
  arrange(team_name, match(slot, ol_pos_levels))

pool <- cand_ol %>% anti_join(modal_pick, by = "row_id")
pool_team <- split(pool, pool$team_name)
flex_rows <- vector("list", nrow(open_slots))
n_flex <- 0L
for (i in seq_len(nrow(open_slots))) {
  tm <- open_slots$team_name[i]
  pl <- pool_team[[tm]]
  if (!is.null(pl) && nrow(pl) > 0) {
    pl <- pl[order(-pl$usage_ord, pl$player_id, na.last = TRUE), ,
             drop = FALSE]
    pick <- pl[1, , drop = FALSE]
    pool_team[[tm]] <- pl[-1, , drop = FALSE]
    n_flex <- n_flex + 1L
    flex_rows[[n_flex]] <- pick %>%
      mutate(slot_from = slot, slot = open_slots$slot[i],
             rung = "flex_fill")
  }
}
flex_pick <- if (n_flex > 0L) bind_rows(flex_rows[seq_len(n_flex)]) else
  modal_pick[0, ]

slot_fill <- bind_rows(modal_pick, flex_pick)

cat("--- rung 2 FLEX-FILL (PROPOSED P2): open slot <- best unassigned",
    "rostered body ---\n")
if (nrow(flex_pick)) {
  print(flex_pick %>%
          select(team_name, slot, slot_from, roster_name, usage_ord,
                 status), n = Inf)
} else cat("  (none -- every slot had a modal candidate)\n")

# slot-completed grid: still-missing slots = PHANTOMS at the slot prior
slots_full <- grid_ol %>%
  left_join(slot_fill, by = c("team_name", "slot")) %>%
  left_join(rookie_prior, by = c("slot" = "det_position")) %>%
  mutate(phantom = is.na(roster_name),
         rung = dplyr::coalesce(rung, "phantom"))

cat("--- rung 3 PHANTOM slots (pool empty; priced at the slot prior) ---\n")
print(slots_full %>% filter(phantom) %>% select(team_name, slot), n = Inf)

# phantom ladder: for every phantom team, the WHOLE rostered OL room,
# so the cause is visible in one glance -- no body with that modal
# slot at all? rostered but no PFF id? everyone assigned elsewhere?
phantom_teams <- slots_full %>% filter(phantom) %>%
  pull(team_name) %>% unique()
if (length(phantom_teams)) {
  cat("--- phantom ladder: full rostered OL room on each phantom team ---\n")
  print(cand_ol %>% filter(team_name %in% phantom_teams) %>%
          select(team_name, roster_name, slot, usage_ord, status,
                 entry_year) %>%
          arrange(team_name, desc(usage_ord)), n = Inf)
}

cat("--- membership: rung-1 picks with zero NFL-snap evidence",
    "(usage_ord == 0) ---\n")
print(modal_pick %>% filter(usage_ord == 0) %>%
        select(team_name, slot, roster_name, status), n = Inf)

# --- THE 160-SLOT LADDER: the membership this file priced. This is
#     the audit for the 18 teams canon never hand-built; eyeball it
#     against reality. slot_from is NA for modal picks, set for flex.
cat("--- THE 160-SLOT LEAGUE LADDER (membership priced below) ---\n")
print(slots_full %>%
        select(team_name, slot, roster_name, rung, slot_from, usage_ord,
               status) %>%
        arrange(team_name, match(slot, ol_pos_levels)), n = 160)

# --- A4. UNPICKED-STARTER AUDIT (PROPOSED P3, threshold 400): every
#     2025 OL with heavy pass-block usage the engine picked NOWHERE,
#     with the disposition. This is where "where did X go" questions
#     get per-player answers. Display names: 2025 max-snap game row.
nm25_ol <- all_pass_block_summary %>%
  filter(season == 2025, in_season(week)) %>%
  group_by(player_id) %>%
  slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, disp_name = player)

big25_unpicked <- ol_2025_snaps %>%
  filter(snaps_2025 >= 400) %>%
  anti_join(slot_fill %>% filter(!is.na(player_id)) %>%
              select(player_id), by = "player_id") %>%
  left_join(nm25_ol, by = "player_id") %>%
  left_join(ol_2026_lg %>%
              select(player_id, ros_team = team_name, ros_status = status),
            by = "player_id") %>%
  mutate(win_key = slot_fill$roster_name[match(
    paste(ros_team, det_position),
    paste(slot_fill$team_name, slot_fill$slot))],
    disposition = case_when(
      is.na(ros_team) ~ "not on a 2026 roster per nflreadr",
      ros_status == "no_pff_id" ~ "ON A 2026 ROSTER but no PFF id -- JOIN BROKE HERE",
      is.na(win_key) ~ paste0("on ", ros_team,
                              " -- unassigned (flex pool, native ",
                              det_position, ")"),
      TRUE ~ paste0("on ", ros_team, " -- lost ", det_position,
                    " to ", win_key))) %>%
  arrange(desc(snaps_2025))

cat("--- A4: 2025 OL with >= 400 PB snaps picked NOWHERE (P3) ---\n")
print(big25_unpicked %>%
        select(disp_name, snaps_2025, det_position, disposition), n = Inf)

# ------------------------------------------------------------
# C. THE LENS MACHINE. Identical text in the pass and run files --
#    only the lens argument and the column maps swap. v26 side:
#    canon final-value law + PROPOSED P1 last resorts, so V can
#    never be NA: blend -> raw25 -> slot prior -> unit scalar.
# ------------------------------------------------------------

league_one_lens <- function(lens) {
  vf  <- switch(lens, tps = "tps_grade_bl", gap = "gap_bl",  zone = "zone_bl")
  v25 <- switch(lens, tps = "tps_grade",    gap = "gap",     zone = "zone")
  vpr <- switch(lens, tps = "pr_tps",       gap = "pr_gap",  zone = "pr_zone")
  
  prior_unit <- mean(rookie_prior[[vpr]], na.rm = TRUE)
  
  slots_v <- slots_full %>%
    mutate(V_bl = .data[[vf]],
           V_raw25 = .data[[v25]],
           V_pr = .data[[vpr]],
           V = case_when(
             phantom ~ dplyr::coalesce(V_pr, prior_unit),
             status == "rookie" ~ dplyr::coalesce(V_bl, V_pr, prior_unit),
             TRUE ~ dplyr::coalesce(V_bl, V_raw25, V_pr, prior_unit)),
           V_source = case_when(
             phantom & !is.na(V_pr)              ~ "prior_phantom",
             phantom                             ~ "unit_scalar",
             status == "rookie" & !is.na(V_bl)   ~ "blend",
             status == "rookie" & !is.na(V_pr)   ~ "prior_rookie",
             status == "rookie"                  ~ "unit_scalar",
             !is.na(V_bl)                        ~ "blend",
             !is.na(V_raw25)                     ~ "raw25",
             !is.na(V_pr)                        ~ "prior_rescue",
             TRUE                                ~ "unit_scalar"),
           prior_used = !(V_source %in% c("blend", "raw25")))
  if (any(is.na(slots_v$V))) {
    cat("slots with NA V after the full value chain (should be none):\n")
    print(slots_v %>% filter(is.na(V)) %>%
            select(team_name, slot, roster_name, rung, status, V_source))
    stop("v26 value chain produced NA -- the last-resort rungs failed")
  }
  
  # P1 honesty: the rescue rungs print by name, every run.
  rescue <- slots_v %>%
    filter(V_source %in% c("prior_rescue", "unit_scalar"))
  cat(sprintf(paste0("--- P1 rescues (%s lens): picked bodies with no",
                     " qualifying pctl, priced at prior/scalar ---\n"), lens))
  if (nrow(rescue)) {
    print(rescue %>% select(team_name, slot, roster_name, rung, status,
                            V_source, V), n = Inf)
  } else cat("  (none -- every pick earned a blend or raw25)\n")
  vs_tiers <- slots_v %>% dplyr::count(V_source, name = "slots") %>%
    mutate(share = slots / sum(slots)) %>% arrange(desc(slots))
  cat(sprintf("V_source tiers (%s lens, share of 160 slots): %s\n", lens,
              paste(vs_tiers$V_source, round(vs_tiers$share, 3),
                    sep = "=", collapse = " | ")))
  
  team26 <- slots_v %>%
    group_by(team_name) %>%
    summarise(v26 = mean(V),
              prior_share = mean(prior_used),
              n_scored = sum(!is.na(V)),
              .groups = "drop")
  if (any(team26$n_scored != 5L)) {
    cat("teams failing the 5-slot wall:\n")
    print(team26 %>% filter(n_scored != 5L))
    stop("v26 slot wall failed -- every team must price exactly 5 slots")
  }
  
  # FACED FILL LAW (uniform, Andy's 2026-08-17 ruling, OL port):
  #   canon faced construction (one blocker per week x team x slot by
  #   pass-block snaps, the >= 25 PB-snap frame) priced with the
  #   earned 2025 pctl where present, else the slot prior, else the
  #   unit scalar. Currency data stays NA; fill_share = cell share.
  faced_cells <- all_pass_block_summary %>%
    filter(season == 2025, in_season(week),
           det_position %in% ol_pos_levels) %>%
    mutate(def_t = stringr::str_remove(def_ssn, "2025$")) %>%
    group_by(def_t, week, team_name, det_position) %>%
    slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(def_t, week, team_name, det_position, player, player_id) %>%
    left_join(ol_season_pctl %>%
                select(player_id, det_position, F25 = all_of(v25)),
              by = c("player_id", "det_position")) %>%
    left_join(rookie_prior %>% select(det_position, VPR = all_of(vpr)),
              by = "det_position") %>%
    mutate(fill = is.na(F25),
           fill_tier = case_when(!fill ~ "earned",
                                 !is.na(VPR) ~ "slot_prior",
                                 TRUE ~ "unit_scalar"),
           F25 = dplyr::coalesce(F25, VPR, prior_unit))
  stopifnot(!any(is.na(faced_cells$F25)))
  tier_rt <- faced_cells %>%
    dplyr::count(fill_tier, name = "cells") %>%
    mutate(share = cells / sum(cells))
  cat(sprintf("faced fill tiers (%s lens, by cell share): %s\n", lens,
              paste(tier_rt$fill_tier, round(tier_rt$share, 3),
                    sep = "=", collapse = " | ")))
  faced <- faced_cells %>%
    group_by(def_t) %>%
    summarise(faced25 = mean(F25, na.rm = TRUE),
              fill_share = mean(fill),
              n_cells = dplyr::n(),
              n_weeks = dplyr::n_distinct(week),
              po_weeks = dplyr::n_distinct(week[week > 18]),
              .groups = "drop")
  if (nrow(faced) != 32L) {
    cat("expected 32 focals, got", nrow(faced), "\n")
    cat("missing:", setdiff(pff32_ol, faced$def_t), "\n")
    cat("extra:", setdiff(faced$def_t, pff32_ol), "\n")
    stop("focal count wall failed")
  }
  cat(sprintf("cells per focal (%s lens): min %d / med %d / max %d\n",
              lens, min(faced$n_cells), as.integer(median(faced$n_cells)),
              max(faced$n_cells)))
  cat(sprintf("NE faced weeks (%s lens): %d incl. %d playoff weeks\n", lens,
              faced$n_weeks[faced$def_t == "NE"],
              faced$po_weeks[faced$def_t == "NE"]))
  
  opp_map_ol %>%
    left_join(team26, by = c("opp" = "team_name")) %>%
    group_by(focal) %>%
    summarise(v26 = mean(v26, na.rm = TRUE),
              prior_share = mean(prior_share, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(faced, by = c("focal" = "def_t")) %>%
    mutate(lens = lens, d = v26 - faced25) %>%
    select(focal, lens, v26, faced25, d, prior_share, fill_share)
}

league_sys_rb <- bind_rows(league_one_lens("gap"),
                           league_one_lens("zone"))

# ------------------------------------------------------------
# D. THE LEAGUE RUN-BLOCK DIFFERENTIAL -- two scheme lenses, one frame
# ------------------------------------------------------------

league_wide_rb <- league_sys_rb %>%
  tidyr::pivot_wider(names_from = lens,
                     values_from = c(v26, faced25, d,
                                     prior_share, fill_share)) %>%
  mutate(rank_gap  = rank(-d_gap),
         rank_zone = rank(-d_zone)) %>%
  arrange(desc(d_gap))

cat("\n--- THE OPPONENT RUN-BLOCK DIFFERENTIAL, sorted by d_gap ---\n")
print(league_wide_rb %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), n = 32)

cat("\n--- sorted by d_zone ---\n")
print(league_wide_rb %>% arrange(desc(d_zone)) %>%
        select(focal, v26_zone, faced25_zone, d_zone, rank_zone,
               fill_share_zone) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), n = 32)

summ_rb <- league_sys_rb %>%
  group_by(lens) %>%
  summarise(league_mean_d = round(mean(d), 3),
            league_sd_d   = round(sd(d), 3),
            ne_d    = round(d[focal == "NE"], 3),
            ne_rank = rank(-d)[focal == "NE"],
            mean_prior_share = round(mean(prior_share), 3),
            mean_fill_share  = round(mean(fill_share), 3),
            .groups = "drop")

cat("\n--- league summary per lens ---\n")
print(summ_rb)

cat("\ncor(d_gap, d_zone) across 32:",
    round(cor(league_wide_rb$d_gap, league_wide_rb$d_zone), 3), "\n")
cat("AFC East, run-block slate (rank 1 = schedule gets hardest):\n")
print(league_wide_rb %>% filter(focal %in% c("BUF", "MIA", "NE", "NYJ")) %>%
        select(focal, rank_gap, rank_zone) %>%
        arrange(rank_gap))

# ------------------------------------------------------------
# RECEIPTS (soft-guarded: need the canon file's objects in session)
# ------------------------------------------------------------

if (exists("ne_2025_opp_ol_games") && exists("opp_ol_2026_final") &&
    exists("sched_2026")) {
  
  # R1 -- FACED MACHINE CHECK: canon's NE slate_2025 per-slot gap/zone
  #   means (earned-only, na.rm -- pre-fill-law convention) vs this
  #   file's pre-fill NE per-slot means. IDENTICAL construction by
  #   design; must match to 1e-8. Evidence prints BEFORE the wall.
  canon_s25 <- ne_2025_opp_ol_games %>%
    group_by(det_position) %>%
    summarise(canon_gap = mean(gap_f, na.rm = TRUE),
              canon_zone = mean(zone_f, na.rm = TRUE), .groups = "drop")
  mine_s25 <- all_pass_block_summary %>%
    filter(season == 2025, in_season(week), def_ssn == "NE2025",
           det_position %in% ol_pos_levels) %>%
    group_by(week, team_name, det_position) %>%
    slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    left_join(ol_season_pctl %>%
                select(player_id, det_position, gap, zone),
              by = c("player_id", "det_position")) %>%
    group_by(det_position) %>%
    summarise(mine_gap = mean(gap, na.rm = TRUE),
              mine_zone = mean(zone, na.rm = TRUE), .groups = "drop")
  chk_s25 <- canon_s25 %>% full_join(mine_s25, by = "det_position") %>%
    mutate(ok_gap = abs(canon_gap - mine_gap) < 1e-8,
           ok_zone = abs(canon_zone - mine_zone) < 1e-8)
  cat("\n--- R1: NE faced-2025 per-slot GAP/ZONE, canon vs this file (pre-fill) ---\n")
  print(chk_s25 %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
  stopifnot(all(chk_s25$ok_gap, na.rm = TRUE),
            all(chk_s25$ok_zone, na.rm = TRUE))
  
  # R2 -- THE FILL-LAW MOVE for NE, per lens (BEFORE = earned-only
  #   mean, the pre-fill convention; AFTER = the fill-law value).
  ne_row <- league_wide_rb %>% filter(focal == "NE")
  cat("\n--- R2: NE faced25 before/after the fill law (gap / zone) ---\n")
  for (ln in c("gap", "zone")) {
    v25 <- switch(ln, gap = "gap", zone = "zone")
    pre <- all_pass_block_summary %>%
      filter(season == 2025, in_season(week), def_ssn == "NE2025",
             det_position %in% ol_pos_levels) %>%
      group_by(week, team_name, det_position) %>%
      slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      left_join(ol_season_pctl %>%
                  select(player_id, det_position, F25 = all_of(v25)),
                by = c("player_id", "det_position")) %>%
      summarise(pre = mean(F25, na.rm = TRUE)) %>% pull(pre)
    after <- if (ln == "gap") ne_row$faced25_gap else ne_row$faced25_zone
    fshr  <- if (ln == "gap") ne_row$fill_share_gap else
      ne_row$fill_share_zone
    cat(sprintf("  %4s: before %.4f | after %.4f | fill_share %.3f\n",
                ln, pre, after, fshr))
  }
  
  # R3a -- MEMBERSHIP SWAP: canon's hand tribble vs this engine,
  #   TRIBBLE TEAMS ONLY (the fused file's R3 dumped 86 tribble-NA
  #   rows for non-tribble teams and buried the signal; fixed).
  #   Prints only slots where they differ.
  trib_teams <- unique(opp_ol_2026_final$team_name)
  swap_ol <- opp_ol_2026_final %>%
    select(team_name, slot = det_position, tribble_pick = roster_name) %>%
    full_join(slots_full %>%
                select(team_name, slot, auto_pick = roster_name, rung,
                       slot_from, usage_ord, status),
              by = c("team_name", "slot")) %>%
    filter(team_name %in% trib_teams) %>%
    filter(is.na(tribble_pick) | is.na(auto_pick) |
             tribble_pick != auto_pick)
  cat("\n--- R3a: slots where this engine disagrees with the tribble",
      "(NE's 14 opponents only) ---\n")
  print(swap_ol, n = Inf)
  
  # R4 -- NE v26 under both memberships, per lens (the membership-swap
  #   magnitude on the one schedule canon certifies). NOT a wall.
  canon_s26 <- tibble(team_name = sched_2026) %>%
    left_join(opp_ol_2026_final %>%
                select(team_name, det_position, gap_f, zone_f),
              by = "team_name", relationship = "many-to-many") %>%
    summarise(canon_gap = mean(gap_f, na.rm = TRUE),
              canon_zone = mean(zone_f, na.rm = TRUE))
  cat("\n--- R4: NE v26 gap/zone, tribble membership (canon) vs this engine ---\n")
  cat(sprintf("  gap : canon %.4f | auto %.4f\n", canon_s26$canon_gap,
              ne_row$v26_gap))
  cat(sprintf("  zone: canon %.4f | auto %.4f\n", canon_s26$canon_zone,
              ne_row$v26_zone))
} else {
  cat("\n[soft skip] canon OL objects not in session -- R1/R2/R3a/R4",
      "receipts need new_england_opp_ol_schedule.R sourced first\n")
}

# ------------------------------------------------------------
# THE COLUMN LEGEND -- prints at the end of every run
# ------------------------------------------------------------

legend_rb <- c(
  "",
  "============================================================",
  "HOW TO READ WHAT YOU JUST BUILT -- OPPONENT RUN BLOCK, LEAGUE",
  "============================================================",
  "One row per focal team per scheme lens (canon facets):",
  "  gap = run block, gap scheme   | zone = run block, zone scheme",
  "Pass pro lives in league_opp_pass_block_schedule -- the two are",
  "different markets (cor(d_tps, d_zone) = -0.07 on the fused run).",
  "",
  "  v26_gap / v26_zone     mean of the focal's 17 2026 opponents'",
  "             run-block unit values in that scheme; each opponent",
  "             = mean of its five slot values. THE 2026 SLATE.",
  "  faced25_gap / _zone    the 2025 starters the focal's FRONT",
  "             actually faced (one blocker per week x team x slot",
  "             by pass-block snaps, canon construction), priced",
  "             with the earned season pctl or a prior (fill law).",
  "  d_gap / d_zone         v26 - faced25 = the YoY schedule",
  "             differential in that scheme. d > 0: the 2026 slate's",
  "             run-block lines are BETTER than the lines this front",
  "             beat on in 2025 (harder ahead).",
  "  prior_share_* share of the focal's opponent slot weight riding",
  "             priors/phantoms/rescues (v26 honesty).",
  "  fill_share_*  share of faced-2025 cells priced at priors.",
  "  rank_gap / rank_zone   rank(-d) across 32; 1 = hardest ahead.",
  "",
  "SIGN CAVEAT: d runs positive for most or all teams BY",
  "CONSTRUCTION -- v26 prices projected starters, faced25 prices the",
  "bodies that actually played in 2025 (injury fill-ins and",
  "prior-priced cells included). Canon's own NE file says the same.",
  "The RANK is the signal; the sign is structural.",
  "",
  "HOW EACH SLOT GOT ITS VALUE (V_source, printed every run):",
  "  blend         24/25 snap-weighted blend (canon) -- the norm.",
  "  raw25         2025-only earned pctl (canon fallback).",
  "  prior_rookie  rookie with no blend -> slot entry-year prior",
  "                (canon).",
  "  prior_phantom no rostered body for the slot -> slot prior",
  "                (signed phantom law).",
  "  prior_rescue  PICKED body with no qualifying pctl -> slot",
  "                prior (PROPOSED P1 -- UNSIGNED).",
  "  unit_scalar   prior table hole -> unit scalar (P1 backstop).",
  "HOW EACH SLOT GOT ITS BODY (rung, printed every run):",
  "  modal     top pmax('25,'24) PB-snap usage at his modal slot.",
  "  flex_fill PROPOSED P2 -- UNSIGNED: open slot took the best",
  "            unassigned rostered body; slot_from shows his native",
  "            slot. Phantom is the backstop when the pool is empty.",
  "",
  "FIREWALL: this lens uses canon currency-one (schedule-UNadjusted)",
  "pctls by law; currency-three values never enter here.",
  "============================================================"
)
cat(legend_rb, sep = "\n")

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# save.image("~/league_opp_run_block_workspace.RData")
# system('aws s3 cp ~/league_opp_run_block_workspace.RData s3://nfl-pff-data-lucas/workspaces/')

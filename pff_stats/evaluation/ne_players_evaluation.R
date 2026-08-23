# ============================================================
# NE'S OWN UNITS -- BEFORE AND AFTER INJURY PRICING (2026)
# Andy, 2026-08-22: "i'd also like to see new england's units
# the before and after as well ... just for new england
# players themselves."
#
# WHAT THIS ASKS: the whole pipeline prices OPPONENTS'
# injuries. This is the mirror. If OUR guys miss time in
# 2026, how much does each NE unit actually lose? Per
# player: healthy value (before) vs injury-priced value
# (after), raw and adjusted, one table, all seven units.
#
# 2026-08-22 v2: PASS RUSH JOINED (unit six, off the new
# pass-rush availability layer) and THE CHURN BOARD arrived
# (section 0c): who moved in, who moved out, roster route.
# A "*" on a table row = new to NE in 2026.
# 2026-08-22 v3: RUN DEFENSE JOINED (unit seven, off the new
# run-defense availability layer) -- the unit set is complete.
#
# DIRECTION -- OUR SIDE NOW: higher = better FOR NE on every
# row of every unit. after - before < 0 means injuries cost
# us. THE COLORS FLIP from the opponent tables: red = NE
# loses quality, blue = the price went UP (a backup-level
# starter's expectation can rise when his misses route to a
# replacement who is better than him).
#
# NOTHING IS RECOMPUTED HERE. Every number is FILTERED out of
# the six walled availability member frames -- zero joins,
# zero new math except after - before and 17 x (1 - avail).
# The layers behind those frames carry their own walls.
#
# STAMPS CARRIED (unchanged laws):
#   - PRINT LAW: short English phrases; the table opens with
#     its question; display renames at print; internals keep
#     contract names.
#   - FULL-PRECISION LAW: nothing is rounded here at all --
#     the priced values for OL / rushing arrive rounded to 4
#     from their own layers (their law, their receipt); this
#     file adds no rounding of its own. gt formats at print.
#   - CROSSED-STREAMS LAW: consumes unit-unique member frames
#     ONLY; nothing canon is edited or re-assigned; runs in
#     any session order after its SOURCE ORDER.
#   - m/z NEVER RECOMBINED: receiving and secondary carry one
#     row per player per split; run block one row per slot
#     per scheme. No cross-split value is ever averaged.
#   - gt viewer law: ONE table, printed LAST. The marker
#     below guarantees a failed run never leaves a previous
#     table on screen.
#
# SOURCE ORDER: league_receiving_availability,
#   league_secondary_availability, league_pass_block_
#   availability, league_run_block_availability,
#   league_rushing_availability, pass_rush_availability_layer,
#   run_defense_availability_layer
#   (each green through its own walls), then THIS FILE.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(gt)
})

# ------------------------------------------------------------
# 0. GATES -- the seven member frames, column receipts, unit
#    identity. Prints what is missing BEFORE stopping.
# ------------------------------------------------------------

needed_neo <- c("members_rc", "members_cv",
                "slot_value_26_pb_build", "slot_value_26_rb_build",
                "memb_lg_ru", "members_pa", "members_ra",
                "ol_pos_levels", "REC_BANDS")
missing_neo <- needed_neo[!vapply(needed_neo, exists, logical(1))]
if (length(missing_neo)) {
  cat("missing session objects:\n"); print(missing_neo)
  stop("see SOURCE ORDER in header -- run the seven availability ",
       "layers first")
}

req_cols_neo <- list(
  members_rc = c("team_name", "roster_name", "band", "avail",
                 "usage_w", "mg_f", "mg_p", "zg_f", "zg_p",
                 "c3mg_f", "c3mg_p", "c3zg_f", "c3zg_p"),
  members_cv = c("team", "roster_name", "band", "split", "avail",
                 "usage_w", "grade_f", "grade_p", "c3_f", "adj_p"),
  slot_value_26_pb_build = c("team_name", "slot", "roster_name",
                             "avail", "gf_raw", "gf_raw_av",
                             "gf_adj", "gf_adj_av"),
  slot_value_26_rb_build = c("team_name", "slot", "roster_name",
                             "avail", "V_gap", "V_gap_av",
                             "V_zone", "V_zone_av", "V_c3",
                             "V_c3_av"),
  memb_lg_ru = c("team", "roster_name", "avail", "gf", "gf_av",
                 "V_c3", "V_c3_av", "uw"),
  members_pa = c("team", "roster_name", "band", "status", "avail",
                 "gf", "gf_p", "c3_f", "c3_p", "uw"),
  members_ra = c("team", "roster_name", "band", "status", "avail",
                 "grade_f", "grade_p", "c3_f", "c3_p", "usage_w"))
for (nm_neo in names(req_cols_neo)) {
  fr_neo <- get(nm_neo)
  miss_neo <- setdiff(req_cols_neo[[nm_neo]], names(fr_neo))
  if (length(miss_neo)) {
    cat("frame:", nm_neo, "-- missing columns:\n"); print(miss_neo)
    cat("actual columns:\n"); print(names(fr_neo))
    stop("column receipt printed above -- crossed-streams guard")
  }
}

# unit identity: if another unit is squatting on a name, the
# band / slot sets below fire before anything is built
stopifnot(all(members_rc$band %in% REC_BANDS))
stopifnot(all(members_cv$band %in% c("CB", "SCB", "S", "LB")))
stopifnot(all(members_cv$split %in% c("man", "zone")))
stopifnot(all(slot_value_26_pb_build$slot %in% ol_pos_levels))
stopifnot(all(slot_value_26_rb_build$slot %in% ol_pos_levels))
stopifnot(all(members_pa$band %in% c("ED", "DI")))
stopifnot(all(members_ra$band %in% c("DI", "ED", "LB", "S")))

# anti-staleness marker: if the run dies, the viewer shows THIS,
# never a previous unit's table
gt_marker_neo <- tibble::tibble(
  status = paste0("NE OWN-UNITS table sourced ",
                  format(Sys.time(), "%H:%M:%S"),
                  " -- the table builds in section 3."),
  if_you_see_this = "the run stopped early -- read the console tail") %>%
  gt()
print(gt_marker_neo)

# ------------------------------------------------------------
# 0c. THE CHURN BOARD -- who moved in, who moved out. Roster
#     route: nflreadr 2025 vs 2026, the same source the member
#     frames ride. A "*" on a table row below = new to NE in
#     2026 (rookies count as arrivals). Name matching is
#     cleaned (caps, no punctuation) so format drift cannot
#     fool it; unmatched table names print as a receipt.
# ------------------------------------------------------------

ros25_neo <- nflreadr::load_rosters(2025) %>% filter(team == "NE")
ros26_neo <- nflreadr::load_rosters(2026) %>% filter(team == "NE")

grp_neo <- function(p) dplyr::case_when(
  p %in% c("T", "OT", "G", "OG", "C", "OL")      ~ "OL",
  p %in% c("WR", "TE")                            ~ "pass catchers",
  p %in% c("RB", "FB")                            ~ "backs",
  p %in% c("DL", "DE", "DT", "NT", "EDGE")        ~ "front",
  p %in% c("LB", "ILB", "MLB", "OLB")             ~ "linebackers",
  p %in% c("CB", "S", "DB", "FS", "SS", "NB")     ~ "secondary",
  TRUE ~ "other")

churn_in <- ros26_neo %>%
  filter(!full_name %in% ros25_neo$full_name) %>%
  mutate(grp = grp_neo(position)) %>% arrange(grp, full_name)
churn_out <- ros25_neo %>%
  filter(!full_name %in% ros26_neo$full_name) %>%
  mutate(grp = grp_neo(position)) %>% arrange(grp, full_name)

cat("\n== THE CHURN BOARD -- NE roster, 2025 -> 2026 ==\n")
cat("IN (", nrow(churn_in), "):\n", sep = "")
for (g in sort(unique(churn_in$grp))) {
  gg <- churn_in %>% filter(grp == g)
  cat(sprintf("   %-14s %s\n", g,
              paste0(gg$full_name, " (", gg$position, ")",
                     collapse = ", ")))
}
cat("OUT (", nrow(churn_out), "):\n", sep = "")
for (g in sort(unique(churn_out$grp))) {
  gg <- churn_out %>% filter(grp == g)
  cat(sprintf("   %-14s %s\n", g,
              paste0(gg$full_name, " (", gg$position, ")",
                     collapse = ", ")))
}

cl_neo      <- function(x) gsub("[^A-Z]", "", toupper(x))
new26_keys  <- cl_neo(churn_in$full_name)
ros26_keys  <- cl_neo(ros26_neo$full_name)

# ------------------------------------------------------------
# 1. EXTRACTION -- NE rows only, one long frame. Zero joins.
#    Player order inside a unit: healthy headline value, best
#    first (zone for the split units -- the headline lens per
#    the canon receipts). OL units order by slot. Lens rows
#    for one player stay adjacent. match() on roster_name is
#    safe: every member frame is walled unique on team x name.
# ------------------------------------------------------------

rec_ne <- members_rc %>% filter(team_name == "NE")
sec_ne <- members_cv %>% filter(team == "NE")
pb_ne  <- slot_value_26_pb_build %>% filter(team_name == "NE")
rb_ne  <- slot_value_26_rb_build %>% filter(team_name == "NE")
ru_ne  <- memb_lg_ru %>% filter(team == "NE")
pa_ne  <- members_pa %>% filter(team == "NE")
ra_ne  <- members_ra %>% filter(team == "NE")

cat("\n== NE'S OWN UNITS -- BEFORE AND AFTER ==\n")
cat("-- NE rows pulled per unit (player x lens rows where the\n")
cat("   unit lives in splits / schemes):\n")
cat(sprintf("   receiving  %2d members -> %2d rows (man + zone)\n",
            nrow(rec_ne), 2L * nrow(rec_ne)))
cat(sprintf("   secondary  %2d rows (committee seats, one per split)\n",
            nrow(sec_ne)))
cat(sprintf("   pass block %2d slots   -> %2d rows\n",
            nrow(pb_ne), nrow(pb_ne)))
cat(sprintf("   run block  %2d slots   -> %2d rows (gap + zone)\n",
            nrow(rb_ne), 2L * nrow(rb_ne)))
cat(sprintf("   rushing    %2d backs   -> %2d rows\n",
            nrow(ru_ne), nrow(ru_ne)))
cat(sprintf("   pass rush  %2d members -> %2d rows\n",
            nrow(pa_ne), nrow(pa_ne)))
cat(sprintf("   run defense %2d members -> %2d rows\n",
            nrow(ra_ne), nrow(ra_ne)))
stopifnot(nrow(rec_ne) > 0, nrow(sec_ne) > 0, nrow(pb_ne) > 0,
          nrow(rb_ne) > 0, nrow(ru_ne) > 0, nrow(pa_ne) > 0,
          nrow(ra_ne) > 0)

ord_rec <- rec_ne %>% arrange(desc(zg_f)) %>% pull(roster_name)
ord_sec <- sec_ne %>% filter(split == "zone") %>%
  arrange(desc(grade_f)) %>% pull(roster_name)
ord_ru  <- ru_ne %>% arrange(desc(gf)) %>% pull(roster_name)
ord_pa  <- pa_ne %>% arrange(desc(gf)) %>% pull(roster_name)
ord_ra  <- ra_ne %>% arrange(desc(grade_f)) %>% pull(roster_name)
# secondary: a man-only seat (not seated vs zone) sorts after
# the zone-seated players, by his man value
ord_sec <- c(ord_sec,
             sec_ne %>% filter(split == "man",
                               !roster_name %in% ord_sec) %>%
               arrange(desc(grade_f)) %>% pull(roster_name))

lens_levels_neo <- c("man", "gap", "overall", "zone")

rec_long <- bind_rows(
  rec_ne %>% transmute(unit = "Receiving", player = roster_name,
                       lens = "man", role = band, avail,
                       bef = mg_f, aft = mg_p,
                       abef = c3mg_f, aaft = c3mg_p,
                       uw = usage_w),
  rec_ne %>% transmute(unit = "Receiving", player = roster_name,
                       lens = "zone", role = band, avail,
                       bef = zg_f, aft = zg_p,
                       abef = c3zg_f, aaft = c3zg_p,
                       uw = usage_w)) %>%
  mutate(pord = match(player, ord_rec))

sec_long <- sec_ne %>%
  transmute(unit = "Secondary", player = roster_name,
            lens = split, role = band, avail,
            bef = grade_f, aft = grade_p,
            abef = c3_f, aaft = adj_p, uw = usage_w) %>%
  mutate(pord = match(player, ord_sec))

pb_long <- pb_ne %>%
  transmute(unit = "Pass block", player = roster_name,
            lens = "overall", role = slot, avail,
            bef = gf_raw, aft = gf_raw_av,
            abef = gf_adj, aaft = gf_adj_av,
            uw = NA_real_) %>%
  mutate(pord = match(role, ol_pos_levels))

rb_long <- bind_rows(
  rb_ne %>% transmute(unit = "Run block", player = roster_name,
                      lens = "gap", role = slot, avail,
                      bef = V_gap, aft = V_gap_av,
                      abef = V_c3, aaft = V_c3_av,
                      uw = NA_real_),
  rb_ne %>% transmute(unit = "Run block", player = roster_name,
                      lens = "zone", role = slot, avail,
                      bef = V_zone, aft = V_zone_av,
                      abef = V_c3, aaft = V_c3_av,
                      uw = NA_real_)) %>%
  mutate(pord = match(role, ol_pos_levels))

ru_long <- ru_ne %>%
  transmute(unit = "Rushing", player = roster_name,
            lens = "overall", role = "committee", avail,
            bef = gf, aft = gf_av,
            abef = V_c3, aaft = V_c3_av, uw = uw) %>%
  mutate(pord = match(player, ord_ru))

pa_long <- pa_ne %>%
  transmute(unit = "Pass rush", player = roster_name,
            lens = "overall", role = band, avail,
            bef = gf, aft = gf_p,
            abef = c3_f, aaft = c3_p, uw = uw) %>%
  mutate(pord = match(player, ord_pa))

ra_long <- ra_ne %>%
  transmute(unit = "Run defense", player = roster_name,
            lens = "overall", role = band, avail,
            bef = grade_f, aft = grade_p,
            abef = c3_f, aaft = c3_p, uw = usage_w) %>%
  mutate(pord = match(player, ord_ra))

ne_own_long <- bind_rows(rec_long, sec_long, pb_long, rb_long,
                         ru_long, pa_long, ra_long) %>%
  mutate(unit = factor(unit,
                       levels = c("Receiving", "Rushing",
                                  "Pass block", "Run block",
                                  "Pass rush", "Secondary",
                                  "Run defense")),
         lens = factor(lens, levels = lens_levels_neo),
         emiss = (1 - avail) * 17,
         d  = aft  - bef,
         ad = aaft - abef) %>%
  arrange(unit, pord, lens) %>%
  mutate(new26 = cl_neo(player) %in% new26_keys,
         player = dplyr::if_else(new26, paste0(player, " *"),
                                 player))

stopifnot(!any(is.na(ne_own_long$pord)))

# name-match receipt: table players (non-phantom) who did not
# match the nflreadr 2026 NE roster get no star -- timing or
# format drift, printed for eyeballs, never silent
unm_neo <- ne_own_long %>%
  filter(!grepl("^PHANTOM", player)) %>%
  distinct(player) %>%
  filter(!cl_neo(player) %in% ros26_keys)
if (nrow(unm_neo) > 0) {
  cat("\n-- name-match receipt: no 2026 NE roster match (no",
      "star applied):\n")
  print(unm_neo$player)
}

# NA law: every cell is filled except pass-block RAW, where the
# canon box-score holes are inherited on purpose ('--' at print)
na_chk_neo <- ne_own_long %>%
  summarise(n_bad = sum(is.na(bef) & unit != "Pass block") +
              sum(is.na(aft) & unit != "Pass block") +
              sum(is.na(abef)) + sum(is.na(aaft)) +
              sum(is.na(avail)))
stopifnot(na_chk_neo$n_bad == 0)
stopifnot(all(ne_own_long$avail >= 0 & ne_own_long$avail <= 1))

# ------------------------------------------------------------
# 2. CONSOLE SUMMARY -- the unit-level answer in seven lines,
#    before the player-level table. Weights: usage where the
#    unit carries it, flat across the five OL slots where it
#    does not (a line plays or sits together).
# ------------------------------------------------------------

sum_neo <- function(df, w = NULL, label) {
  b <- if (is.null(w)) mean(df$bef, na.rm = TRUE) else
    weighted.mean(df$bef, w = df[[w]], na.rm = TRUE)
  a <- if (is.null(w)) mean(df$aft, na.rm = TRUE) else
    weighted.mean(df$aft, w = df[[w]], na.rm = TRUE)
  bg <- sum(17 * (1 - df$avail))
  cat(sprintf("   %-10s before %.3f -> after %.3f (%s) | exp backup gms %.1f\n",
              label, b, a,
              if (is.null(w)) "flat across slots" else "usage-weighted",
              bg))
}

cat("\n-- what injuries cost each NE unit, headline lens --\n")
sum_neo(rec_ne %>% mutate(bef = zg_f, aft = zg_p), "usage_w",
        "Receiving (zone)")
sum_neo(ru_ne %>% mutate(bef = gf, aft = gf_av), "uw",
        "Rushing")
sum_neo(pb_ne %>% mutate(bef = gf_adj, aft = gf_adj_av), NULL,
        "Pass block (adj)")
sum_neo(rb_ne %>% mutate(bef = V_zone, aft = V_zone_av), NULL,
        "Run block (zone)")
sum_neo(pa_ne %>% mutate(bef = gf, aft = gf_p), "uw",
        "Pass rush")
sum_neo(sec_ne %>% filter(split == "zone") %>%
          mutate(bef = grade_f, aft = grade_p), "usage_w",
        "Secondary (zone)")
sum_neo(ra_ne %>% mutate(bef = grade_f, aft = grade_p),
        "usage_w", "Run defense")
cat("   (secondary backup games are per-split seats; the zone\n")
cat("    committee's number prints above, man's is in the file\n")
sec_bg_man <- sec_ne %>% filter(split == "man") %>%
  summarise(bg = sum(17 * (1 - avail))) %>% pull(bg)
cat(sprintf("    -- man committee: exp backup gms %.1f)\n", sec_bg_man))

# ------------------------------------------------------------
# 3. THE TABLE -- one gt, seven unit groups. Question in the
#    title, direction and the color flip in the subtitle.
# ------------------------------------------------------------

gt_neo <- ne_own_long %>%
  select(unit, player, lens, role, avail, emiss,
         bef, aft, d, abef, aaft, ad) %>%
  gt(groupname_col = "unit", rowname_col = "player") %>%
  tab_header(
    title = paste0("What do injuries cost NEW ENGLAND'S own ",
                   "units in 2026? \u2014 before and after"),
    subtitle = paste0(
      "before = everyone plays to his own history | after = ",
      "priced by that history (misses route to the measured ",
      "backup level) | OUR side now: red = NE loses quality, ",
      "the colors flip from the opponent tables | '--' = canon ",
      "box-score hole | run block's adjusted value is ",
      "scheme-neutral and repeats on both rows | grade lens ",
      "throughout \u2014 YPRR / suppression / MTF live in the ",
      "unit boards | * = new to NE in 2026 (the churn board, ",
      "section 0c)")) %>%
  tab_spanner(label = "availability", columns = c(avail, emiss)) %>%
  tab_spanner(label = "Raw value", columns = c(bef, aft, d)) %>%
  tab_spanner(label = "Adjusted (same-slate)",
              columns = c(abef, aaft, ad)) %>%
  cols_label(lens = "lens", role = "role",
             avail = "avail", emiss = "exp miss (g)",
             bef = "before", aft = "after", d = "\u0394",
             abef = "before", aaft = "after", ad = "\u0394") %>%
  fmt_percent(columns = c(avail, bef, aft, d, abef, aaft, ad),
              decimals = 0) %>%
  fmt_percent(columns = c(d, ad), decimals = 1,
              force_sign = TRUE) %>%
  fmt_number(columns = emiss, decimals = 1) %>%
  sub_missing(missing_text = "--") %>%
  data_color(columns = c(d, ad),
             fn = scales::col_numeric(
               palette = c("#C60C30", "#f7f7f7", "#6baed6"),
               domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
             autocolor_text = TRUE) %>%
  tab_options(table.font.size = px(12),
              data_row.padding = px(3),
              column_labels.font.weight = "bold",
              row_group.font.weight = "bold")

print(gt_neo)

cat("\n== NE OWN-UNITS TABLE BUILT ==\n")
cat("one table, seven units. Recall with: print(gt_neo)\n")
# gtsave("ne_own_units_before_after.png", gt_neo)  # uncomment to save

# ------------------------------------------------------------
# 4. STATUS -- ALL SEVEN UNITS LIVE. Run defense joined
#    2026-08-22 off the new run-defense availability layer;
#    with it, the pipeline's unit set is complete:
#    receiving / rushing / pass block / run block / pass rush /
#    secondary / run defense.
# ------------------------------------------------------------

cat("\n-- checkpoint: ne_own_long (", nrow(ne_own_long),
    " rows) + gt_neo in session --\n", sep = "")

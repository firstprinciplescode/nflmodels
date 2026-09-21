# data_check.R
# PROPOSED -- Claude, UNSIGNED, 2026-09-20. New file. READ-ONLY on the data: it reads cache/*.rds and writes one text log.
#
# Run from the Terminal (not RStudio), ~1 minute:
#   "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" util/data_check.R
# Log: cache/data_check_<date>.txt   -- every line starts PASS or FLAG. A FLAG is something to look at, not always an error.
#
# What it checks on every model frame in cache/:
#   weeks      every season 2016-2025 has every regular-season week and playoff weeks 28 / 29 / 30 / 32 with the right number of team-games
#   keys       no duplicated rows on the frame's own key
#   NA         share of NA per key column, season by season -- a season that is far off the others is flagged (how the dead 2025 part_ data showed up)
#   level      season mean of the key stats -- a season more than 15% off the ten-season mean is flagged (how the stale combined_pbp showed up: xTD 25% low)
#   sense      roles read the way football does: WR1 > WR2 > ... > WR5 in routes and targets, lead backs > committee > spot backs, shares add to 1
#   cross      every team-game in qb_stats_df_final has receiving rows and rushing rows

suppressPackageStartupMessages({library(dplyr); library(tidyr)})
options(width = 220, dplyr.summarise.inform = FALSE)
REPO <- "C:/Users/vflre/Downloads/nflmodels_UPDATE"; setwd(REPO)
LOG <- file.path("cache", paste0("data_check_", format(Sys.Date(), "%Y-%m-%d"), ".txt"))
con <- file(LOG, open = "wt"); n_flag <- 0
say  <- function(...) { s <- paste0(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = con) }
res  <- function(ok, what, detail = "") { if (!ok) n_flag <<- n_flag + 1; say(if (ok) "PASS  " else "FLAG  ", what, if (nzchar(detail)) paste0("  --  ", detail) else "") }
show <- function(d) { x <- capture.output(print(as.data.frame(d), row.names = FALSE, digits = 3)); for (l in x) say("        ", l) }
rd   <- function(n) as.data.frame(ungroup(readRDS(file.path("cache", paste0(n, ".rds")))))
say("DATA CHECK  ", format(Sys.time(), "%Y-%m-%d %H:%M"), "   repo cache: ", file.path(REPO, "cache"))

REG_WEEKS <- function(s) if (s <= 2020) 1:17 else 1:18
PO_TG     <- function(s) c(`28` = if (s <= 2019) 8 else 12, `29` = 8, `30` = 4, `32` = 2)

check_weeks <- function(x, name, team_col) {
  bad <- character(0)
  for (s in 2016:2025) { xs <- x[x$season == s, ]
    miss <- setdiff(c(REG_WEEKS(s), 28, 29, 30, 32), unique(xs$week)); if (length(miss)) bad <- c(bad, sprintf("%d missing week %s", s, paste(miss, collapse = "/")))
    extra <- setdiff(unique(xs$week), c(REG_WEEKS(s), 28, 29, 30, 32)); if (length(extra)) bad <- c(bad, sprintf("%d has unexpected week %s", s, paste(extra, collapse = "/")))
    po <- PO_TG(s); for (w in names(po)) { n <- length(unique(xs[[team_col]][xs$week == as.integer(w)])); if (n != po[[w]]) bad <- c(bad, sprintf("%d wk %s: %d team-games, expected %d", s, w, n, po[[w]])) }
    if ("def_ssn" %in% names(xs)) { reg <- xs[xs$week <= 18 & !is.na(xs$def_ssn), ]; g <- tapply(reg$week, reg$def_ssn, function(w) length(unique(w))); full <- length(REG_WEEKS(s)) - 1
      odd <- g[!g %in% c(full, full - 1)]                                   # 16 (17 from 2021); one short = the cancelled BUF-CIN game in 2022
      if (length(g) != 32) bad <- c(bad, sprintf("%d: %d defenses, expected 32", s, length(g)))
      if (length(odd)) bad <- c(bad, sprintf("%d: regular-season games per team off for %s", s, paste(names(odd), odd, sep = "=", collapse = " "))) }
  }
  if (max(x$season) > 2025) bad <- c(bad, paste("season", max(x$season), "present (2026 is quarantined)"))
  res(!length(bad), paste0(name, ": all weeks, all seasons 2016-2025"), paste(utils::head(bad, 6), collapse = "; "))
}
check_na <- function(x, name, cols, tol = 0.05) {
  cols <- intersect(cols, names(x)); if (!length(cols)) return(invisible())
  t <- x %>% group_by(season) %>% summarise(across(all_of(cols), ~ mean(is.na(.)))) %>% as.data.frame()
  bad <- character(0)
  for (cl in cols) { v <- t[[cl]]; m <- stats::median(v); off <- which(abs(v - m) > tol)
    if (length(off)) bad <- c(bad, sprintf("%s: NA share %s in %s (other seasons %.3f)", cl, paste(sprintf("%.3f", v[off]), collapse = "/"), paste(t$season[off], collapse = "/"), m)) }
  res(!length(bad), paste0(name, ": NA share steady across seasons (", length(cols), " columns; worst column ", sprintf("%.3f", max(colMeans(is.na(x[cols])))), " NA overall)"), paste(bad, collapse = "; "))
}
check_level <- function(x, name, cols, tol = 0.15) {
  cols <- intersect(cols, names(x)); if (!length(cols)) return(invisible())
  t <- x %>% group_by(season) %>% summarise(across(all_of(cols), ~ mean(., na.rm = TRUE))) %>% as.data.frame()
  bad <- character(0)
  for (cl in cols) { v <- t[[cl]]; m <- mean(v, na.rm = TRUE); off <- which(abs(v / m - 1) > tol | !is.finite(v))
    if (length(off)) bad <- c(bad, sprintf("%s: %s in %s vs ten-season mean %.3f", cl, paste(sprintf("%.3f", v[off]), collapse = "/"), paste(t$season[off], collapse = "/"), m)) }
  res(!length(bad), paste0(name, ": season means within 15% of the ten-season mean (", paste(cols, collapse = ", "), ")"), paste(bad, collapse = "; "))
  invisible(t)
}
check_dups <- function(x, name, key) { d <- sum(duplicated(x[key])); res(d == 0, paste0(name, ": no duplicate rows on ", paste(key, collapse = " + ")), if (d) paste(d, "duplicated rows") else "") }

# ------------------------------------------------------------------ qb_stats_df_final + xtd_proportion (team-game)
say("\n===== qb_stats_df_final  (", format(file.mtime("cache/qb_stats_df_final.rds"), "%m-%d %H:%M"), ")")
q <- rd("qb_stats_df_final"); say("        ", nrow(q), " x ", ncol(q))
check_weeks(q, "qb_stats_df_final", "qbgrp_ssn"); check_dups(q, "qb_stats_df_final", c("qbgrp_ssn", "def_ssn", "week", "season"))
qc <- c("plays", "pass_rate", "fastr_xpass_rate", "pbp_xpass_rate", "part_xpass_rate", "acc_rate", "fastr_cp", "pbp_cp", "part_cp", "ypa", "pbp_xypa", "part_xypa",
        "sack_rate", "pbp_sack_rate", "part_sack_rate", "pbp_xtds", "part_xtds", "tds", "twp_rate", "int_rate", "scr_rate", "fgs")
check_na(q, "qb_stats_df_final", qc); lv <- check_level(q, "qb_stats_df_final", setdiff(qc, c("int_rate", "twp_rate", "scr_rate", "sack_rate", "pbp_sack_rate", "part_sack_rate")))
rk <- grep("_rank$|_rank_def$", names(q), value = TRUE); rr <- vapply(q[rk], function(v) all(v >= 0 & v <= 1, na.rm = TRUE), TRUE)
res(all(rr), paste0("qb_stats_df_final: all ", length(rk), " rank columns sit inside 0..1"),
    paste(vapply(names(rr)[!rr], function(cl) { v <- q[[cl]]; b <- which(v < 0 | v > 1); sprintf("%s: %d rows outside (range %.3f to %.3f; e.g. %s wk %s %s)", cl, length(b), min(v, na.rm = TRUE), max(v, na.rm = TRUE),
                                                                                        q$def_ssn[b[1]], q$week[b[1]], q$season[b[1]]) }, ""), collapse = "; "))
res(abs(sum(q$pbp_xtds) / sum(q$part_xtds) - 1) < 0.05, "qb_stats_df_final: pbp xTD and part xTD totals agree", sprintf("pbp %.0f vs part %.0f", sum(q$pbp_xtds), sum(q$part_xtds)))
say("        season means:"); show(lv[, c("season", intersect(c("plays", "pass_rate", "ypa", "pbp_xtds", "part_xtds", "tds", "fgs"), names(lv)))])

say("\n===== xtd_proportion  (", format(file.mtime("cache/xtd_proportion.rds"), "%m-%d %H:%M"), ")")
xp <- rd("xtd_proportion"); check_weeks(xp, "xtd_proportion", "qbgrp_ssn"); check_dups(xp, "xtd_proportion", c("qbgrp_ssn", "def_ssn", "week", "season"))
pc <- c("pbp_pass_prop", "part_pass_prop", "pbp_qb_scramble_prop", "part_qb_scramble_prop", "pbp_run_prop", "part_run_prop"); check_na(xp, "xtd_proportion", pc)
check_level(xp, "xtd_proportion", c("pbp_pass_prop", "part_pass_prop", "pbp_run_prop", "part_run_prop"))
res(all(xp %>% group_by(season) %>% summarise(m = mean(part_qb_scramble_prop, na.rm = TRUE)) %>% pull(m) > 0.005), "xtd_proportion: part scramble share is alive in every season (the dead copy read 0)")
s1 <- with(xp, pbp_pass_prop + pbp_qb_scramble_prop + pbp_run_prop); res(all(abs(s1 - 1) < 0.02, na.rm = TRUE), "xtd_proportion: pass + scramble + run share add to 1 in every game", sprintf("worst %.3f", max(abs(s1 - 1), na.rm = TRUE)))

# ------------------------------------------------------------------ receiving_func_base (player-game)
say("\n===== receiving_func_base  (", format(file.mtime("cache/receiving_func_base.rds"), "%m-%d %H:%M"), ")")
r <- rd("receiving_func_base"); say("        ", nrow(r), " x ", ncol(r))
check_weeks(r, "receiving_func_base", "abbreviation"); check_dups(r, "receiving_func_base", c("player_id", "abbreviation", "week", "season"))
dk <- r[duplicated(r[c("player_id", "abbreviation", "week", "season")]) | duplicated(r[c("player_id", "abbreviation", "week", "season")], fromLast = TRUE), ]
if (nrow(dk)) { say("        the duplicated player-games:"); show(dk %>% select(player, abbreviation, week, season, qbgrp_ssn, snap_counts_pass_route, targets, receiver_player_name) %>% arrange(player, season, week)) }
res(!any(grepl("\\.(x|y)$", names(r)) & !names(r) %in% c("game_id.x", "game_id.y", "old_game_id.x", "player.x", "player.y")), "receiving_func_base: no surprise .x / .y columns (weather not double-joined)",
    paste(grep("\\.(x|y)$", names(r), value = TRUE), collapse = ", "))
check_na(r, "receiving_func_base", c("qbgrp_ssn", "def_ssn", "final_position_group", "tgt_cluster_name", "rte_cluster_name", "align_cluster_name", "man_zone_grp_cluster", "xpass_percentile", "xtd_percentile",
                                     "targets", "tgt_share", "pbp_rec_xtds", "part_rec_xtds", "part_cp", "part_xypa", "onfield_perc"))
check_level(r %>% filter(targets > 0), "receiving_func_base (targeted games)", c("ypa", "pbp_xypa", "part_xypa", "acc_rate", "pbp_cp", "part_cp"))
tg <- r %>% group_by(abbreviation, week, season) %>% summarise(sh = sum(tgt_share, na.rm = TRUE), xs = sum(pbp_xtds_share, na.rm = TRUE), px = sum(part_xtds_share, na.rm = TRUE))
res(mean(abs(tg$sh - 1) < 0.02) > 0.97, "receiving_func_base: target shares add to 1 per team-game", sprintf("%.1f%% of team-games within 0.02", 100 * mean(abs(tg$sh - 1) < 0.02)))
res(mean(abs(tg$px - 1) < 0.02, na.rm = TRUE) > 0.97, "receiving_func_base: part xTD shares add to 1 per team-game", sprintf("%.1f%% of team-games within 0.02", 100 * mean(abs(tg$px - 1) < 0.02, na.rm = TRUE)))
role <- r %>% filter(final_position_group == "WR", pos_rank <= 6) %>% group_by(wr_rank = pos_rank) %>%
  summarise(player_games = n(), routes = mean(snap_counts_pass_route), onfield = mean(onfield_perc, na.rm = TRUE), targets = mean(targets), tgt_share = mean(tgt_share), xtd_share = mean(pbp_xtds_share))
mono <- function(v) all(diff(v) < 0)
res(mono(role$routes) && mono(role$tgt_share) && mono(role$xtd_share), "receiving sense: WR1 > WR2 > ... > WR6 in routes, target share and xTD share")
res(role$tgt_share[role$wr_rank == 5] < 0.05 && role$tgt_share[role$wr_rank == 1] > 0.20, "receiving sense: WR5 is small and WR1 is big",
    sprintf("WR1 %.1f%% of targets, WR5 %.1f%%", 100 * role$tgt_share[1], 100 * role$tgt_share[5])); show(role)
role25 <- r %>% filter(final_position_group == "WR", pos_rank <= 6, season == 2025, week >= 28) %>% group_by(wr_rank = pos_rank) %>% summarise(player_games = n(), routes = mean(snap_counts_pass_route), tgt_share = mean(tgt_share))
res(mono(role25$routes), "receiving sense: the same order holds inside the 2025 playoff weeks (28/29/30/32)"); show(role25)
grp <- r %>% filter(!is.na(final_position_group)) %>% group_by(final_position_group) %>% summarise(player_games = n(), routes = mean(snap_counts_pass_route), tgt_share = mean(tgt_share), adot = mean(adot, na.rm = TRUE), ypa = mean(ypa, na.rm = TRUE))
res(grp$adot[grp$final_position_group == "WR"] > grp$adot[grp$final_position_group == "TE"] && grp$adot[grp$final_position_group == "TE"] > grp$adot[grp$final_position_group == "BACK"],
    "receiving sense: depth of target reads WR > TE > BACK"); show(grp)

# ------------------------------------------------------------------ rushing (player-game + the three team-game frames)
say("\n===== rush_stats_final  (", format(file.mtime("cache/rush_stats_final.rds"), "%m-%d %H:%M"), ")")
u <- rd("rush_stats_final"); say("        ", nrow(u), " x ", ncol(u))
check_weeks(u, "rush_stats_final", "qbgrp_ssn"); check_dups(u, "rush_stats_final", c("rusher_player_id", "qbgrp_ssn", "def_ssn", "week", "season"))
check_na(u, "rush_stats_final", c("team", "position_group", "rank_grp", "attempts", "ypc", "pbp_xypc", "part_xypc", "pbp_xtd", "part_xtd", "situation_cluster", "gap_cluster", "xtd_percentile", "gap_z", "rush_share", "part_xtd_share"))
check_level(u %>% filter(position_group == "HB"), "rush_stats_final (backs)", c("ypc", "pbp_xypc", "part_xypc", "ybc", "yac"))
xt <- u %>% group_by(season) %>% summarise(pbp = sum(pbp_xtd), part = sum(part_xtd, na.rm = TRUE), ratio = part / pbp)
res(all(xt$ratio > 0.90 & xt$ratio < 1.10), "rush_stats_final: part xTD total within 10% of pbp xTD total in every season", sprintf("ratio range %.3f to %.3f", min(xt$ratio), max(xt$ratio)))
sh <- u %>% group_by(qbgrp_ssn, def_ssn, week, season) %>% summarise(s = sum(rush_share, na.rm = TRUE)); res(mean(abs(sh$s - 1) < 0.02) > 0.97, "rush_stats_final: rush shares add to 1 per team-game", sprintf("%.1f%% within 0.02", 100 * mean(abs(sh$s - 1) < 0.02)))
rg <- u %>% filter(position_group == "HB", !is.na(rank_grp)) %>% group_by(rank_grp) %>% summarise(player_games = n(), carries = mean(attempts, na.rm = TRUE), rush_share = mean(rush_share, na.rm = TRUE), xtd_share = mean(pbp_xtd_share, na.rm = TRUE), ypc = mean(ypc))
res(mono(rg$carries) && mono(rg$rush_share) && rg$rush_share[rg$rank_grp == "A"] > 0.5 && rg$rush_share[rg$rank_grp == "C"] < 0.15, "rushing sense: lead backs (A) > committee (B) > spot backs (C) in carries and share"); show(rg)
ng <- u %>% filter(!is.na(situation_cluster)) %>% group_by(rank_grp) %>% summarise(sit = n_distinct(situation_cluster), gap = n_distinct(gap_cluster[!is.na(gap_cluster)]))
res(identical(ng$sit, c(4L, 5L, 4L)) && identical(ng$gap, c(8L, 4L, 4L)), "rush_stats_final: cluster groups are 4 / 5 / 4 situation and 8 / 4 / 4 gap (A / B / C), the numbering the label notes describe", paste(ng$rank_grp, ng$sit, ng$gap, collapse = " | "))
qb <- u %>% filter(position_group == "QB") %>% summarise(share = mean(rush_share, na.rm = TRUE), ypc = mean(ypc)); res(qb$share < 0.20, "rushing sense: quarterback designed runs are a small share", sprintf("mean share %.3f", qb$share))
for (nm in c("rush_stats_high", "rush_stats_low", "rush_stats_rec")) if (file.exists(file.path("cache", paste0(nm, ".rds")))) { h <- rd(nm)
  say("\n===== ", nm, "  (", format(file.mtime(file.path("cache", paste0(nm, ".rds"))), "%m-%d %H:%M"), ")   ", nrow(h), " x ", ncol(h))
  po <- vapply(2016:2025, function(s) all(c(30, 32) %in% h$week[h$season == s]), TRUE); res(all(po) || nm == "rush_stats_rec", paste0(nm, ": weeks 30 and 32 present in every season"), paste(c(2016:2025)[!po], collapse = ","))
  check_dups(h, nm, c("off_ssn", "def_ssn", "week", "season")); check_na(h, nm, c("ypc", "pbp_xypc", "part_xypc", "pbp_xtd", "part_xtd", "part_xtd_rank", "part_xypc_rank_def")) }

# ------------------------------------------------------------------ cross-frame
say("\n===== cross-frame")
k  <- function(d, t = "qbgrp_ssn") unique(paste(d[[t]], d$def_ssn, d$week, d$season))
kq <- k(q); res(all(kq %in% k(u)), "every qb_stats_df_final team-game has rushing rows", paste(sum(!kq %in% k(u)), "team-games without"))
res(all(kq %in% k(r)), "every qb_stats_df_final team-game has receiving rows", paste(sum(!kq %in% k(r)), "team-games without"))
res(all(k(u) %in% kq) && all(k(r[!is.na(r$qbgrp_ssn), ]) %in% kq), "no rushing / receiving team-game is missing from qb_stats_df_final")
wx <- q %>% filter(season == 2025, week >= 28) %>% summarise(n = n(), temp_na = sum(is.na(temp)), wind_na = sum(is.na(wind))); res(wx$n == 26, "qb_stats_df_final: 26 team-games in the 2025 playoffs", sprintf("%d rows; temp NA %d, wind NA %d", wx$n, wx$temp_na, wx$wind_na))

say("\n===== combined_pbp_GOOD_2026-09-19  (", format(file.mtime("cache/combined_pbp_GOOD_2026-09-19.rds"), "%m-%d %H:%M"), ")")
p <- readRDS("cache/combined_pbp_GOOD_2026-09-19.rds"); say("        ", nrow(p), " x ", ncol(p))
res("part_predicted_after_scramble_xtd" %in% names(p), "combined_pbp: the good build (scramble-xTD columns present)")
p <- as.data.frame(p)[, intersect(c("season", "week", "posteam", "qbgrp_ssn", "def_ssn", "play_type", "pass_attempt", "sack", "rush", "qb_scramble", "qb_kneel", "air_yards",
                                    "part_predicted_cp", "pbp_predicted_cp", "part_predicted_ypc", "pbp_predicted_ypc", "part_predicted_after_pass_xtd", "part_predicted_after_run_xtd"), names(p))]; invisible(gc())
check_weeks(p, "combined_pbp", "posteam")
pa <- p %>% filter(play_type == "pass", pass_attempt == 1, sack == 0, !is.na(air_yards)); check_na(pa, "combined_pbp pass attempts", c("pbp_predicted_cp", "part_predicted_cp", "part_predicted_after_pass_xtd", "qbgrp_ssn", "def_ssn"))
ru <- p %>% filter(rush == 1, qb_scramble == 0, qb_kneel == 0); check_na(ru, "combined_pbp designed runs", c("pbp_predicted_ypc", "part_predicted_ypc", "part_predicted_after_run_xtd", "qbgrp_ssn", "def_ssn"))
ppg <- p %>% group_by(season) %>% summarise(plays_per_team_game = n() / n_distinct(paste(posteam, week))); res(all(abs(ppg$plays_per_team_game / mean(ppg$plays_per_team_game) - 1) < 0.08), "combined_pbp: plays per team-game steady across seasons",
    sprintf("%.1f to %.1f", min(ppg$plays_per_team_game), max(ppg$plays_per_team_game)))

say("\n===== RESULT: ", n_flag, " FLAG", if (n_flag != 1) "S" else "", "   log written to ", LOG)
close(con)

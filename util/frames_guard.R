# frames_guard.R
# PROPOSED -- Claude, UNSIGNED. NEW file 2026-09-20. Changes nothing that exists.
#
# WHY: the three key frames (receiving_func_base, rush_stats_final, qb_stats_df_final) kept 'going back to missing data' and
# receiver_registry kept vanishing. Cause (traced + independently verified 2026-09-20): RStudio silently restores
# C:/Users/vflre/OneDrive/Documents/.RData (2026-01-27, the JANUARY frames, no registry) at every start, and save.image()
# onto one file name then freezes those January frames over the good workspace. Nothing in the repo drops playoff weeks.
#
# Sourcing this file defines functions ONLY. Nothing is read, loaded, written or changed until you call one.
#   frames_check()                    one line per key frame in the session: GOOD / STALE and why (rows, 2025 playoff weeks,
#                                     every season-week against the cache copy, duplicated weather columns)
#   frames_restore("rush_stats_final")  puts the known-good cache copy into the session, ON PURPOSE (old one kept as <name>_OLD)
#   safe_load(file, take = c(...))    load() into a side environment; copies only what you name or what is missing
#   safe_save_image()                 refuses to freeze a STALE frame; writes a NEW dated file, never over an older one
#   registry_keep() / registry_back() receiver_registry to / from cache/receiver_registry.rds (keep MERGES, never shrinks)
# Part 1 below is the traced draft; part 2 is the verifier's tested corrections (they replace three definitions from part 1).

# ===================== part 1 =====================
# ============================================================================
# frames_guard_DRAFT.R  --  PROPOSED, Claude, UNSIGNED, 2026-09-20.  DRAFT: lives in the scratchpad, not in the repo.
#
# WHAT IT IS: four functions. Sourcing this file defines them and does NOTHING else --
#   no readRDS, no load, no rm, no assignment to any of your frames. Every action happens
#   only when YOU call one of them.
#
#   frames_check()                       one line per key frame: rows x cols, seasons, 2025 playoff weeks,
#                                        the cache copy next to it, and GOOD / STALE / NOT IN SESSION.
#                                        Reads the cache copy into a private variable to compare; never
#                                        puts it in the session.
#   frames_restore("rush_stats_final")   YOU call it on purpose. Reads cache/<name>.rds over the session copy.
#                                        The copy it replaces is kept as <name>_STALE_<mmdd_HHMM> (keep_old = FALSE to skip).
#   safe_load("some_workspace.RData")    replacement for load(). Loads into a PRIVATE environment, copies in only
#                                        objects that are MISSING from the session, plus any you name in take = .
#                                        Never replaces an object you already have unless you name it.
#                                        The private environment is dropped at the end (nothing like `e` left behind
#                                        for save.image to swallow).
#   safe_save_image()                    replacement for save.image(). Runs frames_check() first and REFUSES if a key
#                                        frame is STALE; writes a NEW date-stamped file, never on top of an old one;
#                                        leaves out environment objects (the `e` that made the 09-19 file 11 GB).
#
# WHY (evidence, 2026-09-20): RStudio starts in ~ (= C:/Users/vflre/OneDrive/Documents) and auto-restores
#   ~/.RData from 2026-01-27 -- 257 objects, with receiving_func_base 58,314 rows (no wk 30/32),
#   rush_stats_final 20,388 rows (no wk 30/32), qb_stats_df_final 5,520 x 210 (no wk 32, 18 columns short).
#   All 257 of those names are in the top level of the 09-19 16:20 everything-workspace. save.image() then
#   froze them. See the trace report for the console-history timeline.
# ============================================================================

FRAMES_REPO  <- "C:/Users/vflre/Downloads/nflmodels_UPDATE"
FRAMES_KEY   <- c("receiving_func_base", "rush_stats_final", "qb_stats_df_final")
FRAMES_WX    <- c("temp", "wind", "rain_ind", "snow_ind")      # the weather columns the *_func files join on AFTER the build
FRAMES_PO_WK <- c(28, 29, 30, 32)                              # 2025 playoff weeks a complete frame carries

.frames_cache_file <- function(nm) file.path(FRAMES_REPO, "cache", paste0(nm, ".rds"))

.frames_wk25 <- function(x) {
  if (!is.data.frame(x) || !all(c("week", "season") %in% names(x))) return(integer(0))
  sort(unique(x$week[which(x$season == 2025 & x$week >= 28)]))
}

# verdict for ONE data frame against the cache copy (ref may be NULL when there is no cache file)
.frames_verdict <- function(x, ref) {
  why <- character(0)
  miss_wk <- setdiff(FRAMES_PO_WK, .frames_wk25(x))
  if (length(miss_wk)) why <- c(why, paste0("2025 week ", paste(miss_wk, collapse = "/"), " missing"))
  if (any(c(paste0(FRAMES_WX, ".x"), paste0(FRAMES_WX, ".y")) %in% names(x)))
    why <- c(why, "weather joined TWICE (temp.x / temp.y) -- restore, then run the weather join once")
  if (!is.null(ref)) {
    if (nrow(x) < nrow(ref)) why <- c(why, sprintf("%d rows short of cache", nrow(ref) - nrow(x)))
    gone <- setdiff(names(ref), names(x))
    if (length(gone)) why <- c(why, sprintf("%d cache columns missing (%s%s)", length(gone), paste(utils::head(gone, 3), collapse = ", "), if (length(gone) > 3) ", ..." else ""))
  }
  why
}

frames_check <- function(names = FRAMES_KEY, envir = .GlobalEnv, quiet = FALSE) {
  out <- list()
  for (nm in names) {
    f <- .frames_cache_file(nm)
    ref <- if (file.exists(f)) as.data.frame(dplyr::ungroup(readRDS(f))) else NULL      # private; never enters the session
    ref_txt <- if (is.null(ref)) "no cache copy" else sprintf("cache %d x %d, saved %s", nrow(ref), ncol(ref), format(file.mtime(f), "%m-%d %H:%M"))
    if (!exists(nm, envir = envir, inherits = FALSE)) {
      out[[nm]] <- data.frame(frame = nm, status = "NOT IN SESSION", rows = NA, cols = NA, seasons = "", po_wk_2025 = "", detail = ref_txt)
      next
    }
    x <- get(nm, envir = envir, inherits = FALSE)
    if (!is.data.frame(x)) { out[[nm]] <- data.frame(frame = nm, status = "NOT A FRAME", rows = NA, cols = NA, seasons = "", po_wk_2025 = "", detail = class(x)[1]); next }
    why <- .frames_verdict(x, ref)
    wx  <- if (nm != "qb_stats_df_final" && !all(FRAMES_WX %in% names(x)) && !length(why)) "  [weather not joined yet]" else ""
    out[[nm]] <- data.frame(frame = nm, status = if (length(why)) "STALE" else "GOOD", rows = nrow(x), cols = ncol(x),
                            seasons = if ("season" %in% names(x)) paste(range(x$season, na.rm = TRUE), collapse = "-") else "",
                            po_wk_2025 = paste(.frames_wk25(x), collapse = ","),
                            detail = paste0(if (length(why)) paste0(paste(why, collapse = "; "), "  | ") else "", ref_txt, wx))
  }
  res <- do.call(rbind, out); rownames(res) <- NULL
  if (!quiet) {
    cat(sprintf("\nframes_check  %s   (GOOD = 2025 weeks 28,29,30,32 present, not shorter than cache, no cache column missing)\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
    for (i in seq_len(nrow(res))) with(res[i, ], cat(sprintf("  %-14s %-22s %7s x %-4s %-10s wk25: %-12s %s\n", status, frame,
                                                             ifelse(is.na(rows), "-", format(rows, big.mark = ",")), ifelse(is.na(cols), "-", cols), seasons, po_wk_2025, detail)))
    bad <- res$frame[res$status == "STALE"]
    if (length(bad)) cat("\n  FIX, when YOU decide to:  ", paste0('frames_restore("', bad, '")', collapse = ";  "), "\n\n", sep = "") else cat("\n")
  }
  invisible(res)
}

frames_restore <- function(nm, keep_old = TRUE, weather = TRUE, envir = .GlobalEnv) {
  stopifnot(is.character(nm), length(nm) == 1)
  f <- .frames_cache_file(nm)
  if (!file.exists(f)) stop("no cache copy: ", f, call. = FALSE)
  new <- readRDS(f)
  why <- .frames_verdict(as.data.frame(dplyr::ungroup(new)), NULL)
  if (length(why)) stop("the CACHE copy of ", nm, " is itself not complete (", paste(why, collapse = "; "), ") -- nothing was changed.", call. = FALSE)
  if (exists(nm, envir = envir, inherits = FALSE) && keep_old) {
    old_nm <- paste0(nm, "_STALE_", format(Sys.time(), "%m%d_%H%M"))
    assign(old_nm, get(nm, envir = envir, inherits = FALSE), envir = envir)
    cat("kept the copy you had as  ", old_nm, "\n", sep = "")
  }
  # the same weather join rec_func_AWS.R:96-99 / rush_func_AWS.R:154-157 do -- done ONCE, only if the columns are not there yet
  if (weather && nm %in% c("receiving_func_base", "rush_stats_final") && !any(FRAMES_WX %in% names(new))) {
    if (exists("qb_stats_df_final", envir = envir, inherits = FALSE) && !length(.frames_verdict(get("qb_stats_df_final", envir = envir), NULL))) {
      q <- get("qb_stats_df_final", envir = envir)
      new <- dplyr::left_join(new, q[, c("qbgrp_ssn", "def_ssn", "week", "season", FRAMES_WX)], by = c("qbgrp_ssn", "def_ssn", "week", "season"))
      cat("weather columns joined from the qb_stats_df_final in your session (it checks GOOD). Do NOT run the join lines in the func file again.\n")
    } else cat("weather NOT joined: qb_stats_df_final in the session is missing or STALE. frames_restore(\"qb_stats_df_final\") first, then call this again.\n")
  }
  assign(nm, new, envir = envir)
  cat(sprintf("RESTORED %s <- %s  (%d x %d, cache saved %s)\n", nm, f, nrow(new), ncol(new), format(file.mtime(f), "%Y-%m-%d %H:%M")))
  invisible(frames_check(nm, envir = envir))
}

safe_load <- function(file, take = NULL, big = FALSE, envir = .GlobalEnv) {
  stopifnot(file.exists(file))
  gb <- file.size(file) / 1024^3
  if (gb > 0.5 && !big) stop(sprintf("%s is %.1f GB on disk (about %.0f GB in memory). R has to read ALL of it even to take one object; the 10 GB load on 09-18 19:55 killed the session. If you mean it: safe_load(file, take = ..., big = TRUE)", basename(file), gb, gb * 9), call. = FALSE)
  side <- new.env()
  on.exit({ rm(list = ls(side, all.names = TRUE), envir = side); invisible(gc()) }, add = TRUE)
  nms <- load(file, envir = side)
  have <- vapply(nms, exists, TRUE, envir = envir, inherits = FALSE)
  bad_take <- setdiff(take, nms); if (length(bad_take)) cat("not in that file: ", paste(bad_take, collapse = ", "), "\n")
  copy <- union(nms[!have], intersect(take, nms))
  # a key frame coming out of a workspace is copied only if it checks GOOD, even when the session has none
  for (k in intersect(copy, FRAMES_KEY)) {
    f <- .frames_cache_file(k); ref <- if (file.exists(f)) as.data.frame(dplyr::ungroup(readRDS(f))) else NULL
    why <- .frames_verdict(get(k, envir = side), ref)
    if (length(why)) { cat(sprintf("REFUSED  %-24s the copy inside this workspace is STALE (%s). Use frames_restore(\"%s\").\n", k, paste(why, collapse = "; "), k)); copy <- setdiff(copy, k) }
  }
  copy <- copy[!vapply(copy, function(n) is.environment(get(n, envir = side)), TRUE)]          # never bring a nested workspace-in-an-environment across
  for (n in copy) assign(n, get(n, envir = side), envir = envir)
  cat(sprintf("\nsafe_load  %s  (saved %s, %d objects)\n  copied %d  (%d were missing from the session, %d you named)\n  left alone %d objects you already have\n",
              basename(file), format(file.mtime(file), "%Y-%m-%d %H:%M"), length(nms), length(copy), sum(!have[copy]), sum(have[copy]), sum(have & !(nms %in% copy))))
  if (any(have & nms %in% copy)) cat("  REPLACED because you named them: ", paste(nms[have & nms %in% copy], collapse = ", "), "\n")
  invisible(frames_check(envir = envir))
}

safe_save_image <- function(dir = FRAMES_REPO, stem = "nfl_the_everything_workspace", force = FALSE, envir = .GlobalEnv) {
  res <- frames_check(envir = envir)
  if (any(res$status == "STALE") && !force) stop("NOT SAVED: a key frame is STALE, and saving would freeze it into the workspace. Restore it first (or force = TRUE).", call. = FALSE)
  nms <- ls(envir)                                                                                # same set save.image() writes (no dot-names)
  envs <- nms[vapply(nms, function(n) is.environment(get(n, envir = envir)), TRUE)]
  if (length(envs)) cat("left out (environments -- a loaded workspace parked in one doubles the file): ", paste(envs, collapse = ", "), "\n")
  f <- file.path(dir, sprintf("%s_%s.RData", stem, format(Sys.time(), "%Y%m%d_%H%M")))
  if (file.exists(f)) stop("already exists: ", f, call. = FALSE)
  save(list = setdiff(nms, envs), file = f, envir = envir)
  cat(sprintf("SAVED %d objects -> %s  (%.2f GB). Older workspaces were not touched.\n", length(nms) - length(envs), f, file.size(f) / 1024^3))
  invisible(f)
}


# ===================== part 2 (overrides) =====================
# frames_guard_ADDENDUM.R -- PROPOSED, Claude (verify agent), UNSIGNED, 2026-09-20. Lives in the scratchpad, not in the repo.
# Source AFTER frames_guard_DRAFT.R. Sourcing defines functions only; nothing runs, nothing is read or written.
#
# 1. .frames_verdict  -- adds a per (season, week) row-count comparison with the cache copy, so a frame with the
#                        same total rows but a hole somewhere (or a week that is thinner than cache) is caught.
# 2. frames_restore   -- weather defaults to FALSE: the cache copies are PRE-join, and rec_func_AWS.R:96-99 /
#                        rush_func_AWS.R:154-157 do the join when those files run. Joining inside the restore AND
#                        running the file = temp.x / temp.y (verified). Ask for it with weather = TRUE.
# 3. registry_keep() / registry_back() -- receiver_registry has never been written to disk by any file; it only
#                        lives in the session, so every restart loses it. registry_keep() MERGES the session copy
#                        with the disk copy (never shrinks the disk copy) and also writes a dated twin.

.frames_verdict_v1 <- .frames_verdict
.frames_verdict <- function(x, ref) {
  why <- .frames_verdict_v1(x, ref)
  if (!is.null(ref) && all(c("season", "week") %in% names(x)) && all(c("season", "week") %in% names(ref))) {
    a <- as.data.frame(table(season = ref$season, week = ref$week), stringsAsFactors = FALSE); a <- a[a$Freq > 0, ]
    b <- as.data.frame(table(season = x$season,   week = x$week),   stringsAsFactors = FALSE)
    m <- merge(a, b, by = c("season", "week"), all.x = TRUE, suffixes = c("_cache", "_here")); m$Freq_here[is.na(m$Freq_here)] <- 0
    thin <- m[m$Freq_here < m$Freq_cache, ]
    if (nrow(thin)) why <- c(why, sprintf("%d season-weeks thinner than cache (first: %s wk %s, %d vs %d rows)", nrow(thin), thin$season[1], thin$week[1], thin$Freq_here[1], thin$Freq_cache[1]))
  }
  why
}

.frames_restore_v1 <- frames_restore
frames_restore <- function(nm, keep_old = TRUE, weather = FALSE, envir = .GlobalEnv) .frames_restore_v1(nm, keep_old = keep_old, weather = weather, envir = envir)

REGISTRY_FILE <- file.path(FRAMES_REPO, "cache", "receiver_registry.rds")

registry_keep <- function(file = REGISTRY_FILE, envir = .GlobalEnv) {
  if (!exists("receiver_registry", envir = envir, inherits = FALSE)) stop("receiver_registry is not in the session -- registry_back() first.", call. = FALSE)
  here <- as.data.frame(get("receiver_registry", envir = envir))
  disk <- if (file.exists(file)) as.data.frame(readRDS(file)) else here[0, ]
  key  <- function(d) paste(d$view_name, format(d$logged_at), d$params, sep = " @@ ")
  extra <- disk[!(key(disk) %in% key(here)), , drop = FALSE]          # rows only the disk copy has are KEPT
  all_rows <- rbind(extra[, names(here), drop = FALSE], here); all_rows <- all_rows[!duplicated(key(all_rows)), , drop = FALSE]
  all_rows <- all_rows[order(all_rows$logged_at), , drop = FALSE]; rownames(all_rows) <- NULL
  out <- tibble::as_tibble(all_rows)
  saveRDS(out, file)
  twin <- sub("[.]rds$", format(Sys.time(), "_%Y%m%d_%H%M.rds"), file); saveRDS(out, twin)
  cat(sprintf("receiver_registry KEPT: %d rows (%d views) -> %s  (+ dated twin %s). %d rows came from disk only, %d from the session.\n",
              nrow(out), length(unique(out$view_name)), file, basename(twin), nrow(extra), nrow(here)))
  invisible(out)
}

registry_back <- function(file = REGISTRY_FILE, envir = .GlobalEnv) {
  if (!file.exists(file)) stop("no registry on disk yet: ", file, call. = FALSE)
  disk <- readRDS(file)
  if (exists("receiver_registry", envir = envir, inherits = FALSE)) {
    here <- get("receiver_registry", envir = envir)
    if (nrow(here) > 0 && !all(here$view_name %in% disk$view_name)) stop("the session receiver_registry has views the disk copy lacks -- run registry_keep() first so nothing is lost.", call. = FALSE)
  }
  assign("receiver_registry", disk, envir = envir)
  cat(sprintf("receiver_registry BACK: %d rows, %d views, logged %s to %s  <- %s\n", nrow(disk), length(unique(disk$view_name)), format(min(disk$logged_at)), format(max(disk$logged_at)), file))
  invisible(disk)
}


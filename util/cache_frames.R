# ============================================================
# CACHE FRAMES -- dated receipts on disk for the frames that cost
# Athena time. Added 2026-09-12 after a session wipe cost a day.
#
#   source("util/cache_frames.R")
#   cache_frames("rushing_qbgrp", "run_defense_qbgrp")   # after a step-0
#   uncache_frames("rushing_qbgrp")                       # fast path next session
#   cache_ls()                                            # what is on disk, how old
#
# LAW: a cache is a RECEIPT WITH A DATE, not a session object. uncache_
#   frames() prints the date and row count of every file it loads, so a
#   stale frame is visible the moment it enters the session. Re-running
#   the step-0 file is always the canon path; this is the shortcut.
#
# Files go to cache/ under getwd() (the repo root). *.rds is gitignored.
# ============================================================

CACHE_DIR <- "cache"

cache_frames <- function(...) {
  nms <- c(...)
  dir.create(CACHE_DIR, showWarnings = FALSE)
  for (nm in nms) {
    if (!exists(nm, envir = .GlobalEnv)) {
      cat("SKIP  ", nm, " -- not in session\n", sep = ""); next
    }
    obj <- get(nm, envir = .GlobalEnv)
    f <- file.path(CACHE_DIR, paste0(nm, ".rds"))
    saveRDS(obj, f)
    dim_txt <- if (is.data.frame(obj)) paste0(nrow(obj), " x ", ncol(obj)) else class(obj)[1]
    cat(sprintf("CACHED %-28s -> %s  (%s, %s)\n", nm, f, dim_txt,
                format(Sys.time(), "%Y-%m-%d %H:%M")))
  }
  invisible(nms)
}

uncache_frames <- function(...) {
  nms <- c(...)
  for (nm in nms) {
    f <- file.path(CACHE_DIR, paste0(nm, ".rds"))
    if (!file.exists(f)) {
      cat("MISSING ", nm, " -- no ", f, "; run its step-0 file\n", sep = ""); next
    }
    obj <- readRDS(f)
    assign(nm, obj, envir = .GlobalEnv)
    age <- round(as.numeric(difftime(Sys.time(), file.mtime(f), units = "days")), 1)
    dim_txt <- if (is.data.frame(obj)) paste0(nrow(obj), " x ", ncol(obj)) else class(obj)[1]
    cat(sprintf("LOADED %-28s <- %s  (%s, saved %s, %s days old)\n", nm, f, dim_txt,
                format(file.mtime(f), "%Y-%m-%d %H:%M"), age))
  }
  invisible(nms)
}

cache_ls <- function() {
  fs <- list.files(CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (!length(fs)) { cat("cache/ is empty\n"); return(invisible(NULL)) }
  out <- data.frame(
    frame    = sub("\\.rds$", "", basename(fs)),
    saved    = format(file.mtime(fs), "%Y-%m-%d %H:%M"),
    days_old = round(as.numeric(difftime(Sys.time(), file.mtime(fs), units = "days")), 1),
    mb       = round(file.size(fs) / 1048576, 1)
  )
  print(out[order(out$frame), ], row.names = FALSE)
  invisible(out)
}

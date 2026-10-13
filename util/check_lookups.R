# check_lookups.R -- Claude, 2026-09-27. New file. Read-only: changes nothing in the session.
#
#   source("C:/Users/vflre/Downloads/nflmodels_UPDATE/util/check_lookups.R")
#
# Tells you, object by object, whether the comparison lookups in THIS session are the APRIL set (S3, 2026-04-01) or the retired
# JANUARY set (archive/2026-09-27_january_lookups_retired/). Downloads a fresh April copy from S3 into a private environment to compare.
# BOTH = the two builds are identical for that object (the S3 less-def files ARE the 2026-01-27 build; April never rebuilt them).

local({
  G <- globalenv(); REPO <- "C:/Users/vflre/Downloads/nflmodels_UPDATE"; bucket <- "nfl-pff-data-lucas"
  lens <- c("blitz", "depth", "less", "pa", "pressure")
  A <- new.env()
  for (k in lens) for (p in c(paste0("qbgrp_def_functions/list_dependencies_", k, ".RData"), paste0("def_functions/list_dependencies_", k, "_def.RData"))) {
    tmp <- tempfile(); aws.s3::save_object(p, bucket = bucket, file = tmp); load(tmp, envir = A); unlink(tmp) }
  jf <- file.path(REPO, "archive/2026-09-27_january_lookups_retired/cache/comparison_lookups_from_workspace_2026-09-19.rds")
  J <- if (file.exists(jf)) readRDS(jf) else NULL
  rows <- list()
  for (k in lens) for (s in c("", "_def")) for (w in 1:2) {
    nm <- if (w == 1) paste0("df_", k, s, "_scaled_z") else paste0("importance_matrix_", k, s)
    apr <- get(paste0("list_dependencies_", k, s), envir = A)[[w]]
    if (!exists(nm, envir = G, inherits = FALSE)) { rows[[nm]] <- c(nm, "MISSING from session"); next }
    cur <- get(nm, envir = G, inherits = FALSE)
    isA <- isTRUE(all.equal(cur, apr, check.attributes = FALSE)); isJ <- !is.null(J) && isTRUE(all.equal(cur, J[[nm]], check.attributes = FALSE))
    rows[[nm]] <- c(nm, if (isA && isJ) "BOTH (identical in the two builds)" else if (isA) "APRIL" else if (isJ) "JANUARY  <-- run util/load_april_lookups.R" else "NEITHER (some other build)")
  }
  out <- do.call(rbind, rows); colnames(out) <- c("object", "build in this session")
  print(noquote(out[, 2, drop = FALSE]), right = FALSE)
  pd <- if (exists("comparison_pressure_def_func", envir = G)) isTRUE(attr(get("comparison_pressure_def_func", envir = G), "april_qb_wrap")) else NA
  cat("\npressure-defense Defense->QB wrap in place:", pd, "\n")
  if (exists("comparison_blitz_def_func", envir = G)) cat("quick eyeball: nrow(comparison_blitz_def_func(\"LA2025\", .965)) =",
    nrow(as.data.frame(get("comparison_blitz_def_func", envir = G)("LA2025", .965))), "  (120 = APRIL, 125 = JANUARY)\n")
})

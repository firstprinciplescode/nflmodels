# load_april_lookups.R -- Claude, 2026-09-27. New file. APRIL ONLY (Andy: "IT'S ALL APRIL").
#
#   source("C:/Users/vflre/Downloads/nflmodels_UPDATE/util/load_april_lookups.R")
#
# 1. Removes every comparison lookup already in the session (whatever build it came from -- the retired January set included).
# 2. Loads the 2026-04-01 set from S3, exactly the way stats_comparison_engine.R does it (qbgrp_def_functions/ + def_functions/,
#    load() on the .rds, list_dependencies_*[[1]] = df_*_scaled_z, [[2]] = importance_matrix_*).
# 3. The April comparison_pressure_def_func names its first output column "Defense"; every engine and def_comparison_AWS.R read $QB,
#    so the April pressure-defense pool would silently collapse. The function is wrapped so that column comes back as "QB".
#    Nothing on S3 is changed. Re-source this after sourcing any *_func_AWS.R (they re-download April without the wrap).

local({
  G <- globalenv(); bucket <- if (exists("STATS_BUCKET", envir = G)) get("STATS_BUCKET", envir = G) else "nfl-pff-data-lucas"
  lens <- c("blitz", "depth", "less", "pa", "pressure")
  objs <- c(paste0("comparison_", lens, "_func"), paste0("comparison_", lens, "_def_func"), paste0("df_", lens, "_scaled_z"), paste0("df_", lens, "_def_scaled_z"),
            paste0("importance_matrix_", lens), paste0("importance_matrix_", lens, "_def"), paste0("list_dependencies_", lens), paste0("list_dependencies_", lens, "_def"))
  had <- objs[vapply(objs, exists, logical(1), envir = G, inherits = FALSE)]
  if (length(had)) rm(list = had, envir = G)
  for (k in lens) for (p in c(paste0("qbgrp_def_functions/comparison_", k, "_func.rds"), paste0("qbgrp_def_functions/list_dependencies_", k, ".RData"),
                             paste0("def_functions/comparison_", k, "_def_func.rds"), paste0("def_functions/list_dependencies_", k, "_def.RData"))) {
    tmp <- tempfile(); aws.s3::save_object(p, bucket = bucket, file = tmp); load(tmp, envir = G); unlink(tmp)
  }
  for (k in lens) {
    assign(paste0("importance_matrix_", k), get(paste0("list_dependencies_", k), envir = G)[[2]], envir = G)
    assign(paste0("df_", k, "_scaled_z"), get(paste0("list_dependencies_", k), envir = G)[[1]], envir = G)
    assign(paste0("importance_matrix_", k, "_def"), get(paste0("list_dependencies_", k, "_def"), envir = G)[[2]], envir = G)
    assign(paste0("df_", k, "_def_scaled_z"), get(paste0("list_dependencies_", k, "_def"), envir = G)[[1]], envir = G)
  }
  pd <- get("comparison_pressure_def_func", envir = G)
  if (!isTRUE(attr(pd, "april_qb_wrap"))) {
    w <- function(...) { x <- .april_pressure_def_raw(...); if ("Defense" %in% names(x)) names(x)[names(x) == "Defense"] <- "QB"; x }
    attr(w, "april_qb_wrap") <- TRUE
    assign(".april_pressure_def_raw", pd, envir = G); assign("comparison_pressure_def_func", w, envir = G)
  }
  cat(sprintf("APRIL lookups loaded (2026-04-01 set from S3): 40 objects%s; comparison_pressure_def_func returns QB\n",
              if (length(had)) sprintf(", %d old ones removed first", length(had)) else ""))
})

# comparison_engine_thresholds.R  (v7)
# PROPOSED. Replaces v6. Self-contained: source in a session that already
# has qb_stats_df_final and the ten comparison_*_func lookups.
# Zero package dependencies -- base R throughout.
#
# ANDY'S SPEC:
#   "70 at weight 1" is the IDEAL POINT -- not a hard rule, and not a
#   throwaway readout either. The real target is DYNAMIC: for any
#   tolerance t there is a target comp-list size T(t):
#       T(1.00) = 70                  (the anchor)
#       slope at 1.00 = -7 per +0.01  (steepest at the anchor)
#       T(1.10) >= 30                 (curve flattens; NOT purely linear)
#   Below 1.00 the mirror holds: +7-ish per -0.01, saturating at the
#   side's universe size (you cannot comp more entities than exist).
#
#   HONEST CEILINGS (v6, per Andy):
#   T_INT is the ASPIRATION, not a universal target. A starved entity
#   (TENTannehill-2019) cannot honestly reach it -- getting there means
#   cranking tolerances to 1.20+, i.e. letting in comps that do not
#   belong. So each QB x DEF cell gets its own target:
#       n_target = the both-bucket n when BOTH entities sit at their own
#                  curve crossings, capped to [T_FLOOR, T_INT].
#   Set HONEST_CAP <- FALSE to revert to one global target for all cells.
#
#   ONE-SIDED N-FIT (v7, per Andy -- "if there are more similar games
#   there will be more games, and that's ok"):
#   Shortfall costs; surplus is FREE. The v6 symmetric |n - n_target|
#   penalized overshoot like shortfall, so the optimizer strangled its
#   dense-neighborhood entities (SEA2024 blitz/less, Prescott less/
#   pressure -- 140-150 comps at 1.00) tighter than their own curve just
#   to shave games back down to the target. That was the honest-ceiling
#   principle applied to starved cells but violated for rich ones. Now
#   every entity floats to its curve crossing and we take the extra
#   games. ONE_SIDED <- FALSE restores the symmetric version.
#   WARNING: with ONE_SIDED the curve term is the ONLY thing holding
#   list sizes -- LAM must be > 0 (enforced below).
#
#   THE OPTIMIZATION, per lens: choose (SEA2025 tol, SEA2024 tol, one
#   tol per QB) minimizing
#       sum over 6 both-bucket cells  max(0, n_target(cell) - n)  (n fit)
#     + LAM * sum over 5 entities     |size - T(tol)|             (curve fit)
#   subject to the non-negotiable floor: every both-bucket n >= T_FLOOR.
#
# METHOD (exact, exhaustive): per lens, sweep every entity over the grid
# once, precompute every both-bucket count as a matrix, then scan the
# (def1 x def2) plane. Each QB's best tol is independent given the def
# pair, so the scan finds the exact global optimum on the grid.
#
# READOUT per lens (all labels read from the knobs, never hardcoded):
#   - per entity: size at tol 1.00 vs the anchor (calibration gap),
#     and the curve crossing (where this entity naturally sits)
#   - per cell: its n_target ('*' = honest ceiling, capped below T_INT)
#   - chosen tol/size vs target, and n vs target
#
# KNOWN GEOGRAPHY (banked, v6 readout): Tannehill-2019's isolation is on
# blitz/depth/pressure (14/11/20 comps at 1.00); PA is his richest
# neighborhood (~102). SEA2024 is the mirror: dense on blitz/less
# (~145/150), starved on depth (~15). Depth is starved for every entity
# in the study -- expect its cells to stay honest-capped.

# ---------------------------------------------------------------------------
# 0. KNOBS
# ---------------------------------------------------------------------------
T_INT   <- 400     # both-bucket games aspiration (per QB x DEF cell)
T_FLOOR <- 85      # both-bucket games floor, non-negotiable
LAM     <- 1       # weight on curve fit; 0 = counts are free
HONEST_CAP <- TRUE # per-cell honest ceilings; FALSE = one global T_INT target
ONE_SIDED  <- TRUE # TRUE: surplus games are free, only shortfall costs.
# FALSE: symmetric |n - n_target| (v6 behavior)

ANCHOR_T <- 1.00   # where the anchor sits
T_ANCHOR <- 70     # ideal list size at the anchor
SLOPE_PP <- 6      # teams gained/lost per 0.01 of tolerance at the anchor
T_ASYMP  <- 30     # decay floor as tolerance rises -> T(1.10) ~ 35, never < 30
SIZE_FLOOR <- 0    # 0 = off. Set to 30 to make "at least 30 teams" a HARD
# constraint. Off by default: starved entities (see
# TENTannehill-2019 depth) cannot always honor it, and
# the curve already pulls sizes toward it softly.

tol_grid <- seq(0.80, 1.35, by = 0.005)

if (ONE_SIDED && LAM <= 0)
  stop("ONE_SIDED with LAM = 0 is degenerate: surplus is free, so nothing ",
       "holds list sizes. Set LAM > 0 (curve pins the sizes) or ONE_SIDED ",
       "<- FALSE (symmetric n-fit holds them instead).")

# ---------------------------------------------------------------------------
# 1. THE DYNAMIC TARGET CURVE
#    t >= 1: exponential decay to T_ASYMP, slope exactly -SLOPE_PP*100 at 1
#    t <= 1: linear at the anchor slope, capped at the side's universe size
#    C1-smooth at the anchor by construction.
# ---------------------------------------------------------------------------
K_DECAY <- (SLOPE_PP * 100) / (T_ANCHOR - T_ASYMP)

target_size <- function(t, n_univ) {
  raw <- ifelse(t >= ANCHOR_T,
                T_ASYMP + (T_ANCHOR - T_ASYMP) * exp(-K_DECAY * (t - ANCHOR_T)),
                T_ANCHOR + SLOPE_PP * 100 * (ANCHOR_T - t))
  pmin(raw, n_univ)
}

# one-sided or symmetric n deviation, read from the knob
n_dev <- function(n, tgt) {
  if (ONE_SIDED) pmax(0, tgt - n) else abs(n - tgt)
}

cat("# target curve T(t), uncapped -- the shape every entity is scored against\n")
show_t <- c(0.90, 0.95, 0.98, 1.00, 1.01, 1.02, 1.05, 1.10, 1.15, 1.20)
cat(sprintf("#   tol  %s\n", paste(sprintf("%5.2f", show_t), collapse = " ")))
cat(sprintf("#   T(t) %s\n",
            paste(sprintf("%5.0f", target_size(show_t, Inf)), collapse = " ")))

# ---------------------------------------------------------------------------
# 2. ENTITIES, LENSES, LOOKUPS
# ---------------------------------------------------------------------------
qbs    <- c("SEADarnold-2025")
defs   <- c("NE2025", "NE2025")
lenses <- names(stats_categories)
lenses <- c("blitz", "depth", "less", "pa", "pressure")
qb_funcs  <- list(blitz = comparison_blitz_func,  depth = comparison_depth_func,
                  less = comparison_less_func,    pa = comparison_pa_func,
                  pressure = comparison_pressure_func)
def_funcs <- list(blitz = comparison_blitz_def_func, depth = comparison_depth_def_func,
                  less = comparison_less_def_func,   pa = comparison_pa_def_func,
                  pressure = comparison_pressure_def_func)

DFQ <- as.data.frame(table(qb_stats_df_final$qbgrp_ssn,
                           qb_stats_df_final$def_ssn),
                     stringsAsFactors = FALSE)
names(DFQ) <- c("qbgrp_ssn", "def_ssn", "g")
DFQ <- DFQ[DFQ$g > 0, ]
G <- as.numeric(DFQ$g)

n_qb_univ  <- length(unique(DFQ$qbgrp_ssn))
n_def_univ <- length(unique(DFQ$def_ssn))
tgt_qb  <- target_size(tol_grid, n_qb_univ)    # target per grid point, QB side
tgt_def <- target_size(tol_grid, n_def_univ)   # target per grid point, DEF side

# ---------------------------------------------------------------------------
# 3. SWEEP HELPERS
# ---------------------------------------------------------------------------
sweep_entity <- function(f, entity) {
  lapply(tol_grid, function(t) {
    comps <- f(entity, t)$QB
    list(tol = t, size = length(comps), lst = unique(c(comps, entity)))
  })
}
ind_mat <- function(sw, key) {
  vapply(sw, function(x) as.numeric(DFQ[[key]] %in% x$lst),
         numeric(nrow(DFQ)))
}
crossing <- function(sz, tgt) which.min(abs(sz - tgt))   # index of curve crossing
cal_line <- function(sw, label, tgt) {
  sz <- vapply(sw, `[[`, numeric(1), "size")
  i1 <- which.min(abs(tol_grid - ANCHOR_T))
  ix <- crossing(sz, tgt)
  cat(sprintf(paste0("   %-26s at 1.00: size %3d (anchor %d) | curve crossing ",
                     "tol %.3f (size %d, target %.0f)\n"),
              label, sz[i1], T_ANCHOR, tol_grid[ix], sz[ix], tgt[ix]))
}
size_mask <- function(sz) {
  if (SIZE_FLOOR > 0) sz >= SIZE_FLOOR else rep(TRUE, length(sz))
}

final_qb <- list(); final_def <- list(); final_n <- list()

# ---------------------------------------------------------------------------
# 4. THE OPTIMIZER
# ---------------------------------------------------------------------------
for (L in lenses) {
  sw_def <- lapply(defs, function(d) sweep_entity(def_funcs[[L]], d)); names(sw_def) <- defs
  sw_qb  <- lapply(qbs,  function(q) sweep_entity(qb_funcs[[L]],  q)); names(sw_qb)  <- qbs
  sz_def <- lapply(sw_def, function(s) vapply(s, `[[`, numeric(1), "size"))
  sz_qb  <- lapply(sw_qb,  function(s) vapply(s, `[[`, numeric(1), "size"))
  
  Dm <- lapply(sw_def, ind_mat, key = "def_ssn")
  Qm <- lapply(sw_qb,  ind_mat, key = "qbgrp_ssn")
  # N[[q]][[d]][k, i] = both-bucket games for QB tol k vs DEF tol i
  N <- lapply(qbs, function(q) lapply(defs, function(d)
    crossprod(Qm[[q]], Dm[[d]] * G)))
  names(N) <- qbs; for (q in qbs) names(N[[q]]) <- defs
  
  # honest per-cell n targets: both-bucket n at the curve crossings,
  # capped to [T_FLOOR, T_INT]
  k0_q <- vapply(qbs,  function(q) crossing(sz_qb[[q]],  tgt_qb),  numeric(1))
  k0_d <- vapply(defs, function(d) crossing(sz_def[[d]], tgt_def), numeric(1))
  n_tgt <- matrix(T_INT, nrow = length(qbs), ncol = length(defs),
                  dimnames = list(qbs, defs))
  if (HONEST_CAP) {
    for (q in qbs) for (d in defs) {
      n_nat <- N[[q]][[d]][k0_q[q], k0_d[d]]
      n_tgt[q, d] <- min(T_INT, max(n_nat, T_FLOOR))
    }
  }
  
  nT <- length(tol_grid)
  best <- list(score = Inf, curve = Inf)
  fallback <- list(minn = -1)
  A_of <- vector("list", length(qbs)); names(A_of) <- qbs
  
  for (i in seq_len(nT)) {
    if (SIZE_FLOOR > 0 && sz_def[[defs[1]]][i] < SIZE_FLOOR) next
    for (q in qbs) {
      A <- n_dev(N[[q]][[defs[1]]][, i], n_tgt[q, defs[1]])
      A[N[[q]][[defs[1]]][, i] < T_FLOOR] <- Inf
      A_of[[q]] <- A
    }
    def_curve_i <- LAM * abs(sz_def[[defs[1]]][i] - tgt_def[i])
    for (j in seq_len(nT)) {
      if (SIZE_FLOOR > 0 && sz_def[[defs[2]]][j] < SIZE_FLOOR) next
      tot <- def_curve_i + LAM * abs(sz_def[[defs[2]]][j] - tgt_def[j])
      cur <- abs(sz_def[[defs[1]]][i] - tgt_def[i]) +
        abs(sz_def[[defs[2]]][j] - tgt_def[j])
      ks <- integer(0); ok <- TRUE
      for (q in qbs) {
        B <- n_dev(N[[q]][[defs[2]]][, j], n_tgt[q, defs[2]])
        B[N[[q]][[defs[2]]][, j] < T_FLOOR] <- Inf
        gap_q <- abs(sz_qb[[q]] - tgt_qb)
        s <- A_of[[q]] + B + LAM * gap_q
        s[!size_mask(sz_qb[[q]])] <- Inf
        k <- which.min(s)
        if (!is.finite(s[k])) {
          ok <- FALSE
          m <- pmin(N[[q]][[defs[1]]][, i], N[[q]][[defs[2]]][, j])
          km <- which.max(m)
          if (m[km] > fallback$minn)
            fallback <- list(minn = m[km], i = i, j = j, q = q, k = km)
          break
        }
        tot <- tot + s[k]; cur <- cur + gap_q[k]; ks <- c(ks, k)
      }
      if (!ok) next
      if (tot < best$score || (tot == best$score && cur < best$curve))
        best <- list(score = tot, curve = cur, i = i, j = j, ks = ks)
    }
  }
  
  cat("\n== ", L, " ==\n", sep = "")
  for (d in defs) cal_line(sw_def[[d]], paste("DEF", d), tgt_def)
  for (q in qbs)  cal_line(sw_qb[[q]],  paste("QB ", q), tgt_qb)
  cat(sprintf("   cell targets (* = honest ceiling, capped below %g):\n", T_INT))
  for (q in qbs)
    cat(sprintf("     %-18s vs %s %3.0f%-2s vs %s %3.0f%-2s\n", q,
                defs[1], n_tgt[q, defs[1]], if (n_tgt[q, defs[1]] < T_INT) "*" else "",
                defs[2], n_tgt[q, defs[2]], if (n_tgt[q, defs[2]] < T_INT) "*" else ""))
  
  if (!is.finite(best$score)) {
    cat(sprintf(paste0("   FLOOR UNREACHABLE -- best achievable min-n %d at ",
                       "%s tol %.3f x %s tol %.3f x %s tol %.3f; ",
                       "lens needs a ruling\n"),
                fallback$minn, defs[1], tol_grid[fallback$i],
                defs[2], tol_grid[fallback$j], fallback$q, tol_grid[fallback$k]))
    final_def[[L]] <- c(NA_real_, NA_real_)           # failed lenses write
    final_qb[[L]]  <- rep(NA_real_, length(qbs))      # NA rows instead of
    final_n[[L]]   <- matrix(NA_real_, nrow = length(defs), ncol = length(qbs))
    next                                            # crashing the paste block
  }
  
  i <- best$i; j <- best$j
  cat(sprintf("   DEF %-8s tol %.3f  size %3d (target %2.0f)  |  DEF %-8s tol %.3f  size %3d (target %2.0f)\n",
              defs[1], tol_grid[i], sz_def[[defs[1]]][i], tgt_def[i],
              defs[2], tol_grid[j], sz_def[[defs[2]]][j], tgt_def[j]))
  for (a in seq_along(qbs)) {
    q <- qbs[a]; k <- best$ks[a]
    cat(sprintf("   QB  %-18s tol %.3f  size %3d (target %2.0f)  n %3d / %3d (tgt %3.0f / %3.0f)\n",
                q, tol_grid[k], sz_qb[[q]][k], tgt_qb[k],
                N[[q]][[defs[1]]][k, i], N[[q]][[defs[2]]][k, j],
                n_tgt[q, defs[1]], n_tgt[q, defs[2]]))
  }
  cat(sprintf("   score: %s %.0f  +  curve-fit %.0f (unweighted) x LAM %g\n",
              if (ONE_SIDED) "n-shortfall" else "n-fit",
              best$score - LAM * best$curve, best$curve, LAM))
  
  final_def[[L]] <- c(tol_grid[i], tol_grid[j])
  final_qb[[L]]  <- tol_grid[best$ks]
  final_n[[L]]   <- vapply(seq_along(qbs), function(a)
    c(N[[qbs[a]]][[defs[1]]][best$ks[a], i],
      N[[qbs[a]]][[defs[2]]][best$ks[a], j]), numeric(2))
}

# ---------------------------------------------------------------------------
# 5. PASTE-OVER TABLES FOR THE ENGINE (same format as v4-v6; NA = failed lens)
#    Nothing crosses over automatically: either run the two printed
#    assignments in-session (xtds_run reads the globals at call time),
#    or paste them over the table block in xtds_comparison_engine.R.
#    The "projected" matrix is EXACT for the both-bucket row -- same comp
#    lists, same game counts the workbooks use.
# ---------------------------------------------------------------------------
cat("\n# ---- paste over the engine tables ----\n")
cat("xtds_tol_qb <- list(\n")
for (a in seq_along(qbs)) cat(sprintf('  "%s" = c(%s)%s\n', qbs[a],
                                      paste(sprintf("%s = %.3f", lenses,
                                                    vapply(lenses, function(L) final_qb[[L]][a], numeric(1))),
                                            collapse = ", "), if (a < length(qbs)) "," else ""))
cat(")\nxtds_tol_def <- list(\n")
for (b in seq_along(defs)) cat(sprintf('  "%s" = c(%s)%s\n', defs[b],
                                       paste(sprintf("%s = %.3f", lenses,
                                                     vapply(lenses, function(L) final_def[[L]][b], numeric(1))),
                                             collapse = ", "), if (b < length(defs)) "," else ""))
cat(")\n\n# projected both-bucket matrix (exact; per-cell targets printed per lens above):\n")
for (a in seq_along(qbs)) for (b in seq_along(defs))
  cat(sprintf("# %-20s vs %-8s  %s\n", qbs[a], defs[b],
              paste(sprintf("%s %3.0f", lenses,
                            vapply(lenses, function(L) final_n[[L]][b, a], numeric(1))),
                    collapse = "  ")))

# ---------------------------------------------------------------------------
# usage: source this file in the AWS session after tds_func_AWS.R has loaded
# qb_stats_df_final and the ten lookups. Retune in one place:
#   - the curve shape:  T_ANCHOR / SLOPE_PP / T_ASYMP in section 0
#   - the trade-off:    LAM (0 = counts free, large = curve is king;
#                       must be > 0 while ONE_SIDED is TRUE)
#   - surplus policy:   ONE_SIDED (TRUE = surplus free, FALSE = symmetric)
#   - honest ceilings:  HONEST_CAP <- FALSE reverts to one global T_INT
#   - hard 30-team min: SIZE_FLOOR <- 30 (off by default, see comment)
# ---------------------------------------------------------------------------


xtds_tol_qb <- list(
  "NEMaye-2025" = c(blitz = 0.995, depth = 0.960, less = 0.990, pa = 1.020, pressure = 1.035),
  "TENTannehill-2019" = c(blitz = 1.090, depth = 1.100, less = 0.995, pa = 0.980, pressure = 1.115),
  "DALPrescott-2025" = c(blitz = 0.970, depth = 0.965, less = 0.940, pa = 0.940, pressure = 0.940)
)

xtds_tol_def <- list(
  "SEA2025" = c(blitz = 0.995, depth = 1.110, less = 1.020, pa = 1.035, pressure = 1.005),
  "SEA2024" = c(blitz = 0.965, depth = 1.085, less = 0.955, pa = 1.005, pressure = 1.010)
)
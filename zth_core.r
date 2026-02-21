# ============================================================
# ZTH Core — Structural Engine
# Version: 1.0.0
# Author: Maxime Tarzaali
#
# Scope (minimal):
# - Transformations F+ / F-
# - Structural value V (V_struct)
# - Human-scale mapping V_human in [0,2] with Vi -> 1
#
# License: MIT (see LICENSE file at repo root)
# ============================================================

ZTH_TENSION_NAMES <- c("R", "Q", "I", "C", "D", "Ri")

zth_assert_named_tensions <- function(x) {
  if (is.null(names(x))) {
    stop("tensions must be a named vector with names: R, Q, I, C, D, Ri", call. = FALSE)
  }
  missing <- setdiff(ZTH_TENSION_NAMES, names(x))
  if (length(missing) > 0) {
    stop(
      paste0("tensions is missing name(s): ", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ----------------------------
# Transformations (monotone, positive)
# ----------------------------

Fplus <- function(s, alpha = 0.25, floor = 0.25) {
  pmax(floor, 1 + alpha * s)
}

Fminus <- function(s, alpha = 0.25, floor = 0.25) {
  pmax(floor, 1 + alpha * s)
}

# ----------------------------
# Structural value V (V_struct)
# ----------------------------

compute_V <- function(R, Q, I, C, D, Ri, alpha = 0.25, floor = 0.25) {
  num <- Fplus(R, alpha, floor) * Fplus(Q, alpha, floor) * Fplus(I, alpha, floor)
  den <- Fminus(C, alpha, floor) * Fminus(D, alpha, floor) * Fminus(Ri, alpha, floor)
  num / den
}

compute_V_from_vector <- function(tensions, alpha = 0.25, floor = 0.25) {
  zth_assert_named_tensions(tensions)
  tau <- tensions[ZTH_TENSION_NAMES]
  compute_V(
    R  = tau[["R"]],
    Q  = tau[["Q"]],
    I  = tau[["I"]],
    C  = tau[["C"]],
    D  = tau[["D"]],
    Ri = tau[["Ri"]],
    alpha = alpha,
    floor = floor
  )
}

# ----------------------------
# Human-scale mapping V_human in [0,2]
# ----------------------------

compute_V_human <- function(V_struct, V_min = 1/216, V_max = 216) {
  if (!is.numeric(V_struct)) stop("V_struct must be numeric.", call. = FALSE)
  if (V_min <= 0 || V_max <= 0) stop("V_min and V_max must be > 0.", call. = FALSE)
  if (V_min >= 1 || V_max <= 1) stop("Require V_min < 1 < V_max.", call. = FALSE)
  
  V_clamped <- pmax(V_min, pmin(V_max, V_struct))
  L <- log(V_clamped) / log(V_max)  # V_min -> -1, 1 -> 0, V_max -> +1
  V_h <- 1 + L                      # [-1,1] -> [0,2]
  pmax(0, pmin(2, V_h))
}
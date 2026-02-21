# ============================================================
# ZTH Core — Visualisation Module
# Version: 1.0.0
# Author: Maxime Tarzaali
#
# Scope:
# - Radar plot for structural tensions
# - Vertical human-scale value plot (V_human in [0,2])
# - Optional time-series plot for V(t)
#
# Requires:
# - zth_core.r (zth_assert_named_tensions)
# - fmsb (for radar chart)
#
# License: MIT (see LICENSE file at repo root)
# ============================================================


# ------------------------------------------------------------
# V(t) time-series plot
# ------------------------------------------------------------

plot_V_series <- function(df, main = "Structural value V(t)") {
  
  if (!all(c("t", "V") %in% names(df))) {
    stop("df must contain columns 't' and 'V'.", call. = FALSE)
  }
  
  plot(
    df$t, df$V,
    type = "l",
    lwd  = 2,
    xlab = "t",
    ylab = "V(t)",
    main = main
  )
  
  abline(h = 1, lty = 2, col = "grey")
}



# ------------------------------------------------------------
# Radar plot — structural tensions
# ------------------------------------------------------------

plot_tension_radar <- function(
    tensions,
    main = "Tension profile (R, Q, I, C, D, Ri)"
) {
  
  if (!requireNamespace("fmsb", quietly = TRUE)) {
    stop("Package 'fmsb' is required for radar plots.", call. = FALSE)
  }
  
  zth_assert_named_tensions(tensions)
  
  vals <- as.numeric(tensions[c("R", "Q", "I", "C", "D", "Ri")])
  names(vals) <- c("R", "Q", "I", "C", "D", "Ri")
  
  dat <- rbind(
    max     = rep( 2, 6),
    min     = rep(-3, 6),
    current = vals
  )
  
  dat <- as.data.frame(dat)
  
  fmsb::radarchart(
    dat,
    axistype = 1,
    seg      = 5,
    pcol     = "blue",
    pfcol    = rgb(0, 0, 1, 0.3),
    plwd     = 2,
    cglcol   = "grey",
    cglty    = 1,
    title    = main
  )
}



# ------------------------------------------------------------
# Vertical value scale — V_human in [0,2]
# ------------------------------------------------------------

plot_V_scale_vertical <- function(
    V_h_current,
    V_h_snapshot = NULL,
    V_min = 0,
    V_max = 2,
    main = "Value scale (human range)"
) {
  
  Vc <- max(V_min, min(V_max, V_h_current))
  
  Vs <- if (!is.null(V_h_snapshot)) {
    max(V_min, min(V_max, V_h_snapshot))
  } else {
    NA_real_
  }
  
  breaks <- c(0, 0.3, 0.7, 1.3, 1.7, 2)
  cols   <- c("darkred", "red", "orange", "chartreuse3", "darkgreen")
  
  plot(
    c(0, 1), c(V_min, V_max),
    type = "n",
    xlab = "",
    ylab = "V_human (0–2)",
    xaxt = "n",
    main = main
  )
  
  # Background bands
  for (i in seq_along(cols)) {
    rect(
      0, breaks[i], 1, breaks[i + 1],
      col    = adjustcolor(cols[i], alpha.f = 0.3),
      border = NA
    )
  }
  
  # Central reference line
  abline(v = 0.5, col = "grey80")
  
  # Current value
  points(0.5, Vc, pch = 19, col = "black", cex = 1.3)
  
  # Optional snapshot
  if (!is.null(V_h_snapshot)) {
    points(0.5, Vs, pch = 17, col = "darkgrey", cex = 1.2)
    legend(
      "topleft",
      legend = c("Current", "Snapshot"),
      pch    = c(19, 17),
      col    = c("black", "darkgrey"),
      bty    = "n"
    )
  }
}

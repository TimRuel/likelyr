# =====================================================================
# plot-palette.R
# Color Palettes for Log-Likelihood Visualization
# =====================================================================

#' Colors for Likelihood-Ratio Confidence Levels
#'
#' @param conf_ints Data frame of CI values.
#' @return Named vector of colors for labelling CI endpoints on likelihood plot.
#' @keywords internal
get_ci_palette <- function(interval_estimates_df) {
  levels <- interval_estimates_df$Level
  n <- length(levels)

  # Twilight Princess: cool teal-green, but lighter at the dark end
  ci_colors <- colorspace::sequential_hcl(
    n,
    h = 150,           # bluish-green (teal forest vibe)
    c = c(35, 65),     # moderate chroma → avoids neon & mud
    l = c(92, 40),     # lighter dark end so plots stay readable
    power = 1.1        # subtle easing for smooth transitions
  )

  names(ci_colors) <- levels
  ci_colors
}

# Default colors for pseudolikelihood curve, MLE marker, and truth marker
CURVE_COLOR <- "#052880"
MLE_COLOR   <- "#941703"
TRUTH_COLOR <- "#C4A806"









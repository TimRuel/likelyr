# =====================================================================
# plot-palette.R
# Color Palettes for Likelihood Visualization
# =====================================================================

#' Colors for Likelihood-Ratio Confidence Levels
#'
#' @param conf_ints Data frame of CI values.
#' @return Named vector of colors for labelling CI endpoints on likelihood plot.
#' @keywords internal
get_ci_palette <- function(conf_ints) {

  confidence <- conf_ints$confidence
  n <- length(confidence)

  ci_palette <- viridisLite::viridis(n, option = "D")
  names(ci_palette) <- confidence

  ci_palette
}

# Default colors for MLE and truth markers
MLE_COLOR   <- "#D55E00"  # orange
TRUTH_COLOR <- "#009E73"  # teal

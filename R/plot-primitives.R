# =====================================================================
# plot-primitives.R
# Low-level Plot Building Blocks
# =====================================================================

#' Build ggplot stat_function layer for pseudolikelihood curves
#'
#' @param psi_endpoints Numeric range.
#' @param relative_loglik_fn Function of psi.
#'
#' @return A ggplot stat_function layer.
#' @keywords internal
make_stat_fn <- function(psi_endpoints, relative_loglik_fn) {

  ggplot2::stat_function(
    fun = relative_loglik_fn,
    geom = "line",
    color = "#00A2FF",
    linewidth = 1.5,
    xlim = psi_endpoints
  )
}

# =====================================================================
# plot-primitives.R
# Low-level Plot Building Blocks
# =====================================================================

#' Build ggplot stat_function layer for pseudo-log-likelihood curves
#'
#' @param psi_endpoints Numeric range.
#' @param zero_max_psi_ll_fn Function of psi.
#'
#' @return A ggplot stat_function layer.
#' @keywords internal
make_stat_fn <- function(psi_endpoints, zero_max_psi_ll_fn) {

  ggplot2::stat_function(
    fun = zero_max_psi_ll_fn,
    geom = "line",
    color = CURVE_COLOR,
    linewidth = 1.5,
    xlim = psi_endpoints
  )
}

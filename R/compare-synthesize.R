# ======================================================================
# compare-synthesize.R — Synthesize pseudolikelihood comparison results
# ======================================================================

#' Synthesize Pseudolikelihood Comparison Results
#'
#' @description
#' High-level orchestration helper that synthesizes point estimates and
#' interval estimates from a fitted likelihood result object.
#'
#' This function performs **no rendering or plotting**. It computes and
#' returns only the data frames required to later construct tables and
#' plots via `view()` and `plot()` methods (local-only).
#'
#' @param res_list A fitted likelihood result object. Must be compatible with
#'   \code{get_point_estimates_df()} and
#'   \code{get_interval_estimates_df()}.
#'
#' @return A named list with the following elements:
#'
#' \describe{
#'   \item{\code{point_estimates_df}}{
#'     Data frame of point estimates and uncertainty measures.
#'   }
#'   \item{\code{interval_estimates_df}}{
#'     Data frame of interval estimates and diagnostics.
#'   }
#' }
#'
#' @keywords internal
synthesize_comparison <- function(res_list) {
  point_estimates_df <- get_point_estimates_df(res_list)

  interval_estimates_df <- get_interval_estimates_df(res_list)

  list(
    point_estimates_df = point_estimates_df,
    interval_estimates_df = interval_estimates_df
  )
}

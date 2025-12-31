# =====================================================================
# compare-synthesize.R — Synthesize pseudolikelihood comparison results
# =====================================================================

#' Synthesize pseudolikelihood comparison results
#'
#' @description
#' High-level orchestration helper that synthesizes point estimates and
#' interval estimates from a fitted likelihood result object, and renders
#' a complete set of comparison tables.
#'
#' @details
#' This function coordinates the following steps:
#'
#' \enumerate{
#'   \item Extract point estimates via \code{get_point_estimates_df()}
#'   \item Render the point estimate comparison table
#'   \item Extract interval estimates via \code{get_interval_estimates_df()}
#'   \item Render the interval estimate comparison table
#'   \item Combine point and interval estimates into a unified data frame
#'   \item Render the combined comparison table
#' }
#'
#' The combined \code{estimates_df} is constructed by repeating point
#' estimates across confidence levels and column-binding the interval
#' diagnostics, ensuring alignment required by
#' \code{render_estimates_comparison_table()}.
#'
#' This function performs **no estimation** itself; it is purely a
#' synthesis and rendering layer.
#'
#' @param res A fitted likelihood result object. Must be compatible with
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
#'     Data frame of interval estimates and diagnostics, including attached
#'     attributes used for rendering.
#'   }
#'   \item{\code{estimates_df}}{
#'     Unified data frame combining point and interval information, suitable
#'     for combined rendering.
#'   }
#'   \item{\code{point_estimates_comparison_table}}{
#'     `kableExtra` HTML table comparing point estimates.
#'   }
#'   \item{\code{interval_estimates_comparison_table}}{
#'     `kableExtra` HTML table comparing interval estimates.
#'   }
#'   \item{\code{estimates_comparison_table}}{
#'     `kableExtra` HTML table combining point and interval estimates.
#'   }
#' }
#'
#' @seealso
#' \code{\link{render_point_estimates_comparison_table}},
#' \code{\link{render_interval_estimates_comparison_table}},
#' \code{\link{render_estimates_comparison_table}}
#'
#' @family inference-renderers
#' @export
synthesize_comparison <- function(res) {

  point_estimates_df <- get_point_estimates_df(res)

  point_estimates_comparison_table <-
    render_point_estimates_comparison_table(point_estimates_df)

  interval_estimates_df <- get_interval_estimates_df(res)

  interval_estimates_comparison_table <-
    render_interval_estimates_comparison_table(interval_estimates_df)

  n_levels <- interval_estimates_df |>
    dplyr::select(Level) |>
    dplyr::n_distinct()

  estimates_df <- point_estimates_df |>
    tidyr::uncount(n_levels) |>
    dplyr::bind_cols(interval_estimates_df) |>
    dplyr::select(
      pseudolikelihood,
      se_psi_hat,
      error,
      psi_hat,
      psi_0,
      Interval,
      Pseudolikelihood,
      Length,
      `Lower Deviation`,
      `Upper Deviation`,
      Status,
      Level
    )

  estimates_comparison_table <-
    render_estimates_comparison_table(estimates_df)

  list(
    point_estimates_df                  = point_estimates_df,
    interval_estimates_df               = interval_estimates_df,
    estimates_df                        = estimates_df,
    point_estimates_comparison_table    = point_estimates_comparison_table,
    interval_estimates_comparison_table = interval_estimates_comparison_table,
    estimates_comparison_table          = estimates_comparison_table
  )
}

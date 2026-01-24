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
#'   \item{\code{estimates_df}}{
#'     Unified data frame combining point and interval information,
#'     suitable for downstream table/plot materialization.
#'   }
#' }
#'
#' @keywords internal
synthesize_comparison <- function(res_list) {
  # ------------------------------------------------------------------
  # Compute-only layer (HPC-safe)
  # ------------------------------------------------------------------
  point_estimates_df <- get_point_estimates_df(res_list)

  interval_estimates_df <- get_interval_estimates_df(res_list)

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

  # ------------------------------------------------------------------
  # Return data only (no tables, no plots)
  # ------------------------------------------------------------------
  list(
    point_estimates_df = point_estimates_df,
    interval_estimates_df = interval_estimates_df,
    estimates_df = estimates_df
  )
}

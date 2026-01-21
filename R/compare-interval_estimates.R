#' Combine interval estimates across pseudolikelihood results
#'
#' @description
#' Extracts interval estimate data frames from a list of likelihood
#' results (e.g., profile and integrated), binds them into a single
#' tidy table, and attaches useful metadata as attributes for
#' downstream plotting and reporting.
#'
#' Specifically, this function:
#' \itemize{
#'   \item Collects \code{interval_estimate_df} from each result
#'   \item Adds a human-readable \code{Pseudolikelihood} label
#'   \item Orders rows by confidence level
#'   \item Attaches point estimates as an attribute
#'   \item Attaches raw interval endpoints (with alpha levels)
#' }
#'
#' @param res Named list of likelihood result objects. Each element
#'   must contain an \code{inference} component with an
#'   \code{interval_estimate_df}.
#'
#' @return A formatted data frame of interval estimates with attributes:
#' \itemize{
#'   \item \code{"point_estimates"} – numeric vector of point estimates
#'   \item \code{"interval_estimates_raw"} – raw interval endpoint data
#' }
#'
#' @keywords internal
get_interval_estimates_df <- function(res) {
  interval_estimates_df <- res |>
    purrr::map(\(x) x$inference$interval_estimate_df) |>
    dplyr::bind_rows(.id = "Pseudolikelihood") |>
    dplyr::mutate(
      Pseudolikelihood = dplyr::recode(
        Pseudolikelihood,
        profile = "Profile",
        integrate = "Integrated"
      )
    ) |>
    dplyr::arrange(Level) |>
    dplyr::select(
      Interval,
      Pseudolikelihood,
      Length,
      `Lower Deviation`,
      `Upper Deviation`,
      Status,
      Level
    )

  attr(interval_estimates_df, "point_estimates") <- res |>
    purrr::map_dbl(
      \(x) attr(x$inference$interval_estimate_df, "point_estimate")
    )

  attr(interval_estimates_df, "interval_estimates_raw") <- res |>
    purrr::map(
      \(x) attr(x$inference$interval_estimate_df, "interval_estimate_raw")
    ) |>
    dplyr::bind_rows(.id = "pseudolikelihood") |>
    dplyr::arrange(-alpha)

  attr(interval_estimates_df, "point_estimate") <- NULL
  attr(interval_estimates_df, "interval_estimate_raw") <- NULL
  attr(interval_estimates_df, "type") <- NULL

  interval_estimates_df
}

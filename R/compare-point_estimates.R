#' Combine point estimates across pseudolikelihood results
#'
#' @description
#' Extracts point estimate data frames from a list of likelihood
#' results (e.g., profile and integrated), binds them into a single
#' tidy table, and adds a human-readable pseudolikelihood label.
#'
#' This is primarily used for constructing comparison tables and
#' downstream reporting.
#'
#' @param res Named list of likelihood result objects. Each element
#'   must contain an \code{inference} component with a
#'   \code{point_estimate_df}.
#'
#' @return A data frame of point estimates with an added
#'   \code{pseudolikelihood} column indicating the pseudolikelihood type.
#'
#' @keywords internal
get_point_estimates_df <- function(res) {
  res |>
    purrr::map(\(x) x$inference$point_estimate_df) |>
    dplyr::bind_rows(.id = "pseudolikelihood") |>
    dplyr::mutate(
      pseudolikelihood = dplyr::recode(
        pseudolikelihood,
        profile = "Profile",
        integrate = "Integrated"
      )
    ) |>
    dplyr::relocate(pseudolikelihood, .after = se_psi_hat)
}

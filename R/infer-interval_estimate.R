# ======================================================================
# infer-interval_estimate.R — Likelihood-based confidence intervals for ψ
# ======================================================================

# ----------------------------------------------------------------------
# Internal: Find a single confidence-interval root
# ----------------------------------------------------------------------

#' Find a Confidence Interval Root by Log-Likelihood Inversion
#'
#' @description
#' Solves for a root of
#' \deqn{\ell(\psi) - \ell(\hat\psi) + c = 0}
#' using numeric root finding. Returns \code{NA_real_} on failure.
#'
#' @param zero_max_psi_ll_fn Function returning
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param crit         Numeric scalar. Chi-square cutoff
#'   \eqn{c = \tfrac{1}{2}\chi^2_{1-\alpha,1}}.
#' @param search_range Numeric length-2 vector. Search interval.
#'
#' @return Numeric scalar root, or \code{NA_real_}.
#'
#' @keywords internal
find_interval_endpoint <- function(zero_max_psi_ll_fn, crit, search_range) {
  tryCatch(
    stats::uniroot(
      f = function(psi) zero_max_psi_ll_fn(psi) + crit,
      interval = search_range
    )$root,
    error = function(e) NA_real_
  )
}

# ----------------------------------------------------------------------
# Internal: Shift log-likelihood by a constant
# ----------------------------------------------------------------------

#' Shift a Psi Log-Likelihood Function
#'
#' @description
#' Returns a new function corresponding to \eqn{\ell(\psi) - c}.
#'
#' @param psi_ll_fn  Function returning \eqn{\ell(\psi)}.
#' @param shift_val  Numeric scalar shift value.
#'
#' @return Function of \eqn{\psi} returning the shifted log-likelihood.
#'
#' @keywords internal
shift_psi_ll_fn <- function(psi_ll_fn, shift_val) {
  function(psi) psi_ll_fn(psi) - shift_val
}

# ----------------------------------------------------------------------
# Internal: Compute raw confidence interval bounds for a single α
# ----------------------------------------------------------------------

#' Compute Raw Psi Confidence Interval Bounds
#'
#' @description
#' Computes lower and upper confidence interval bounds by inverting a
#' zero-maximized log-likelihood function at a single significance level.
#' The search range is bounded by the extent of \code{psi_grid}.
#'
#' @param point_estimate   Numeric scalar. MLE of \eqn{\psi}.
#' @param zero_max_psi_ll_fn Function returning
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param psi_grid         Numeric vector of evaluated \eqn{\psi} values.
#' @param alpha            Numeric scalar significance level.
#'
#' @return A tibble with columns \code{alpha}, \code{lower}, \code{upper}.
#'
#' @importFrom stats qchisq
#' @keywords internal
estimate_interval <- function(
  point_estimate,
  zero_max_psi_ll_fn,
  psi_grid,
  alpha
) {
  crit <- 0.5 * qchisq(1 - alpha, df = 1)

  lower <- find_interval_endpoint(
    zero_max_psi_ll_fn,
    crit,
    c(min(psi_grid), point_estimate)
  )

  upper <- find_interval_endpoint(
    zero_max_psi_ll_fn,
    crit,
    c(point_estimate, max(psi_grid))
  )

  tibble::tibble(alpha = alpha, lower = lower, upper = upper)
}

# ----------------------------------------------------------------------
# Internal: Add CI diagnostics and truth coverage
# ----------------------------------------------------------------------

#' Add Diagnostic Quantities to Psi Confidence Intervals
#'
#' @param interval_estimate_df Data frame with columns \code{alpha},
#'   \code{lower}, \code{upper}.
#' @param point_estimate Numeric scalar. MLE of \eqn{\psi}.
#' @param psi_0          Optional numeric scalar. True value of \eqn{\psi}.
#'
#' @return Data frame with additional diagnostic columns.
#'
#' @keywords internal
add_interval_diagnostics <- function(
  interval_estimate_df,
  point_estimate,
  psi_0 = NA_real_
) {
  interval_estimate_df <- interval_estimate_df |>
    dplyr::mutate(
      length = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_real_,
        upper - lower
      ),
      lower_dev = dplyr::if_else(
        is.na(lower),
        NA_real_,
        point_estimate - lower
      ),
      upper_dev = dplyr::if_else(
        is.na(upper),
        NA_real_,
        upper - point_estimate
      ),
      contains_truth = dplyr::case_when(
        is.na(psi_0) ~ NA,
        TRUE ~ (!is.na(lower) & !is.na(upper) & lower <= psi_0 & upper >= psi_0)
      )
    )

  attr(interval_estimate_df, "point_estimate") <- point_estimate
  attr(interval_estimate_df, "psi_0") <- psi_0

  interval_estimate_df
}

# ----------------------------------------------------------------------
# Internal: Format confidence intervals for presentation
# ----------------------------------------------------------------------

#' Format Psi Confidence Interval Table for Display
#'
#' @param interval_estimate_df Data frame of CI bounds and diagnostics.
#' @param digits Integer. Decimal places for rounding. Default: \code{2}.
#'
#' @return Formatted data frame suitable for display.
#'
#' @keywords internal
format_interval_estimate_df <- function(interval_estimate_df, digits = 2) {
  formatted_df <- interval_estimate_df |>
    dplyr::mutate(
      level = scales::percent(1 - alpha),
      interval = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_character_,
        sprintf("[%.2f, %.2f]", lower, upper)
      )
    ) |>
    dplyr::select(
      interval,
      length,
      lower_dev,
      upper_dev,
      contains_truth,
      level
    ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits)))

  attr(formatted_df, "interval_estimate_raw") <- interval_estimate_df |>
    dplyr::select(alpha, lower, upper)

  formatted_df
}

# ----------------------------------------------------------------------
# Internal: Compute confidence intervals across all α levels
# ----------------------------------------------------------------------

#' Compute Confidence Intervals for a Scalar Parameter Psi
#'
#' @description
#' Computes likelihood-based confidence intervals by inverting a
#' zero-maximized log-likelihood function over multiple confidence levels.
#'
#' @param point_estimate     Numeric scalar. MLE of \eqn{\psi}.
#' @param zero_max_psi_ll_fn Function returning
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param psi_ll_df          Data frame with at least a \code{psi} column.
#' @param alpha_levels       Numeric vector of significance levels.
#' @param psi_0              Optional numeric scalar. True value of
#'   \eqn{\psi}. Default: \code{NA_real_}.
#'
#' @return Formatted data frame of confidence interval summaries.
#'
#' @keywords internal
get_interval_estimate_df <- function(
  point_estimate,
  zero_max_psi_ll_fn,
  psi_ll_df,
  alpha_levels,
  psi_0 = NA_real_
) {
  psi_grid <- psi_ll_df$psi

  alpha_levels |>
    purrr::map_dfr(
      \(alpha) {
        estimate_interval(
          point_estimate = point_estimate,
          zero_max_psi_ll_fn = zero_max_psi_ll_fn,
          psi_grid = psi_grid,
          alpha = alpha
        )
      }
    ) |>
    add_interval_diagnostics(point_estimate, psi_0) |>
    format_interval_estimate_df()
}

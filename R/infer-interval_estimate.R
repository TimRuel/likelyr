# ========================================================================
# infer-interval_estimate.R — Likelihood-based confidence intervals for ψ
# ========================================================================

# ----------------------------------------------------------------------
# Internal: Shift log-likelihood by a constant
# ----------------------------------------------------------------------

#' Shift a Psi Log-Likelihood Function
#'
#' @description
#' Returns a new function corresponding to \eqn{\ell(\psi) + shift_val}.
#'
#' @param psi_loglik Function returning \eqn{\ell(\psi)}.
#' @param shift_val  Numeric scalar shift value.
#'
#' @return Function of \eqn{\psi} returning the shifted log-likelihood.
#'
#' @keywords internal
shift_psi_loglik <- function(psi_loglik, shift_val) {
  psi_loglik_shifted <- function(psi) psi_loglik(psi) + shift_val
  attr(psi_loglik_shifted, "psi_range") <- attr(psi_loglik, "psi_range")
  psi_loglik_shifted
}

# ----------------------------------------------------------------------
# Internal: Find confidence interval endpoints
# ----------------------------------------------------------------------

#' Find Confidence Interval Endpoints by Log-Likelihood Inversion
#'
#' @description
#' Returns endpoints for a \eqn{(1-\alpha) 100\%} likelihood ratio
#' confidence interval for \eqn{\psi} by solving for the roots of
#' \deqn{\ell(\psi) - \ell(\hat\psi) + \tfrac{1}{2}\chi^2_{1-\alpha,1} = 0.}
#'
#' If \code{uniroot} fails on a side because the grid reaches a declared
#' parameter space boundary before descending to the critical threshold,
#' the boundary value is substituted for that endpoint. If the grid
#' simply did not descend far enough without hitting a boundary,
#' \code{NA_real_} is returned for that endpoint.
#'
#' @param psi_loglik   Function returning \eqn{\ell(\psi)}.
#' @param alpha        Numeric scalar in (0, 1).
#' @param psi_interval Optional named list with \code{$lower} and
#'   \code{$upper} slots. When provided, boundary substitution is
#'   applied on each side independently.
#'
#' @return A one-row tibble with columns \code{alpha}, \code{lower},
#'   \code{upper}, and attribute \code{psi_hat}.
#'
#' @keywords internal
find_interval_endpoints <- function(psi_loglik, alpha, psi_interval = NULL) {
  crit <- 0.5 * stats::qchisq(1 - alpha, df = 1)
  psi_loglik_max_point <- get_psi_loglik_max_point(psi_loglik)
  psi_hat <- psi_loglik_max_point[["argmax"]]
  psi_loglik_max <- psi_loglik_max_point[["maximum"]]
  shift_val <- crit - psi_loglik_max
  psi_loglik_shifted <- shift_psi_loglik(psi_loglik, shift_val)

  psi_range <- attr(psi_loglik, "psi range")
  tol <- sqrt(.Machine$double.eps)

  # ------------------------------------------------------------------
  # Lower endpoint
  # ------------------------------------------------------------------
  lower <- tryCatch(
    stats::uniroot(
      f = psi_loglik_shifted,
      interval = c(psi_range[1], psi_hat)
    )$root,
    error = function(e) {
      # Boundary substitution: grid left edge is at declared lower bound
      # and the likelihood is still above the critical threshold there
      if (
        !is.null(min(psi_interval)) &&
          abs(psi_range[1] - min(psi_interval)) < tol &&
          psi_loglik_shifted(psi_range[1]) > 0
      ) {
        min(psi_interval)
      } else {
        NA_real_
      }
    }
  )

  # ------------------------------------------------------------------
  # Upper endpoint
  # ------------------------------------------------------------------
  upper <- tryCatch(
    stats::uniroot(
      f = psi_loglik_shifted,
      interval = c(psi_hat, psi_range[2])
    )$root,
    error = function(e) {
      # Boundary substitution: grid right edge is at declared upper bound
      # and the likelihood is still above the critical threshold there
      if (
        !is.null(psi_interval$upper) &&
          abs(psi_range[2] - psi_interval$upper) < tol &&
          psi_loglik_shifted(psi_range[2]) > 0
      ) {
        psi_interval$upper
      } else {
        NA_real_
      }
    }
  )

  ci_endpoints <- tibble::tibble(alpha = alpha, lower = lower, upper = upper)
  attr(ci_endpoints, "psi_hat") <- psi_hat
  ci_endpoints
}

# ----------------------------------------------------------------------
# Internal: Add CI diagnostics and truth coverage
# ----------------------------------------------------------------------

#' Add Diagnostic Quantities to Psi Confidence Intervals
#'
#' @param interval_estimate_df Data frame with columns \code{alpha},
#'   \code{lower}, \code{upper}.
#' @param psi_0 Optional numeric scalar. True value of \eqn{\psi}.
#'
#' @return Data frame with additional diagnostic columns.
#'
#' @keywords internal
add_interval_diagnostics <- function(interval_estimate_df, psi_0 = NA_real_) {
  psi_hat <- attr(interval_estimate_df, "psi_hat")

  interval_estimate_df <- interval_estimate_df |>
    dplyr::mutate(
      length = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_real_,
        upper - lower
      ),
      lower_dev = dplyr::if_else(is.na(lower), NA_real_, psi_hat - lower),
      upper_dev = dplyr::if_else(is.na(upper), NA_real_, upper - psi_hat),
      contains_truth = if (is.na(psi_0)) {
        NA
      } else {
        !is.na(lower) & !is.na(upper) & lower <= psi_0 & upper >= psi_0
      }
    )

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
#' @param psi_loglik_df A data frame with columns \code{psi} and
#'   \code{loglik}.
#' @param alpha_levels  Numeric vector of significance levels.
#' @param psi_0         Optional numeric scalar. True value of \eqn{\psi}.
#'   Default: \code{NA_real_}.
#' @param psi_interval  Optional named list with \code{$lower} and
#'   \code{$upper} slots. Passed to \code{find_interval_endpoints()} for
#'   boundary substitution.
#'
#' @return Formatted data frame of confidence interval summaries.
#'
#' @keywords internal
get_interval_estimate_df <- function(
  psi_loglik_df,
  alpha_levels,
  psi_0 = NA_real_,
  psi_interval = NULL
) {
  psi_loglik <- fit_psi_loglik(psi_loglik_df)

  interval_estimate_df <- alpha_levels |>
    purrr::map_dfr(
      \(alpha) {
        find_interval_endpoints(
          psi_loglik = psi_loglik,
          alpha = alpha,
          psi_interval = psi_interval
        )
      }
    ) |>
    add_interval_diagnostics(psi_0) |>
    format_interval_estimate_df()

  attr(interval_estimate_df, "pseudolikelihood") <- attr(
    psi_loglik_df,
    "pseudolikelihood"
  )

  interval_estimate_df
}

# ========================================================================
# END infer-interval_estimate.R
# ========================================================================

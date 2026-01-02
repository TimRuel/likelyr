# ======================================================================
# infer-interval_estimate.R — Likelihood-based confidence intervals for ψ
# ======================================================================

# ----------------------------------------------------------------------
# Internal: Find a single confidence-interval root
# ----------------------------------------------------------------------

#' Find a Confidence Interval Root by Log-Likelihood Inversion
#'
#' @description
#' Internal helper that solves for a root of the shifted log-likelihood
#' equation
#' \deqn{
#'   \ell(\psi) - \ell(\hat\psi) + c = 0
#' }
#' using numeric root finding.
#'
#' Root-finding failures return `NA_real_` rather than erroring.
#'
#' @param zero_max_psi_ll_fn A function of \eqn{\psi} returning the
#'   zero-maximized log-likelihood
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param crit Numeric scalar giving the chi-square cutoff
#'   \eqn{c = \tfrac{1}{2}\chi^2_{1-\alpha,1}}.
#' @param search_range Numeric length-2 vector giving the interval over
#'   which to search for a root.
#'
#' @return
#' Numeric scalar giving the root, or `NA_real_` if root finding fails.
#'
#' @keywords internal
find_interval_endpoint <- function(zero_max_psi_ll_fn, crit, search_range) {

  tryCatch(
    stats::uniroot(
      f        = function(psi) zero_max_psi_ll_fn(psi) + crit,
      interval = search_range
    )$root,
    error = function(e) NA_real_
  )
}

# ----------------------------------------------------------------------
# Internal: Expand ψ bounds for robust root finding
# ----------------------------------------------------------------------

#' Expand Psi Search Bounds Around the MLE
#'
#' @description
#' Constructs expanded lower and upper bounds for confidence interval
#' root-finding by extending the observed ψ grid multiplicatively
#' around the MLE.
#'
#' @param psi_grid Numeric vector of evaluated \eqn{\psi} values.
#' @param point_estimate Numeric scalar giving the MLE of \eqn{\psi}.
#' @param expand_factor Numeric scalar controlling the multiplicative
#'   expansion applied to each side of the grid.
#'
#' @return
#' A list with elements `lower` and `upper`.
#'
#' @keywords internal
expand_psi_bounds <- function(psi_grid, point_estimate, expand_factor) {

  psi_min0 <- min(psi_grid)
  psi_max0 <- max(psi_grid)

  list(
    lower = psi_min0 - (point_estimate - psi_min0) * expand_factor,
    upper = psi_max0 + (psi_max0 - point_estimate) * expand_factor
  )
}

# ----------------------------------------------------------------------
# Internal: Shift log-likelihood by a constant
# ----------------------------------------------------------------------

#' Shift a Psi Log-Likelihood Function
#'
#' @description
#' Returns a new function corresponding to
#' \eqn{\ell(\psi) - c}, where \eqn{c} is a constant shift.
#'
#' @param zero_max_psi_ll_fn Function returning \eqn{\ell(\psi)}.
#' @param shift_val Numeric scalar giving the shift value.
#'
#' @return
#' A function of \eqn{\psi} returning the shifted log-likelihood.
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
#' Computes lower and upper confidence interval bounds for a scalar
#' parameter \eqn{\psi} by inverting a zero-maximized log-likelihood
#' function at a single significance level.
#'
#' This function returns *raw numeric bounds only* and performs no
#' formatting or diagnostics.
#'
#' @param point_estimate Numeric scalar giving the MLE of \eqn{\psi}.
#' @param zero_max_psi_ll_fn Function returning
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param psi_grid Numeric vector of evaluated \eqn{\psi} values.
#' @param alpha Numeric scalar giving the significance level.
#' @param expand_factor Numeric scalar controlling multiplicative
#'   expansion of the search bounds.
#'
#' @return
#' A tibble with columns `alpha`, `lower`, and `upper`.
#'
#' @keywords internal
estimate_interval <- function(
    point_estimate,
    zero_max_psi_ll_fn,
    psi_grid,
    alpha,
    expand_factor
) {

  crit   <- 0.5 * stats::qchisq(1 - alpha, df = 1)
  bounds <- expand_psi_bounds(psi_grid, point_estimate, expand_factor)

  lower <- find_interval_endpoint(
    zero_max_psi_ll_fn,
    crit,
    c(bounds$lower, point_estimate)
  )

  upper <- find_interval_endpoint(
    zero_max_psi_ll_fn,
    crit,
    c(point_estimate, bounds$upper)
  )

  tibble::tibble(
    alpha = alpha,
    lower = lower,
    upper = upper
  )
}

# ----------------------------------------------------------------------
# Internal: Add CI diagnostics and truth coverage
# ----------------------------------------------------------------------

#' Add Diagnostic Quantities to Psi Confidence Intervals
#'
#' @description
#' Augments raw confidence interval bounds with interval length,
#' deviations from the MLE, and optional truth coverage indicators.
#'
#' @param interval_estimate_df Data frame with columns `alpha`, `lower`,
#'   and `upper`.
#' @param point_estimate Numeric scalar giving the MLE of \eqn{\psi}.
#' @param psi_0 Optional numeric scalar giving the true value of
#'   \eqn{\psi}. If `NA`, truth coverage diagnostics are omitted.
#'
#' @return
#' A data frame with additional diagnostic columns.
#'
#' @keywords internal
add_interval_diagnostics <- function(
    interval_estimate_df,
    point_estimate,
    psi_0 = NA_real_
) {

  interval_estimate_df <- interval_estimate_df |>
    dplyr::mutate(
      Length = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_real_,
        upper - lower
      ),

      `Lower Deviation` = dplyr::if_else(
        is.na(lower),
        NA_real_,
        point_estimate - lower
      ),

      `Upper Deviation` = dplyr::if_else(
        is.na(upper),
        NA_real_,
        upper - point_estimate
      ),

      contains_truth = dplyr::case_when(
        is.na(psi_0) ~ NA,
        TRUE ~ (!is.na(lower) & !is.na(upper) &
                  lower <= psi_0 & upper >= psi_0)
      ),

      Status = dplyr::case_when(
        is.na(contains_truth) ~ NA_character_,
        contains_truth        ~ "✅",
        TRUE                  ~ "❌"
      )
    )

  attr(interval_estimate_df, "point_estimate") <- point_estimate
  attr(interval_estimate_df, "psi_0") <- psi_0

  return(interval_estimate_df)
}

# ----------------------------------------------------------------------
# Internal: Format confidence intervals for presentation
# ----------------------------------------------------------------------

#' Format Psi Confidence Interval Table for Display
#'
#' @description
#' Converts confidence interval results into a human-readable table by
#' adding confidence levels, formatted interval strings, and rounding
#' numeric quantities.
#'
#' This function is strictly a *presentation helper*.
#'
#' @param interval_estimate_df Data frame containing confidence interval
#'   bounds and diagnostics.
#' @param digits Integer giving the number of decimal places to round
#'   numeric columns.
#'
#' @return
#' A formatted data frame suitable for display.
#'
#' @keywords internal
format_interval_estimate_df <- function(interval_estimate_df, digits = 2) {

  formatted_df <- interval_estimate_df |>
    dplyr::mutate(
      Level = scales::percent(1 - alpha),

      Interval = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_character_,
        sprintf("[%.2f, %.2f]", lower, upper)
      )
    ) |>
    dplyr::select(
      Interval,
      Length,
      `Lower Deviation`,
      `Upper Deviation`,
      Status,
      Level,
    ) |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, digits)
      )
    )

  attr(formatted_df, "interval_estimate_raw") <- interval_estimate_df |>
    dplyr::select(alpha, lower, upper)

  return(formatted_df)
}

# ----------------------------------------------------------------------
# Public API
# ----------------------------------------------------------------------

#' Compute Confidence Intervals for a Scalar Parameter Psi
#'
#' @description
#' Computes likelihood-based confidence intervals for a scalar parameter
#' \eqn{\psi} by inverting a zero-maximized log-likelihood function over
#' multiple confidence levels.
#'
#' The returned table includes interval endpoints, lengths, deviations
#' from the MLE, and optional truth coverage diagnostics.
#'
#' @param point_estimate Numeric scalar giving the MLE of \eqn{\psi}.
#' @param zero_max_psi_ll_fn Function returning
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param psi_ll_df Data frame containing at least a column `psi` giving
#'   the evaluated \eqn{\psi} grid.
#' @param alpha_levels Numeric vector of significance levels.
#' @param expand_factor Numeric scalar controlling multiplicative
#'   expansion of the search bounds.
#' @param psi_0 Optional numeric scalar giving the true value of
#'   \eqn{\psi}. If `NA`, truth coverage is not evaluated.
#'
#' @return
#' A formatted data frame containing confidence interval summaries for
#' each confidence level.
#'
#' @export
get_interval_estimate_df <- function(
    point_estimate,
    zero_max_psi_ll_fn,
    psi_ll_df,
    alpha_levels,
    expand_factor,
    psi_0 = NA_real_
) {

  psi_grid <- psi_ll_df$psi

  alpha_levels |>
    purrr::map_dfr(
      \(alpha) estimate_interval(
        point_estimate     = point_estimate,
        zero_max_psi_ll_fn = zero_max_psi_ll_fn,
        psi_grid           = psi_grid,
        alpha              = alpha,
        expand_factor      = expand_factor
      )
    ) |>
    add_interval_diagnostics(point_estimate, psi_0) |>
    format_interval_estimate_df()
}

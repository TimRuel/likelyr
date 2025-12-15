# ======================================================================
# Psi Confidence Intervals (Likelihood Inversion)
# ======================================================================

# ----------------------------------------------------------------------
# Internal: Find a single confidence-interval root
# ----------------------------------------------------------------------

#' Find a Confidence Interval Root by Likelihood Inversion
#'
#' @description
#' Internal helper that solves for a root of the shifted log-likelihood
#' equation
#' \deqn{
#'   \ell(\psi) - \ell(\hat\psi) + c = 0
#' }
#' using numeric root finding. Failures return `NA_real_` rather than
#' erroring.
#'
#' @param fn A function of \eqn{\psi} returning the zero-maximized
#'   log-likelihood \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param crit Numeric scalar giving the chi-square cutoff
#'   \eqn{c = \frac{1}{2}\chi^2_{1-\alpha,1}}.
#' @param interval Numeric length-2 vector giving the search interval.
#'
#' @return Numeric scalar giving the root, or `NA_real_` if root finding
#'   fails.
#'
#' @keywords internal
.find_ci_root <- function(fn, crit, interval) {

  tryCatch(
    stats::uniroot(
      f        = function(psi) fn(psi) + crit,
      interval = interval
    )$root,
    error = function(e) NA_real_
  )
}

# ----------------------------------------------------------------------
# Internal: Expand psi bounds for robust root finding
# ----------------------------------------------------------------------

#' Expand Psi Search Bounds Around the MLE
#'
#' @description
#' Constructs expanded lower and upper bounds for confidence interval
#' root-finding by extending the observed psi grid multiplicatively
#' around the MLE. This improves robustness when CI endpoints lie
#' outside the evaluated grid.
#'
#' @param psi_grid Numeric vector of evaluated \eqn{\psi} values.
#' @param psi_hat Numeric scalar giving the MLE of \eqn{\psi}.
#' @param expand_factor Numeric scalar giving the multiplicative expansion
#'   factor applied to the left and right grid widths.
#'
#' @return A list with elements `lower` and `upper` giving expanded bounds.
#'
#' @keywords internal
.expand_psi_bounds <- function(psi_grid, psi_hat, expand_factor) {

  psi_min0 <- min(psi_grid)
  psi_max0 <- max(psi_grid)

  list(
    lower = psi_min0 - (psi_hat - psi_min0) * expand_factor,
    upper = psi_max0 + (psi_max0 - psi_hat) * expand_factor
  )
}

# ----------------------------------------------------------------------
# Internal: Compute raw confidence interval bounds for a single alpha
# ----------------------------------------------------------------------

#' Compute Raw Psi Confidence Interval Bounds
#'
#' @description
#' Computes lower and upper confidence interval bounds for a scalar
#' parameter \eqn{\psi} by inverting a zero-maximized log-likelihood
#' function at a given confidence level.
#'
#' No rounding, formatting, or diagnostic quantities are added; this
#' function returns only the raw numeric bounds.
#'
#' @param zero_max_psi_ll_fn Function of \eqn{\psi} returning the
#'   zero-maximized log-likelihood
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param psi_hat Numeric scalar giving the MLE of \eqn{\psi}.
#' @param psi_grid Numeric vector of evaluated \eqn{\psi} values.
#' @param alpha Numeric scalar giving the significance level.
#' @param expand_factor Numeric scalar controlling multiplicative
#'   expansion of the search bounds.
#'
#' @return A tibble with columns `alpha`, `lower`, and `upper`.
#'
#' @keywords internal
.compute_psi_ci <- function(
    zero_max_psi_ll_fn,
    psi_ll_argmax,
    psi_grid,
    alpha,
    expand_factor
) {

  crit   <- 0.5 * stats::qchisq(1 - alpha, df = 1)
  bounds <- .expand_psi_bounds(psi_grid, psi_ll_argmax, expand_factor)

  lower <- .find_ci_root(
    zero_max_psi_ll_fn,
    crit,
    c(bounds$lower, psi_hat)
  )

  upper <- .find_ci_root(
    zero_max_psi_ll_fn,
    crit,
    c(psi_hat, bounds$upper)
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
#' deviations from the MLE, and (optionally) truth coverage indicators.
#'
#' @param df A data frame containing columns `alpha`, `lower`, and `upper`.
#' @param psi_ll_argmax Numeric scalar giving the MLE of \eqn{\psi}.
#' @param psi_0 Optional numeric scalar giving the true value of
#'   \eqn{\psi}. If `NA`, truth coverage diagnostics are omitted.
#'
#' @return A data frame with additional diagnostic columns.
#'
#' @keywords internal
.add_ci_diagnostics <- function(df, psi_ll_argmax, psi_0 = NA_real_) {

  df |>
    dplyr::mutate(
      Length = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_real_,
        upper - lower
      ),

      `Lower Deviation` = dplyr::if_else(
        is.na(lower),
        NA_real_,
        psi_ll_argmax - lower
      ),

      `Upper Deviation` = dplyr::if_else(
        is.na(upper),
        NA_real_,
        upper - psi_ll_argmax
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
}

# ----------------------------------------------------------------------
# Internal: Format confidence intervals for presentation
# ----------------------------------------------------------------------

#' Format Psi Confidence Interval Table for Display
#'
#' @description
#' Converts raw confidence interval results into a human-readable table
#' by adding confidence levels, formatted interval strings, and rounding
#' numeric quantities.
#'
#' @param df A data frame containing confidence interval bounds and
#'   diagnostics.
#' @param digits Integer giving the number of decimal places to round
#'   numeric columns.
#'
#' @return A formatted data frame suitable for display.
#'
#' @keywords internal
.format_ci_table <- function(df, digits = 2) {

  df |>
    dplyr::mutate(
      Level = scales::percent(1 - alpha),

      Interval = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_character_,
        sprintf("[%.2f, %.2f]", lower, upper)
      )
    ) |>
    dplyr::select(
      Level,
      Interval,
      Length,
      `Lower Deviation`,
      `Upper Deviation`,
      Status
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.numeric),
        .fns  = ~ round(.x, digits)
      )
    )
}

# ----------------------------------------------------------------------
# Public API
# ----------------------------------------------------------------------

#' Compute Confidence Intervals for a Scalar Parameter Psi
#'
#' @description
#' Computes likelihood-based confidence intervals for a scalar parameter
#' \eqn{\psi} by inverting a zero-maximized log-likelihood function over
#' multiple confidence levels. The resulting table includes interval
#' endpoints, lengths, deviations from the MLE, and optional truth
#' coverage diagnostics.
#'
#' @param zero_max_psi_ll_fn Function of \eqn{\psi} returning the
#'   zero-maximized log-likelihood
#'   \eqn{\ell(\psi) - \ell(\hat\psi)}.
#' @param psi_ll_argmax Numeric scalar giving the MLE of \eqn{\psi}.
#' @param psi_ll_df Data frame containing at least a column `psi` giving
#'   the evaluated \eqn{\psi} grid.
#' @param alpha_levels Numeric vector of significance levels.
#' @param uniroot_expand_factor Numeric scalar controlling multiplicative
#'   expansion of the search bounds for root finding.
#' @param psi_0 Optional numeric scalar giving the true value of
#'   \eqn{\psi}. If `NA`, truth coverage is not evaluated.
#'
#' @return A formatted data frame containing confidence interval summaries
#'   for each confidence level.
#'
#' @export
get_psi_conf_ints_df <- function(
    zero_max_psi_ll_fn,
    psi_ll_argmax,
    psi_ll_df,
    alpha_levels,
    uniroot_expand_factor,
    psi_0 = NA_real_
) {

  raw <- purrr::map_dfr(
    alpha_levels,
    ~ .compute_psi_ci(
      zero_max_psi_ll_fn = zero_max_psi_ll_fn,
      psi_ll_argmax      = psi_ll_argmax,
      psi_grid           = psi_ll_df$psi,
      alpha              = .x,
      expand_factor      = uniroot_expand_factor
    )
  )

  raw |>
    .add_ci_diagnostics(psi_ll_argmax, psi_0) |>
    .format_ci_table()
}

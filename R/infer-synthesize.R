# =====================================================================
# infer-synthesize.R — Synthesize likelihood-based inference results
# =====================================================================

#' Synthesize Likelihood-Based Inference Results
#'
#' @description
#' Orchestrates point estimation and confidence interval construction from
#' a ψ log-likelihood grid, returning a unified inference object containing
#' numeric results and the data required for downstream presentation.
#'
#' This function performs **no table rendering or plotting**. Tables/plots
#' are materialized later by `view()` and `plot()` (local-only).
#'
#' This function assumes that likelihood evaluation has already been
#' performed and that `psi_ll_df` represents a unimodal likelihood curve.
#'
#' @param psi_ll_df A data frame containing columns `psi` and `loglik`
#'   representing the evaluated log-likelihood curve.
#' @param alpha_levels Numeric vector of significance levels.
#' @param psi_0 Optional numeric scalar giving the true value of ψ.
#' @param expand_factor Numeric scalar controlling multiplicative expansion
#'   of the search bounds for confidence interval root finding.
#'
#' @return A named list containing:
#' \describe{
#'   \item{zero_max_psi_ll_fn}{Shifted/smoothed log-likelihood function with max at 0.}
#'   \item{point_estimate_df}{Data frame with ψ₀, ψ̂, error, and SE(ψ̂).}
#'   \item{interval_estimate_df}{Confidence interval diagnostics table (numeric).}
#'   \item{estimate_df}{Combined point + interval summary table (numeric).}
#' }
#'
#' @keywords internal
synthesize_inference <- function(
  psi_ll_df,
  alpha_levels,
  psi_0,
  expand_factor
) {
  type <- attr(psi_ll_df, "type")

  # --------------------------------------------------
  # Defensive checks
  # --------------------------------------------------
  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_ll_df))) {
    stop(
      "synthesize_inference(): psi_ll_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Smoothed likelihood
  # --------------------------------------------------
  psi_ll_fn <- fit_psi_ll_fn(psi_ll_df)

  # --------------------------------------------------
  # Point estimate + SE
  # --------------------------------------------------
  psi_ll_max_point <- get_psi_ll_max_point(psi_ll_fn, psi_ll_df)

  point_estimate <- psi_ll_max_point$argmax
  max_loglik <- psi_ll_max_point$maximum

  se_point_estimate <- get_se_point_estimate(point_estimate, psi_ll_df)

  point_estimate_df <- tibble::tibble(
    psi_0 = psi_0,
    psi_hat = point_estimate,
    error = point_estimate - psi_0,
    se_psi_hat = se_point_estimate
  ) |>
    round(2)

  attr(point_estimate_df, "type") <- type

  # --------------------------------------------------
  # Interval estimate
  # --------------------------------------------------
  zero_max_psi_ll_fn <- shift_psi_ll_fn(psi_ll_fn, max_loglik)

  interval_estimate_df <- get_interval_estimate_df(
    point_estimate = point_estimate,
    zero_max_psi_ll_fn = zero_max_psi_ll_fn,
    psi_ll_df = psi_ll_df,
    alpha_levels = alpha_levels,
    expand_factor = expand_factor,
    psi_0 = psi_0
  )

  attr(interval_estimate_df, "type") <- type

  # --------------------------------------------------
  # Synthesis table (numeric only)
  # --------------------------------------------------
  estimate_df <- point_estimate_df |>
    dplyr::bind_cols(interval_estimate_df) |>
    dplyr::select(
      se_psi_hat,
      error,
      psi_hat,
      psi_0,
      Interval,
      Length,
      `Lower Deviation`,
      `Upper Deviation`,
      Status,
      Level
    )

  attr(estimate_df, "type") <- type
  attr(estimate_df, "interval_estimate_raw") <- attr(
    interval_estimate_df,
    "interval_estimate_raw"
  )

  # --------------------------------------------------
  # Return data only (no tables, no plots)
  # --------------------------------------------------
  list(
    zero_max_psi_ll_fn = zero_max_psi_ll_fn,
    point_estimate_df = point_estimate_df,
    interval_estimate_df = interval_estimate_df,
    estimate_df = estimate_df
  )
}

# =====================================================================
# infer-synthesize.R — Synthesize likelihood-based inference results
# =====================================================================

#' Synthesize Likelihood-Based Inference Results
#'
#' @description
#' Orchestrates point estimation and confidence interval construction from
#' a ψ log-likelihood grid, returning a unified inference object containing
#' both numeric results and (optionally) rendered summaries.
#'
#' This function assumes that likelihood evaluation has already been
#' performed and that `psi_ll_df` represents a unimodal likelihood curve.
#'
#' @param psi_ll_df A data frame containing columns `psi` and `loglik`
#'   representing the evaluated log-likelihood curve.
#' @param alpha_levels Numeric vector of significance levels.
#' @param psi_0 Optional numeric scalar giving the true value of ψ.
#'   (e.g. `"profile"`, `"integrated"`). Used for labeling and rendering.
#' @param expand_factor Numeric scalar controlling multiplicative expansion
#'   of the search bounds for confidence interval root finding.
#'
#' @return
#' A named list containing:
#' \describe{
#'   \item{psi_ll_fn}{Smoothed log-likelihood function.}
#'   \item{point_estimate_df}{Data frame with ψ₀, ψ̂, and SE(ψ̂).}
#'   \item{interval_estimate_df}{Formatted confidence interval table.}
#'   \item{inference_df}{Combined point and interval estimate summary table.}
#'   \item{point_estimate_table}{Rendered point estimate table}
#'   \item{interval_estimate_table}{Rendered interval estimate table}
#'   \item{estimate_table}{Rendered combined estimate table}
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
      "synthesize(): psi_ll_df must contain columns ",
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
  max_loglik     <- psi_ll_max_point$maximum

  se_point_estimate <- get_se_point_estimate(point_estimate, psi_ll_df)

  point_estimate_df <- tibble::tibble(
    psi_0      = psi_0,
    psi_hat    = point_estimate,
    error      = point_estimate - psi_0,
    se_psi_hat = se_point_estimate
    ) |>
    round(2)

  attr(point_estimate_df, "type") <- type

  # --------------------------------------------------
  # Interval estimate
  # --------------------------------------------------
  zero_max_psi_ll_fn <- shift_psi_ll_fn(psi_ll_fn, max_loglik)

  interval_estimate_df <- get_interval_estimate_df(
    point_estimate     = point_estimate,
    zero_max_psi_ll_fn = zero_max_psi_ll_fn,
    psi_ll_df          = psi_ll_df,
    alpha_levels       = alpha_levels,
    expand_factor      = expand_factor,
    psi_0              = psi_0
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
  attr(estimate_df, "interval_estimate_raw") <- attr(interval_estimate_df, "interval_estimate_raw")

  point_estimate_table <- render_point_estimate_table(point_estimate_df)

  interval_estimate_table <- render_interval_estimate_table(interval_estimate_df)

  estimate_table <- render_estimate_table(estimate_df)

  list(
    zero_max_psi_ll_fn      = zero_max_psi_ll_fn,
    psi_ll_df               = psi_ll_df,
    point_estimate_df       = point_estimate_df,
    interval_estimate_df    = interval_estimate_df,
    estimate_df             = estimate_df,
    point_estimate_table    = point_estimate_table,
    interval_estimate_table = interval_estimate_table,
    estimate_table          = estimate_table
  )
}

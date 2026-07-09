# =====================================================================
# infer-synthesize.R — Synthesize likelihood-based inference results
# =====================================================================

#' Synthesize Likelihood-Based Inference Results
#'
#' @param psi_loglik_df A data frame containing columns \code{psi} and
#'   \code{loglik}.
#' @param alpha_levels  Numeric vector of significance levels.
#' @param psi_0         Optional numeric scalar. True value of ψ.
#' @param psi_interval  Optional sets::interval object.
#' @param enforce_concavity Logical. Whether to project the fitted spline
#'   onto its LCM before computing estimates. Default: \code{FALSE}.
#'
#' @return A named list with \code{point_estimate_df} and
#'   \code{interval_estimate_df}.
#'
#' @keywords internal
synthesize_inference <- function(
  psi_loglik_df,
  alpha_levels,
  psi_0,
  psi_interval = NULL,
  enforce_concavity = FALSE
) {
  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_loglik_df))) {
    stop(
      "synthesize_inference(): psi_loglik_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  point_estimate_df <- get_point_estimate_df(
    psi_loglik_df,
    psi_0,
    enforce_concavity = enforce_concavity
  )

  interval_estimate_df <- get_interval_estimate_df(
    psi_loglik_df = psi_loglik_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0,
    psi_interval = psi_interval,
    enforce_concavity = enforce_concavity
  )

  list(
    point_estimate_df = point_estimate_df,
    interval_estimate_df = interval_estimate_df
  )
}

# =====================================================================
# END infer-synthesize.R
# =====================================================================

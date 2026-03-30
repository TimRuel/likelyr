# =====================================================================
# infer-synthesize.R — Synthesize likelihood-based inference results
# =====================================================================

#' Synthesize Likelihood-Based Inference Results
#'
#' @description
#' Orchestrates point estimation and confidence interval construction
#' from a log-likelihood grid, returning point estimates, interval
#' estimates, and the zero-maximized likelihood function needed for
#' plotting.
#'
#' @param psi_loglik_df A data frame containing columns \code{psi} and
#'   \code{loglik}.
#' @param alpha_levels  Numeric vector of significance levels.
#' @param psi_0         Optional numeric scalar. True value of ψ.
#' @param psi_interval  Optional named list with \code{$lower} and
#'   \code{$upper} slots specifying declared parameter space boundaries.
#'   When provided, a boundary is substituted for \code{NA} if the
#'   likelihood grid reaches it before descending to the critical
#'   threshold.
#'
#' @return A named list with \code{point_estimate_df} and
#'   \code{interval_estimate_df}.
#'
#' @keywords internal
synthesize_inference <- function(
  psi_loglik_df,
  alpha_levels,
  psi_0,
  psi_interval = NULL
) {
  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_loglik_df))) {
    stop(
      "synthesize_inference(): psi_loglik_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  point_estimate_df <- get_point_estimate_df(psi_loglik_df)

  interval_estimate_df <- get_interval_estimate_df(
    psi_loglik_df = psi_loglik_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0,
    psi_interval = psi_interval
  )

  list(
    point_estimate_df = point_estimate_df,
    interval_estimate_df = interval_estimate_df
  )
}

# =====================================================================
# END infer-synthesize.R
# =====================================================================

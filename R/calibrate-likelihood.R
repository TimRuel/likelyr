# ======================================================================
# Likelihood Calibration  (Updated: v3.0)
# ======================================================================

#' Calibrate Likelihood Component
#'
#' @description
#' Binds data to the likelihood's log-likelihood closure.
#'
#' @param likelihood A likelihood_spec object.
#' @param data User data.
#'
#' @return The SAME likelihood_spec object, enriched with:
#'   • $loglik — a closure(param) that captures `data`
#'
#' @keywords internal
calibrate_likelihood <- function(likelihood, data) {
  stopifnot(inherits(likelihood, "likelihood_spec"))

  # -------------------------------------------------------------
  # 1. Bind data to loglik()
  # -------------------------------------------------------------
  orig_loglik <- likelihood$loglik

  likelihood$loglik <- function(param) {
    orig_loglik(param, data)
  }

  likelihood
}

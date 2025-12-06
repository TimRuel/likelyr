# ======================================================================
# Estimand Calibration
# ======================================================================

#' Calibrate Estimand Component
#'
#' @description
#' Produces data-bound psi_fn(), computes psi_MLE and search interval.
#'
#' @param estimand An estimand_spec object.
#' @param data User data.
#' @param theta_mle Numeric MLE of theta.
#'
#' @return The SAME estimand_spec object, enriched with:
#'         • $psi_fn           (data-bound)
#'         • $psi_jac          (data-bound, if applicable)
#'         • $psi_mle
#'         • $search_interval
#' @export
calibrate_estimand <- function(estimand, data, theta_mle) {

  stopifnot(inherits(estimand, "estimand_spec"))

  # -------------------------------------------------------------
  # 1. Bind ψ(θ, data)
  # -------------------------------------------------------------
  orig_psi_fn <- estimand$psi_fn
  estimand$psi_fn <- function(theta) orig_psi_fn(theta, data)

  # -------------------------------------------------------------
  # 2. Bind ψ Jacobian if present
  # -------------------------------------------------------------
  if (!is.null(estimand$psi_jac)) {
    orig_psi_jac <- estimand$psi_jac
    estimand$psi_jac <- function(theta) orig_psi_jac(theta, data)
  }

  # -------------------------------------------------------------
  # 3. Compute ψ̂_MLE
  # -------------------------------------------------------------
  psi_mle <- estimand$psi_fn(theta_mle)

  if (!is.numeric(psi_mle) || length(psi_mle) != 1L || !is.finite(psi_mle)) {
    stop("psi_fn(theta_mle) must return a finite numeric scalar.",
         call. = FALSE)
  }

  estimand$psi_mle <- psi_mle

  # -------------------------------------------------------------
  # 4. Compute search interval
  # -------------------------------------------------------------
  si <- estimand$search_interval_fn(data)

  if (!is.numeric(si) || length(si) != 2L || any(!is.finite(si)) || si[1] >= si[2]) {
    stop("search_interval_fn(data) must return c(lower, upper) with finite lower < upper.",
         call. = FALSE)
  }

  estimand$search_interval <- si

  estimand
}

# ======================================================================
# Nuisance Calibration
# ======================================================================

#' Calibrate Nuisance Component
#'
#' @param nuisance A nuisance_spec object or NULL.
#' @param data User data.
#'
#' @return The SAME nuisance_spec object, with:
#'         • $E_loglik       (data-bound)
#'         • $E_loglik_grad  (data-bound, if present)
#' @export
calibrate_nuisance <- function(nuisance, data) {

  stopifnot(inherits(nuisance, "nuisance_spec"))

  # -------------------------------------------------------------
  # 1. Bind E_loglik(theta, omega_hat, data)
  # -------------------------------------------------------------
  orig_E_loglik <- nuisance$E_loglik
  nuisance$E_loglik <- function(theta, omega_hat) {
    orig_E_loglik(theta, omega_hat, data)
  }

  # -------------------------------------------------------------
  # 2. Bind gradient if supplied
  # -------------------------------------------------------------
  if (!is.null(nuisance$E_loglik_grad)) {
    orig_grad <- nuisance$E_loglik_grad
    nuisance$E_loglik_grad <- function(theta, omega_hat) {
      orig_grad(theta, omega_hat, data)
    }
  }

  nuisance
}

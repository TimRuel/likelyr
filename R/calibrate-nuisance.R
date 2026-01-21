# ======================================================================
# Nuisance Calibration
# ======================================================================

#' Calibrate Nuisance Component
#'
#' @description
#' Binds user-supplied data to nuisance functions at calibration time.
#'
#' Data is injected lexically into the function environment (and removed
#' from the formal argument list) so that:
#'
#' * The user API remains `(param, omega_hat, data)`
#' * Downstream code uses `(param, omega_hat)`
#' * The original function body is preserved
#' * `future` can correctly detect helper dependencies
#'
#' @param nuisance A `nuisance_spec` object.
#' @param data User data.
#'
#' @return
#' The SAME `nuisance_spec` object, with:
#'   • `$E_loglik`       (data-bound)
#'   • `$E_loglik_grad`  (data-bound, if present)
#'
#' @keywords internal
#' @noRd
calibrate_nuisance <- function(nuisance, data) {
  stopifnot(inherits(nuisance, "nuisance_spec"))

  # -------------------------------------------------------
  # Bind expected log-likelihood
  # -------------------------------------------------------
  nuisance$E_loglik <- .bind_data_env(
    nuisance$E_loglik,
    data
  )

  # -------------------------------------------------------
  # Bind gradient (if supplied)
  # -------------------------------------------------------
  if (!is.null(nuisance$E_loglik_grad)) {
    nuisance$E_loglik_grad <- .bind_data_env(
      nuisance$E_loglik_grad,
      data
    )
  }

  nuisance
}

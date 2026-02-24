# ======================================================================
# Nuisance Calibration
# ======================================================================

#' Calibrate Nuisance Component
#'
#' @description
#' Binds user-supplied data and structural model components to nuisance
#' functions at calibration time.
#'
#' After calibration, all nuisance-related functions are fully closed-over
#' and ready for use in integrated likelihood evaluation.
#'
#' @param nuisance  A `nuisance_spec` object.
#' @param parameter Calibrated parameter component.
#' @param estimand  Calibrated estimand component.
#' @param optimizer Optimizer *spec* (pre-calibration).
#' @param data      User data.
#'
#' @return
#' The SAME `nuisance_spec` object, with:
#'   • `$E_loglik`              (data-bound)
#'   • `$E_loglik_grad`         (data-bound, if present)
#'   • `$omega_hat_initgen`     (calibrated)
#'   • `$omega_hat_sampler`     (calibrated)
#'
#' @keywords internal
#' @noRd
calibrate_nuisance <- function(
  nuisance,
  parameter,
  estimand,
  optimizer,
  data
) {
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

  # -------------------------------------------------------
  # Omega-hat calibration
  # -------------------------------------------------------
  spec <- nuisance$omega_hat %||% list()

  # ---- Initial-guess generator ----
  initgen_fn <- spec$initgen %||% make_omega_hat_initgen

  nuisance$omega_hat_initgen <- do.call(
    initgen_fn,
    .match_formals(
      initgen_fn,
      list(
        param_mle = parameter$param_mle,
        param_dim = parameter$param_dim,
        param_lower = parameter$param_lower,
        param_upper = parameter$param_upper,
        psi_jac = estimand$psi_jac
      )
    )
  )

  # ---- Omega-hat sampler ----
  sampler_fn <- spec$sampler %||% make_omega_hat_sampler

  nuisance$omega_hat_sampler <- do.call(
    sampler_fn,
    .match_formals(
      sampler_fn,
      list(
        psi_fn = estimand$psi_fn,
        psi_jac = estimand$psi_jac,
        psi_mle = estimand$psi_mle,
        eq_fn = parameter$eq,
        eq_jac = parameter$eq_jac,
        ineq_fn = parameter$ineq,
        ineq_jac = parameter$ineq_jac,
        optimizer = optimizer
      )
    )
  )

  nuisance
}

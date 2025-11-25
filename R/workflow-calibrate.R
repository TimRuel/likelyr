# =====================================================================
# Calibration Interface
# =====================================================================

#' Calibrate a Likelihood Workflow
#'
#' @description
#' Calibrates a likelihood workflow object for either integrated likelihood
#' (IL) or profile likelihood (PL) generation. This embeds data into all
#' likelihood & estimand functions, computes \eqn{\theta_{\mathrm{MLE}}},
#' validates the search interval, and attaches IL- or PL-specific fields.
#'
#' @param wf A `likelihood_workflow` object created by [workflow()].
#' @param data User data to embed into model/estimand closures.
#' @param type `"IL"` or `"PL"`.
#' @param verbose Logical; prints diagnostics if TRUE.
#'
#' @return
#' S3 object of class:
#' * `"IL_calibration", "likelihood_calibration"`
#' * `"PL_calibration", "likelihood_calibration"`
#'
#' @export
calibrate <- function(wf, data, type = c("IL", "PL"), verbose = TRUE) {

  type <- match.arg(type)

  if (!inherits(wf, "likelihood_workflow"))
    stop("`calibrate()` requires a likelihood_workflow.", call. = FALSE)

  if (type == "IL") {
    calibrate_IL(wf, data, verbose)
  } else {
    calibrate_PL(wf, data, verbose)
  }
}

# =====================================================================
# INTERNAL: Shared Calibration
# =====================================================================

#' @keywords internal
#' @noRd
.calibrate_common <- function(wf, data, verbose) {

  # Validate workflow structure before use
  .validate_calibration_inputs(wf)

  model     <- wf$model
  estimand  <- wf$estimand
  optimizer <- wf$optimizer
  param_dim <- model$param_dim

  # ------------------------------------------------------------
  # Bind data → closures
  # ------------------------------------------------------------
  loglik <- function(theta) model$loglik(theta, data)

  E_loglik <- if (!is.null(model$E_loglik)) {
    function(theta, omega_hat) model$E_loglik(theta, omega_hat, data)
  } else NULL

  E_loglik_grad <- if (!is.null(model$E_loglik_grad)) {
    function(theta, omega_hat) model$E_loglik_grad(theta, omega_hat, data)
  } else NULL

  psi_fn <- function(theta) estimand$psi_fn(theta, data)

  psi_jac <- if (!is.null(estimand$psi_jac)) {
    psi_jac <- function(theta) estimand$psi_jac(theta, data)
  } else NULL

  # ------------------------------------------------------------
  # Compute θ_MLE
  # ------------------------------------------------------------
  theta_mle <- model$theta_mle_fn
  if (!is.null(theta_mle)) {

    theta_mle <- theta_mle(data)

  } else {

    if (verbose)
      cat("[calibrate] Computing θ_MLE via optim()...\n")

    obj <- function(theta) -loglik(theta)

    theta_mle <- optim(
      par    = rep(0, param_dim),
      fn     = obj,
      method = "BFGS"
    )$par
  }

  psi_mle <- psi_fn(theta_mle)

  # ------------------------------------------------------------
  # Validate search interval
  # ------------------------------------------------------------
  si <- estimand$search_interval

  if (!is.numeric(si) ||
      length(si) != 2 ||
      si[1] >= si[2])
    stop("Invalid `search_interval` in estimand_spec().", call. = FALSE)

  if (verbose) {
    cat("[calibrate] θ_MLE = (", paste(format(theta_mle), collapse = ", "), ")\n", sep="")
    cat("[calibrate] ψ_MLE = ", psi_mle, "\n", sep="")
    cat("[calibrate] Branch-mode search interval = [",
        si[1], ", ", si[2], "]\n", sep="")
  }

  # ------------------------------------------------------------
  # Shared calibration state
  # ------------------------------------------------------------
  list(
    workflow        = wf,
    data            = data,
    theta_mle       = theta_mle,
    psi_mle         = psi_mle,
    psi_fn          = psi_fn,
    psi_jac         = psi_jac,
    loglik          = loglik,
    E_loglik        = E_loglik,
    E_loglik_grad   = E_loglik_grad,
    search_interval = si
  )
}

# =====================================================================
# INTERNAL VALIDATOR
# =====================================================================

# Ensures workflow has the required components before calibration starts
#' @keywords internal
#' @noRd
.validate_calibration_inputs <- function(wf) {

  if (is.null(wf$model) || !inherits(wf$model, "likelihood_model"))
    stop("Workflow must contain a valid model_spec().", call. = FALSE)

  if (is.null(wf$estimand) || !inherits(wf$estimand, "likelihood_estimand"))
    stop("Workflow must contain a valid estimand_spec().", call. = FALSE)

  if (is.null(wf$optimizer) || !inherits(wf$optimizer, "likelihood_optimizer"))
    stop("Workflow must contain a valid optimizer_spec().", call. = FALSE)

  invisible(wf)
}

# =====================================================================
# IL CALIBRATION
# =====================================================================

#' Calibrate a Workflow for Integrated Likelihood
#'
#' @description
#' Adds IL-specific calibration components:
#' * equality constraint ψ(θ) = ψ_MLE,
#' * nuisance initialization generator,
#' * nuisance manifold sampler.
#'
#' @inheritParams calibrate
#'
#' @return `"IL_calibration", "likelihood_calibration"`
#' @export
calibrate_IL <- function(wf, data, verbose = TRUE) {

  common <- .calibrate_common(wf, data, verbose)

  # IL requires nuisance spec
  if (is.null(wf$nuisance))
    stop("IL calibration requires nuisance_spec().", call. = FALSE)

  # Constraint ψ(θ) = ψ_MLE
  constraint_fn <- function(theta) {
    common$psi_fn(theta) - common$psi_mle
  }

  il <- list(
    constraint_fn    = constraint_fn,
    generate_init    = make_omega_hat_initgen(common),
    sample_omega_hat = make_omega_hat_sampler(common)
  )

  common$il <- il
  common$omega_hat_con_fn <- constraint_fn

  common$type <- "IL"
  class(common) <- c("IL_calibration", "likelihood_calibration")
  common
}

# =====================================================================
# PL CALIBRATION
# =====================================================================

#' Calibrate a Workflow for Profile Likelihood
#'
#' @description
#' Installs PL-specific calibration components (currently minimal but
#' extensible for future profile-SE functionality).
#'
#' @inheritParams calibrate
#'
#' @return `"PL_calibration", "likelihood_calibration"`
#' @export
calibrate_PL <- function(wf, data, verbose = TRUE) {

  common <- .calibrate_common(wf, data, verbose)

  common$type <- "PL"
  class(common) <- c("PL_calibration", "likelihood_calibration")
  common
}

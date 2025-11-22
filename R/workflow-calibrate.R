# =====================================================================
# Calibration Interface
# =====================================================================
# User-facing API:
#
#   cal <- calibrate(wf, data, type = "IL")
#   cal <- calibrate(wf, data, type = "PL")
#
# Returns an object of class:
#   "IL_calibration", "likelihood_calibration"
#   or
#   "PL_calibration", "likelihood_calibration"
# =====================================================================


calibrate <- function(wf, data, type = c("IL", "PL"), verbose = TRUE) {

  type <- match.arg(type)

  if (!inherits(wf, "likelihood_workflow"))
    stop("`calibrate()` requires a likelihood_workflow.", call. = FALSE)

  if (type == "IL") {
    calibrate_IL(wf, data, verbose = verbose)
  } else {
    calibrate_PL(wf, data, verbose = verbose)
  }
}


# =====================================================================
# INTERNAL: Common Calibration Tasks for BOTH IL and PL
# =====================================================================

.calibrate_common <- function(wf, data, verbose) {

  model     <- wf$model
  estimand  <- wf$estimand
  optimizer <- wf$optimizer
  param_dim <- model$param_dim

  # ------------------------------------------------------------
  # Bind data into likelihood and estimand functions
  # ------------------------------------------------------------
  loglik <- function(theta) model$loglik(theta, data)

  E_loglik <- NULL
  if (!is.null(model$E_loglik)) {
    E_loglik <- function(theta, omega_hat)
      model$E_loglik(theta, omega_hat, data)
  }

  E_loglik_grad <- NULL
  if (!is.null(model$E_loglik_grad)) {
    E_loglik_grad <- function(theta, omega_hat)
      model$E_loglik_grad(theta, omega_hat, data)
  }

  psi_fn <- function(theta) estimand$psi_fn(theta, data)

  psi_jac <- estimand$psi_jac
  if (!is.null(psi_jac)) {
    psi_jac <- function(theta) psi_jac(theta, data)
  }

  # ------------------------------------------------------------
  # Compute θ_MLE (user-supplied or via optim)
  # ------------------------------------------------------------
  theta_mle <- model$theta_mle_fn
  if (!is.null(theta_mle)) {

    theta_mle <- theta_mle(data)

  } else {

    if (verbose)
      cat("[calibrate] Computing θ_MLE using optim()...\n")

    obj <- function(theta) -loglik(theta)

    theta_mle <- optim(
      par    = rep(0, param_dim),
      fn     = obj,
      method = "BFGS"
    )$par
  }

  psi_mle <- psi_fn(theta_mle)

  # ------------------------------------------------------------
  # Validate ψ search interval
  # ------------------------------------------------------------
  search_interval <- estimand$search_interval

  if (!is.numeric(search_interval) ||
      length(search_interval) != 2 ||
      search_interval[1] >= search_interval[2]) {
    stop("Invalid search interval in estimand_spec().", call. = FALSE)
  }

  if (verbose) {
    cat("[calibrate] θ_MLE = (",
        paste(format(theta_mle), collapse = ", "), ")\n", sep = "")
    cat("[calibrate] ψ_MLE = ", psi_mle, "\n", sep="")
    cat("[calibrate] Branch-mode search interval = [",
        search_interval[1], ", ", search_interval[2], "]\n", sep="")
  }

  # ------------------------------------------------------------
  # Return shared calibration info
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
    search_interval = search_interval
  )
}


# =====================================================================
# IL CALIBRATION: Integrated Likelihood Calibration
# =====================================================================
# IL-specific components are placed under $il:
#
# cal$il$constraint_fn
# cal$il$generate_init
# cal$il$sample_omega_hat
#
# Keeping IL-specific logic separate prevents namespace clutter,
# improves extensibility, and keeps PL calibration clean.
# =====================================================================

calibrate_IL <- function(wf, data, verbose = TRUE) {

  common <- .calibrate_common(wf, data, verbose)

  # IL requires nuisance parameter specification
  if (is.null(wf$nuisance)) {
    stop(
      "IL calibration requires a workflow with a nuisance parameter specification.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # IL-specific constraint: ψ(theta) = ψ_MLE
  # ------------------------------------------------------------
  constraint_fn <- function(theta) {
    common$psi_fn(theta) - common$psi_mle
  }

  # ------------------------------------------------------------
  # IL-specific factories (from helpers/eval-omega-hat.R)
  # ------------------------------------------------------------
  il <- list(
    constraint_fn    = constraint_fn,
    generate_init    = make_omega_hat_initgen(common),
    sample_omega_hat = make_omega_hat_sampler(common)
  )

  common$il <- il

  # Convenience / backwards compatibility
  common$omega_hat_con_fn <- constraint_fn

  # ------------------------------------------------------------
  # Annotate calibration type
  # ------------------------------------------------------------
  common$type <- "IL"
  class(common) <- c("IL_calibration", "likelihood_calibration")
  common
}


# =====================================================================
# PL CALIBRATION: Profile Likelihood Calibration
# =====================================================================

calibrate_PL <- function(wf, data, verbose = TRUE) {

  common <- .calibrate_common(wf, data, verbose)

  # Future expansion: attach PL-specific info under common$pl
  # common$pl$profile_se <- compute_profile_se(...)

  common$type <- "PL"
  class(common) <- c("PL_calibration", "likelihood_calibration")
  common
}

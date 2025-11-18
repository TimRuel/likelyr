# =====================================================================
# CALIBRATION STEP
# Bind data-aware closures and compute theta_mle + psi_mle
# =====================================================================

#' Calibrate a likelihood workflow to observed data
#'
#' @description
#' `calibrate()` takes a fully specified `likelihood_workflow` and observed
#' data, embeds the data into the model and estimand (if requested),
#' determines the full-model MLE `theta_mle`, and evaluates the estimand
#' `psi_mle`. This produces a calibration object which is the required
#' starting point for branch-based profile or integrated likelihoods.
#'
#' Unlike the workflow itself, which is purely declarative and data-free,
#' the calibration output *binds* data to the likelihood and estimand so
#' they become functions of the model parameters alone.
#'
#' @param wf   A fully specified `likelihood_workflow` object.
#' @param data Observed data structure in any form required by the user’s
#'             likelihood and estimand implementations.
#' @param ...  Additional arguments forwarded to the optimizer if relevant.
#'
#' @return An S3 object of class `likelihood_calibration` containing:
#'   * `workflow`      Original workflow object
#'   * `data`          Stored data for reference
#'   * `theta_mle`     MLE (supplied or computed)
#'   * `psi_mle`       Estimand value at MLE
#'   * `loglik`        Data-bound function(theta)
#'   * `E_loglik`      Data+omega-bound function(theta, omega_hat)
#'   * `psi`           Data-bound estimand function(theta)
#'
#' @export
calibrate <- function(wf, data, ...) {

  if (!inherits(wf, "likelihood_workflow"))
    stop("`calibrate()` requires a `likelihood_workflow`.", call. = FALSE)

  # ------------------------------------------------------------
  # Extract functions from workflow
  # ------------------------------------------------------------
  loglik_fun   <- wf$model$loglik
  E_loglik_fun <- wf$model$E_loglik
  psi_fun      <- wf$estimand$psi
  theta_mle_in <- wf$model$theta_mle
  optimizer    <- wf$optimizer

  # ------------------------------------------------------------
  # Step 1: obtain full-model theta_mle
  # ------------------------------------------------------------

  if (!is.null(theta_mle_in)) {
    # User supplied MLE directly
    theta_mle <- theta_mle_in

  } else if (optimizer$strict) {
    stop("theta_mle not supplied in model_spec() and optimizer is strict.",
         call. = FALSE)

  } else {
    # Numerical optimization fallback
    if (is.null(optimizer$theta_init)) {
      stop("No `theta_mle` supplied and no `theta_init` specified for optimization.",
           call. = FALSE)
    }

    # objective function (negative loglik)
    full_obj <- function(theta) -loglik_fun(theta)

    if (optimizer$method == "nloptr") {
      opt <- nloptr::auglag(
        x0      = optimizer$theta_init,
        fn      = full_obj,
        control = optimizer$control %||% list()
      )
      theta_mle <- opt$par

    } else if (optimizer$method == "optim") {
      opt <- stats::optim(
        par     = optimizer$theta_init,
        fn      = full_obj,
        control = optimizer$control %||% list()
      )
      theta_mle <- opt$par

    } else {
      stop("Unsupported optimizer method: ", optimizer$method,
           call. = FALSE)
    }
  }

  # ------------------------------------------------------------
  # Step 2: evaluate estimand at MLE
  # ------------------------------------------------------------
  psi_mle <- psi_fun(theta_mle)

  # ------------------------------------------------------------
  # Step 3: construct bound closures
  # ------------------------------------------------------------

  loglik_bound <- function(theta) {
    loglik_fun(theta)
  }

  E_loglik_bound <- function(theta, omega_hat) {
    E_loglik_fun(theta, omega_hat)
  }

  psi_bound <- function(theta) {
    psi_fun(theta)
  }

  # ------------------------------------------------------------
  # Step 4: assemble calibration object
  # ------------------------------------------------------------
  out <- list(
    workflow   = wf,
    data       = data,
    theta_mle  = theta_mle,
    psi_mle    = psi_mle,
    loglik     = loglik_bound,
    E_loglik   = E_loglik_bound,
    psi        = psi_bound
  )

  class(out) <- "likelihood_calibration"
  out
}

# ---------------------------------------------------------------------
# PRINT METHOD
# ---------------------------------------------------------------------

#' @export
print.likelihood_calibration <- function(x, ...) {
  cat("# Likelihood Calibration\n")
  cat("- Model:        ", x$workflow$model$name,     "\n", sep = "")
  cat("- Estimand:     ", x$workflow$estimand$name,  "\n", sep = "")
  cat("- Theta MLE:    ", format(x$theta_mle),       "\n", sep = "")
  cat("- Psi MLE:      ", format(x$psi_mle),         "\n", sep = "")
  invisible(x)
}

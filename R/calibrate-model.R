# ======================================================================
# Calibration Dispatcher
# ======================================================================

#' Calibrate a model_spec to data
#'
#' @description
#' Prepares a model for computing profile or integrated likelihoods.
#' Calibration is modular: each structural component (likelihood,
#' estimand, nuisance) is processed by its own calibration helper.
#'
#' Optimizer and execution specs are *not required at calibration time*.
#' They may be supplied later and are validated by integrate()/profile().
#'
#' @param model   A `model_spec` object.
#' @param data    User data.
#' @param verbose Logical; print calibration diagnostics.
#'                Defaults to FALSE for pipe-friendly behavior.
#'
#' @return A `calibrated_model` object.
#' @export
calibrate <- function(model, data, verbose = FALSE) {
  UseMethod("calibrate")
}

#' @export
calibrate.default <- function(model, data, verbose = FALSE) {
  stop("calibrate() requires a model_spec object.", call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
calibrate.model_spec <- function(model, data, verbose = FALSE) {

  # -------------------------------------------------------------------
  # 1. Validate structural model specification before calibration
  # -------------------------------------------------------------------
  .validate_model_for_calibration(model)

  # Attach data (kept in calibrated_model)
  model$data <- data

  # -------------------------------------------------------------------
  # 2. Modular calibration of each spec component
  # -------------------------------------------------------------------

  # 2a. Likelihood — binds loglik(), computes θ̂_MLE
  model$likelihood <- calibrate_likelihood(model$likelihood, data)

  # 2b. Estimand — binds ψ(), computes ψ̂_MLE and search interval
  model$estimand <- calibrate_estimand(
    model$estimand,
    data      = data,
    theta_mle = model$likelihood$theta_mle
  )

  # 2c. Nuisance — binds E_loglik(), gradient structure, etc.
  model$nuisance <- calibrate_nuisance(model$nuisance, data)

  # 2d. Execution — allowed to be NULL at calibration time
  if (!is.null(model$execution)) {
    model$execution <- calibrate_execution(model$execution)
  }

  # (Optimizer is data-independent and not validated here.)

  # -------------------------------------------------------------------
  # 3. Wrap into calibrated_model
  # -------------------------------------------------------------------
  cal <- new_calibrated_model(model)

  # Mark calibration lock: structural specs can no longer be modified
  cal$.__calibrated__ <- TRUE

  # -------------------------------------------------------------------
  # 4. Optional console output
  # -------------------------------------------------------------------
  if (verbose) print(cal)

  cal
}

# ======================================================================
# INTERNAL VALIDATION
# ======================================================================

# Only structural components must be present for calibration:
#   • likelihood
#   • estimand
#   • nuisance
#
# optimizer_spec() and execution_spec() are NOT required here,
# and will be validated later by integrate() or profile().
.validate_model_for_calibration <- function(model) {

  if (!inherits(model$likelihood, "likelihood_spec"))
    stop("model$likelihood must be a likelihood_spec() before calibration.",
         call. = FALSE)

  if (!inherits(model$estimand, "estimand_spec"))
    stop("model$estimand must be an estimand_spec() before calibration.",
         call. = FALSE)

  if (!inherits(model$nuisance, "nuisance_spec"))
    stop("model$nuisance must be a nuisance_spec() before calibration.",
         call. = FALSE)

  invisible(model)
}

# ======================================================================
# PRINT METHOD (unchanged)
# ======================================================================

#' @export
print.calibrated_model <- function(x, ...) {

  theta_mle <- x$likelihood$theta_mle
  psi_mle   <- x$estimand$psi_mle
  interval  <- x$estimand$search_interval

  cat("# Calibrated Model (likelyr)\n")

  # core calibrated quantities
  cat("- θ̂:           (", paste(format(theta_mle), collapse = ", "), ")\n", sep = "")
  cat("- ψ̂:            ", format(psi_mle), "\n", sep = "")
  cat("- interval:    [", interval[1], ", ", interval[2], "]\n", sep = "")

  # state markers
  cat("- integrated:   ", if (is_integrated(x)) "✓" else "×", "\n", sep = "")
  cat("- profiled:     ", if (is_profiled(x))   "✓" else "×", "\n", sep = "")
  cat("- diagnosed:    ", if (is_diagnosed(x))  "✓" else "×", "\n", sep = "")
  cat("- inferred:     ", if (is_inferred(x))   "✓" else "×", "\n", sep = "")
  cat("- compared:     ", if (is_compared(x))   "✓" else "×", "\n", sep = "")

  invisible(x)
}

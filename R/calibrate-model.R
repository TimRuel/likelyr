# ======================================================================
# Calibration Dispatcher
# ======================================================================

#' Calibrate a model_spec to data
#'
#' @description
#' Prepares a model for computing profile or integrated likelihoods.
#' Calibration is modular: each structural component (parameter,
#' likelihood, estimand, pipeline, execution) is processed
#' by its own calibration helper.
#'
#' @param model   A `model_spec` object.
#' @param data    User data.
#' @param verbose Logical; print calibration diagnostics.
#'
#' @return A `calibrated` model object.
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
  validate_calibrate_input(model)

  model$data <- data

  # -------------------------------------------------------------------
  # 2. Modular calibration of structural components
  #
  # Order matters:
  #   parameter  — computes param_mle; needed by estimand and pipeline
  #   likelihood — binds data into loglik / E_loglik closures
  #   estimand   — binds data into psi_fn; computes psi_mle
  #   pipeline   — uses param_mle, psi_mle, and calibrated closures
  #                to build search interval, omega-hat closures, and
  #                branch mode locator
  # -------------------------------------------------------------------
  model$parameter <- calibrate_parameter(
    parameter = model$parameter,
    data = data
  )

  model$likelihood <- calibrate_likelihood(
    likelihood = model$likelihood,
    data = data
  )

  model$estimand <- calibrate_estimand(
    estimand = model$estimand,
    data = data,
    param_mle = model$parameter$param_mle,
    param_0 = model$parameter$param_0
  )

  model$pipeline <- calibrate_pipeline(
    pipeline = model$pipeline,
    parameter = model$parameter,
    likelihood = model$likelihood,
    estimand = model$estimand,
    solver = model$solver,
    data = data
  )

  # -------------------------------------------------------------------
  # 3. Optional execution calibration
  # -------------------------------------------------------------------
  if (!is.null(model$execution)) {
    model$execution <- calibrate_execution(model$execution)
  }

  # -------------------------------------------------------------------
  # 4. Wrap into calibrated model object
  # -------------------------------------------------------------------
  cal <- new_calibrated_model(model)

  # -------------------------------------------------------------------
  # 5. Initialize results workspace
  # -------------------------------------------------------------------
  cal$workspace <- new_workspace()

  # -------------------------------------------------------------------
  # 6. Optional console output
  # -------------------------------------------------------------------
  if (verbose) {
    print(cal)
  }

  cal
}

# ======================================================================
# INTERNAL VALIDATION
# ======================================================================

#' Validate structural components prior to calibration
#'
#' @keywords internal
#' @noRd
validate_calibrate_input <- function(model) {
  if (!inherits(model$parameter, "parameter_spec")) {
    stop(
      "model$parameter must be a parameter_spec() before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(model$likelihood, "likelihood_spec")) {
    stop(
      "model$likelihood must be a likelihood_spec() before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(model$estimand, "estimand_spec")) {
    stop(
      "model$estimand must be an estimand_spec() before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(model$pipeline, "pipeline_spec")) {
    stop(
      "model$pipeline must be a pipeline_spec() before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(model$solver, "solver_spec")) {
    stop(
      "model$solver must be a solver_spec() before calibration.",
      call. = FALSE
    )
  }

  invisible(model)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.calibrated <- function(x, ...) {
  param_mle <- x$parameter$param_mle
  psi_mle <- x$estimand$psi_mle

  cat("# Calibrated Model (likelyr)\n\n")

  if (!is.null(param_mle)) {
    if (is.matrix(param_mle)) {
      cat("- Full Model Parameter MLE:\n")
      mat <- param_mle
      if (is.null(rownames(mat))) {
        rownames(mat) <- seq_len(nrow(mat))
      }
      if (is.null(colnames(mat))) {
        colnames(mat) <- seq_len(ncol(mat))
      }
      pretty <- capture.output(print(format(mat), quote = FALSE))
      cat(paste0("    ", pretty), sep = "\n")
      cat("\n")
    } else {
      cat(
        "- Full Model Parameter MLE:   (",
        paste(format(param_mle), collapse = ", "),
        ")\n",
        sep = ""
      )
    }
  } else {
    cat("- Full Model Parameter MLE:   <not available>\n")
  }

  cat("- Parameter of Interest MLE: ", format(psi_mle), "\n", sep = "")
  cat("- integrated:   ", if (is_integrated(x)) "✓" else "×", "\n", sep = "")
  cat("- profiled:     ", if (is_profiled(x)) "✓" else "×", "\n", sep = "")

  invisible(x)
}

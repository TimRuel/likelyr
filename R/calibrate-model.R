# ======================================================================
# Calibration Dispatcher
# ======================================================================

#' Calibrate a 'model_spec' object to data
#'
#' @description
#' Prepares a model for computing profile or integrated likelihoods.
#' Calibration is modular: each structural component (parameter,
#' likelihood, estimand, sampler, traversal, execution) is processed
#' by its own calibration helper.
#'
#' @param spec   A `model_spec` object.
#' @param data    User data.
#' @param verbose Logical; print calibration diagnostics.
#'
#' @return A `model` object that has been calibrated to data.
#' @export
calibrate <- function(spec, data, verbose = FALSE) {
  UseMethod("calibrate")
}

#' @export
calibrate.default <- function(spec, data, verbose = FALSE) {
  stop("calibrate() requires a 'model_spec' object.", call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
calibrate.model_spec <- function(spec, data, verbose = FALSE) {
  # -------------------------------------------------------------------
  # 1. Validate structural model specification before calibration
  # -------------------------------------------------------------------
  validate_calibrate_input(spec)

  spec$data <- data

  # -------------------------------------------------------------------
  # 2. Modular calibration of structural components
  #
  # Order matters:
  #   parameter  — computes param_mle; needed by all downstream steps
  #   likelihood — binds data into loglik / E_loglik closures
  #   estimand   — binds data into psi_fn; computes psi_mle
  #   sampler    — builds omega-hat closures from calibrated quantities
  #   traversal  — builds search interval and branch mode locator
  # -------------------------------------------------------------------
  spec$parameter <- calibrate_parameter(
    parameter = spec$parameter,
    data = data
  )

  spec$likelihood <- calibrate_likelihood(
    likelihood = spec$likelihood,
    data = data
  )

  spec$estimand <- calibrate_estimand(
    estimand = spec$estimand,
    data = data,
    param_mle = spec$parameter$param_mle,
    param_0 = spec$parameter$param_0
  )

  spec$sampler <- calibrate_sampler(
    sampler = spec$sampler,
    parameter = spec$parameter,
    estimand = spec$estimand,
    solver = spec$solver,
    data = data
  )

  spec$traversal <- calibrate_traversal(
    traversal = spec$traversal,
    parameter = spec$parameter,
    likelihood = spec$likelihood,
    estimand = spec$estimand,
    solver = spec$solver,
    data = data
  )

  # -------------------------------------------------------------------
  # 3. Optional execution calibration
  # -------------------------------------------------------------------
  if (!is.null(spec$execution)) {
    spec$execution <- calibrate_execution(spec$execution, spec$sampler)
  }

  # -------------------------------------------------------------------
  # 4. Wrap into calibrated model object
  # -------------------------------------------------------------------
  model <- new_model(spec)

  # -------------------------------------------------------------------
  # 5. Initialize results workspace
  # -------------------------------------------------------------------
  model$workspace <- new_workspace()

  # -------------------------------------------------------------------
  # 6. Optional console output
  # -------------------------------------------------------------------
  if (verbose) {
    print(model)
  }

  model
}

# ======================================================================
# INTERNAL VALIDATION
# ======================================================================

#' Validate structural components prior to calibration
#'
#' @keywords internal
#' @noRd
validate_calibrate_input <- function(spec) {
  if (!inherits(spec$parameter, "parameter_spec")) {
    stop(
      "spec$parameter must be a 'parameter_spec' object before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(spec$likelihood, "likelihood_spec")) {
    stop(
      "spec$likelihood must be a 'likelihood_spec' object before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(spec$estimand, "estimand_spec")) {
    stop(
      "spec$estimand must be an 'estimand_spec' object before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(spec$sampler, "sampler_spec")) {
    stop(
      "spec$sampler must be a 'sampler_spec' object before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(spec$traversal, "traversal_spec")) {
    stop(
      "spec$traversal must be a 'traversal_spec' object before calibration.",
      call. = FALSE
    )
  }

  if (!inherits(spec$solver, "solver_spec")) {
    stop(
      "spec$solver must be a 'solver_spec' object before calibration.",
      call. = FALSE
    )
  }

  invisible(spec)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.model <- function(model, ...) {
  param_mle <- model$parameter$param_mle
  param_0 <- model$parameter$param_0
  psi_mle <- model$estimand$psi_mle
  psi_0 <- model$estimand$psi_0

  .fmt_scalar <- function(x) format(signif(x, 3), trim = TRUE)
  .fmt_vec <- function(x) {
    paste(format(signif(x, 3), trim = TRUE), collapse = ", ")
  }
  .fmt_mat <- function(x) {
    m <- signif(x, 3)
    if (is.null(rownames(m))) {
      rownames(m) <- seq_len(nrow(m))
    }
    if (is.null(colnames(m))) {
      colnames(m) <- seq_len(ncol(m))
    }
    capture.output(print(format(m), quote = FALSE))
  }

  cat("# Calibrated Model (likelyr)\n\n")

  # --- Full model parameter (true) ---
  if (!is.null(param_0)) {
    if (is.matrix(param_0)) {
      cat("- \u03b8\u2080:\n")
      cat(paste0("    ", .fmt_mat(param_0)), sep = "\n")
      cat("\n")
    } else {
      cat("- \u03b8\u2080: (", .fmt_vec(param_0), ")\n", sep = "")
    }
  } else {
    cat("- \u03b8\u2080:  <not available>\n")
  }

  # --- Full model parameter MLE ---
  if (!is.null(param_mle)) {
    if (is.matrix(param_mle)) {
      cat("- \u03b8\u0302:\n")
      cat(paste0("    ", .fmt_mat(param_mle)), sep = "\n")
      cat("\n")
    } else {
      cat("- \u03b8\u0302:  (", .fmt_vec(param_mle), ")\n", sep = "")
    }
  } else {
    cat("- \u03b8\u0302:  <not available>\n")
  }

  # --- Parameter of interest ---
  if (!is.null(psi_0)) {
    cat("- \u03c8\u2080: ", .fmt_scalar(psi_0), "\n", sep = "")
  } else {
    cat("- \u03c8\u2080: <not available>\n")
  }

  if (!is.null(psi_mle)) {
    cat("- \u03c8\u0302:  ", .fmt_scalar(psi_mle), "\n", sep = "")
  } else {
    cat("- \u03c8\u0302:  <not available>\n")
  }

  cat("\n")

  # --- Pipeline state ---
  integrated_status <- if (is_integrated(model)) {
    "\u2713"
  } else if (is_preprocessed(model)) {
    "~ preprocessed"
  } else {
    "\u00d7"
  }

  cat(
    "- profiled:   ",
    if (is_profiled(model)) "\u2713" else "\u00d7",
    "\n",
    sep = ""
  )
  cat("- integrated: ", integrated_status, "\n", sep = "")
  cat(
    "- compared:   ",
    if (is_compared(model)) "\u2713" else "\u00d7",
    "\n",
    sep = ""
  )

  invisible(model)
}

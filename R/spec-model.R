# ======================================================================
# spec-model.R — Model Specification Logic
# ======================================================================

# Depends on constructor functions from class-system.R:
#   new_model_spec(), new_likelihood_spec(), new_estimand_spec(),
#   new_parameter_spec(), new_nuisance_spec(),
#   new_optimizer_spec(), new_execution_spec()

# ======================================================================
# INTERNAL: Calibration Lock Helpers
# ======================================================================

# Check if model_spec has been calibrated
.model_is_locked <- function(model) {
  isTRUE(model$.__calibrated__)
}

# Structural specs cannot be modified post-calibration
.slot_is_structural <- function(slot) {
  slot %in% c("parameter", "likelihood", "estimand", "nuisance")
}

# ======================================================================
# MODEL SPECIFICATION CONSTRUCTOR (high-level wrapper)
# ======================================================================

#' Create a Model Specification
#'
#' @description
#' A likelyr model specification declares the structural and computational
#' components needed for likelihood-based inference:
#'
#' **Structural components (must be present before calibration):**
#'   • parameter_spec()
#'   • likelihood_spec()
#'   • estimand_spec()
#'   • nuisance_spec()
#'
#' **Computational components (required for integrate()/profile()):**
#'   • optimizer_spec()
#'   • execution_spec()
#'
#' Structural components cannot be modified *after* calibration.
#'
#' @export
model_spec <- function(parameter  = NULL,
                       likelihood = NULL,
                       estimand   = NULL,
                       nuisance   = NULL,
                       optimizer  = NULL,
                       execution  = NULL,
                       name       = NULL,
                       ...) {

  x <- list(
    name       = name %||% "<model>",
    parameter  = parameter,
    likelihood = likelihood,
    estimand   = estimand,
    nuisance   = nuisance,
    optimizer  = optimizer,
    execution  = execution,
    extra      = list(...)
  )

  x <- new_model_spec(x)
  x$.__calibrated__ <- FALSE

  .validate_structural_specs(x)

  x
}

# ======================================================================
# INCREMENTAL SPEC BUILDER (add)
# ======================================================================

#' @export
add <- function(model, spec, ...) {
  UseMethod("add")
}

#' @export
add.model_spec <- function(model, spec, ...) {

  slot <- .identify_model_slot(spec)

  # Structural specs cannot change after calibration
  if (.model_is_locked(model) && .slot_is_structural(slot)) {
    stop(sprintf("Cannot modify structural slot '%s' after calibration.", slot),
         call. = FALSE)
  }

  # Overwrite allowed
  model[[slot]] <- spec

  # Validate structural parts only
  .validate_structural_specs(model)

  model
}

#' @export
add.default <- function(model, spec, ...) {
  stop("add() must be applied to a model_spec object.", call. = FALSE)
}

# ======================================================================
# INTERNAL: Identify Component Slot by Class
# ======================================================================

.identify_model_slot <- function(x) {

  if (inherits(x, "parameter_spec"))  return("parameter")
  if (inherits(x, "likelihood_spec")) return("likelihood")
  if (inherits(x, "estimand_spec"))   return("estimand")
  if (inherits(x, "nuisance_spec"))   return("nuisance")
  if (inherits(x, "optimizer_spec"))  return("optimizer")
  if (inherits(x, "execution_spec"))  return("execution")

  stop("Unrecognized specification type passed to add().", call. = FALSE)
}

# ======================================================================
# INTERNAL: Validation of STRUCTURAL SPECS ONLY
# ======================================================================

.validate_structural_specs <- function(x) {

  if (!is.null(x$parameter) &&
      !inherits(x$parameter, "parameter_spec"))
    stop("parameter must be a parameter_spec().", call. = FALSE)

  if (!is.null(x$likelihood) &&
      !inherits(x$likelihood, "likelihood_spec"))
    stop("likelihood must be a likelihood_spec().", call. = FALSE)

  if (!is.null(x$estimand) &&
      !inherits(x$estimand, "estimand_spec"))
    stop("estimand must be an estimand_spec().", call. = FALSE)

  if (!is.null(x$nuisance) &&
      !inherits(x$nuisance, "nuisance_spec"))
    stop("nuisance must be a nuisance_spec().", call. = FALSE)

  invisible(x)
}

# ======================================================================
# INTERNAL: Complete Check for integrate() / profile()
# ======================================================================

.is_model_spec_complete <- function(model) {

  required <- c("parameter", "likelihood", "estimand", "nuisance",
                "optimizer", "execution")

  all(vapply(required, function(s) !is.null(model[[s]]), logical(1)))
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.model_spec <- function(x, ...) {
  cat("<likelyr model_spec>\n")
  if (!is.null(x$name))
    cat("Model:          ", x$name, "\n", sep = "")
  cat("Full Parameter: ", if (!is.null(x$parameter))  x$parameter$name  else "(missing)", "\n", sep = "")
  cat("Likelihood:     ", if (!is.null(x$likelihood)) x$likelihood$name else "(missing)", "\n", sep = "")
  cat("Estimand:       ", if (!is.null(x$estimand))   x$estimand$name   else "(missing)", "\n", sep = "")
  cat("Nuisance:       ", if (!is.null(x$nuisance))   x$nuisance$name   else "(missing)", "\n", sep = "")
  cat("Optimizer:      ", if (!is.null(x$optimizer))  x$optimizer$name  else "(missing)", "\n", sep = "")
  cat("Execution:      ", if (!is.null(x$execution))  x$execution$name  else "(missing)", "\n", sep = "")

  invisible(x)
}

# ======================================================================
# END spec-model.R
# ======================================================================

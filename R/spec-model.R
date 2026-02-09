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

#' @keywords internal
.slot_is_structural <- function(slot) {
  slot %in% c("parameter", "likelihood", "estimand", "nuisance")
}

#' @keywords internal
.slot_is_spec <- function(slot) {
  slot %in%
    c(
      "parameter",
      "likelihood",
      "estimand",
      "nuisance",
      "optimizer",
      "execution"
    )
}

# ======================================================================
# MODEL SPECIFICATION CONSTRUCTOR (high-level wrapper)
# ======================================================================

#' Create a Model Specification
#'
#' @description
#' A model specification declares the components needed for
#' likelihood-based inference.
#'
#' All components are required *prior to calibration*:
#'   • parameter_spec()
#'   • likelihood_spec()
#'   • estimand_spec()
#'   • nuisance_spec()
#'   • optimizer_spec()
#'   • execution_spec()
#'
#' However, \code{model_spec()} is intentionally permissive at construction:
#' you may initialize with \code{NULL} components and add them incrementally
#' via [add()].
#'
#' Once calibrated, **no specification component may be modified**.
#'
#' @export
model_spec <- function(
  parameter = NULL,
  likelihood = NULL,
  estimand = NULL,
  nuisance = NULL,
  optimizer = NULL,
  execution = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<model>",
    parameter = parameter,
    likelihood = likelihood,
    estimand = estimand,
    nuisance = nuisance,
    optimizer = optimizer,
    execution = execution,
    extra = list(...)
  ) |>
    new_model_spec()

  # Validate any provided components (NULL allowed)
  .validate_model_specs(x)

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

  # No specs can change after calibration
  if (is_calibrated(model) && .slot_is_spec(slot)) {
    stop(
      sprintf("Cannot modify slot '%s' after calibration.", slot),
      call. = FALSE
    )
  }

  # Overwrite allowed pre-calibration
  model[[slot]] <- spec

  # Validate any provided components (NULL allowed)
  .validate_model_specs(model)

  model
}

#' @export
add.default <- function(model, spec, ...) {
  stop("add() must be applied to a model_spec object.", call. = FALSE)
}

# ======================================================================
# INTERNAL: Identify Component Slot by Class
# ======================================================================

#' @keywords internal
.identify_model_slot <- function(x) {
  if (inherits(x, "parameter_spec")) {
    return("parameter")
  }
  if (inherits(x, "likelihood_spec")) {
    return("likelihood")
  }
  if (inherits(x, "estimand_spec")) {
    return("estimand")
  }
  if (inherits(x, "nuisance_spec")) {
    return("nuisance")
  }
  if (inherits(x, "optimizer_spec")) {
    return("optimizer")
  }
  if (inherits(x, "execution_spec")) {
    return("execution")
  }

  stop("Unrecognized specification type passed to add().", call. = FALSE)
}

# ======================================================================
# INTERNAL: Validation (validate only what is supplied)
# ======================================================================

#' Validate model specification components
#'
#' @description
#' Internal validator that checks whether any components attached to a
#' model object (if present) inherit from the correct specification classes.
#'
#' This function is intentionally permissive: each component is only
#' validated *if it exists*. Missing components are allowed at construction
#' time and are enforced prior to calibration by calibration-stage validators.
#'
#' @param x A list representing a model specification object.
#'
#' @return Invisibly returns \code{x} if validation succeeds.
#'
#' @keywords internal
#' @noRd
.validate_model_specs <- function(x) {
  if (
    !is.null(x$parameter) &&
      !inherits(x$parameter, "parameter_spec")
  ) {
    stop("parameter must be a parameter_spec().", call. = FALSE)
  }

  if (
    !is.null(x$likelihood) &&
      !inherits(x$likelihood, "likelihood_spec")
  ) {
    stop("likelihood must be a likelihood_spec().", call. = FALSE)
  }

  if (
    !is.null(x$estimand) &&
      !inherits(x$estimand, "estimand_spec")
  ) {
    stop("estimand must be an estimand_spec().", call. = FALSE)
  }

  if (
    !is.null(x$nuisance) &&
      !inherits(x$nuisance, "nuisance_spec")
  ) {
    stop("nuisance must be a nuisance_spec().", call. = FALSE)
  }

  if (
    !is.null(x$optimizer) &&
      !inherits(x$optimizer, "optimizer_spec")
  ) {
    stop("optimizer must be an optimizer_spec().", call. = FALSE)
  }

  if (
    !is.null(x$execution) &&
      !inherits(x$execution, "execution_spec")
  ) {
    stop("execution must be an execution_spec().", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# INTERNAL: Complete Check for calibration / integrate() / profile()
# ======================================================================

#' @keywords internal
.is_model_spec_complete <- function(model) {
  required <- c(
    "parameter",
    "likelihood",
    "estimand",
    "nuisance",
    "optimizer",
    "execution"
  )

  all(vapply(required, function(s) !is.null(model[[s]]), logical(1)))
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.model_spec <- function(x, ...) {
  cat("<model_spec>\n")
  if (!is.null(x$name)) {
    cat("Model:          ", x$name, "\n", sep = "")
  }
  cat(
    "Full Parameter: ",
    if (!is.null(x$parameter)) x$parameter$name else "(missing)",
    "\n",
    sep = ""
  )
  cat(
    "Likelihood:     ",
    if (!is.null(x$likelihood)) x$likelihood$name else "(missing)",
    "\n",
    sep = ""
  )
  cat(
    "Estimand:       ",
    if (!is.null(x$estimand)) x$estimand$name else "(missing)",
    "\n",
    sep = ""
  )
  cat(
    "Nuisance:       ",
    if (!is.null(x$nuisance)) x$nuisance$name else "(missing)",
    "\n",
    sep = ""
  )
  cat(
    "Optimizer:      ",
    if (!is.null(x$optimizer)) x$optimizer$name else "(missing)",
    "\n",
    sep = ""
  )
  cat(
    "Execution:      ",
    if (!is.null(x$execution)) x$execution$name else "(missing)",
    "\n",
    sep = ""
  )

  invisible(x)
}

# ======================================================================
# END spec-model.R
# ======================================================================

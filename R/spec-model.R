# ======================================================================
# Model Specification API (constructor + incremental add() builder)
# ======================================================================

#' Create a Model Specification
#'
#' @description
#' A `model_spec` is a declarative container describing the 5 components
#' required to compute profile or integrated likelihoods:
#'
#'   • likelihood_spec()
#'   • estimand_spec()
#'   • nuisance_spec()   (optional for PL)
#'   • optimizer_spec()
#'   • execution_spec()
#'
#' No computation occurs here. Calibration and likelihood generation are
#' handled by [calibrate()], [integrate()], and [profile()].
#'
#' You may construct a model all at once:
#'
#'   model_spec(likelihood, estimand, nuisance, optimizer, execution)
#'
#' or incrementally using pipes:
#'
#'   model_spec() |>
#'     add(likelihood_spec(...)) |>
#'     add(estimand_spec(...))   |>
#'     add(nuisance_spec(...))   |>
#'     add(optimizer_spec(...))  |>
#'     add(execution_spec(...))
#'
#' Validation occurs only when all required slots are filled.
#'
#' @param likelihood  Optional likelihood_spec().
#' @param estimand    Optional estimand_spec().
#' @param nuisance    Optional nuisance_spec().
#' @param optimizer   Optional optimizer_spec().
#' @param execution   Optional execution_spec().
#' @param name        Optional name for the model.
#' @param ...         Additional fields stored in `$extra`.
#'
#' @return A `model_spec` object.
#' @export
model_spec <- function(likelihood = NULL,
                       estimand   = NULL,
                       nuisance   = NULL,
                       optimizer  = NULL,
                       execution  = NULL,
                       name       = NULL,
                       ...) {

  x <- list(
    name       = name %||% "<model>",
    likelihood = likelihood,
    estimand   = estimand,
    nuisance   = nuisance,
    optimizer  = optimizer,
    execution  = execution,
    extra      = list(...)
  )

  class(x) <- "model_spec"

  # Validate only if complete
  if (.is_model_spec_complete(x)) {
    .validate_model_spec(x)
  }

  x
}

# ======================================================================
# Incremental DSL: add()
# ======================================================================

#' Add a Specification Component to a Model
#'
#' @description
#' Adds a single component (`*_spec`) to a `model_spec()` container.
#'
#' Supports:
#'   model_spec() |> add(likelihood_spec(...)) |> add(estimand_spec(...))
#'
#' Execution specs may be overwritten for user convenience; all other
#' components error if reassigned.
#'
#' @param model A `model_spec` object.
#' @param spec  A specification object (likelihood, estimand, nuisance,
#'              optimizer, or execution).
#' @param ...   Unused.
#'
#' @return Updated `model_spec` object.
#' @export
add <- function(model, spec, ...) {
  UseMethod("add")
}

#' @export
add.model_spec <- function(model, spec, ...) {

  slot <- .identify_model_slot(spec)

  # Execution is allowed to be replaced; others are not
  if (slot != "execution" && !is.null(model[[slot]])) {
    stop(sprintf(
      "Model component '%s' has already been assigned.",
      slot
    ), call. = FALSE)
  }

  # Notify user when execution is overwritten
  if (slot == "execution" && !is.null(model$execution)) {
    message(sprintf(
      "Replacing existing execution spec '%s' with '%s'.",
      model$execution$name, spec$name
    ))
  }

  model[[slot]] <- spec

  # Validate if all required components are now filled
  if (.is_model_spec_complete(model)) {
    .validate_model_spec(model)
  }

  model
}

#' @export
add.default <- function(model, spec, ...) {
  stop("add() must be applied to a model_spec object.", call. = FALSE)
}

# ======================================================================
# INTERNAL: Identify Component Slot
# ======================================================================

#' @keywords internal
#' @noRd
.identify_model_slot <- function(x) {

  if (inherits(x, "likelihood_spec")) return("likelihood")
  if (inherits(x, "estimand_spec"))   return("estimand")
  if (inherits(x, "nuisance_spec"))   return("nuisance")
  if (inherits(x, "optimizer_spec"))  return("optimizer")
  if (inherits(x, "execution_spec"))  return("execution")

  stop("Unrecognized specification type passed to add().", call. = FALSE)
}

# ======================================================================
# INTERNAL: Completeness Check
# ======================================================================

#' @keywords internal
#' @noRd
.is_model_spec_complete <- function(model) {
  # nuisance is optional (for profile likelihoods)
  required <- c("likelihood", "estimand", "optimizer", "execution")
  all(vapply(required, function(s) !is.null(model[[s]]), logical(1)))
}

# ======================================================================
# INTERNAL: Full Validation
# ======================================================================

#' @keywords internal
#' @noRd
.validate_model_spec <- function(x) {

  if (!inherits(x$likelihood, "likelihood_spec"))
    stop("model_spec requires a likelihood_spec().", call. = FALSE)

  if (!inherits(x$estimand, "estimand_spec"))
    stop("model_spec requires an estimand_spec().", call. = FALSE)

  if (!inherits(x$optimizer, "optimizer_spec"))
    stop("model_spec requires an optimizer_spec().", call. = FALSE)

  if (!inherits(x$execution, "execution_spec"))
    stop("model_spec requires an execution_spec (serial_spec or parallel_spec).",
         call. = FALSE)

  if (!is.null(x$nuisance) &&
      !inherits(x$nuisance, "nuisance_spec"))
    stop("nuisance must be NULL or a nuisance_spec().", call. = FALSE)

  invisible(x)
}

# ======================================================================
# Print Method
# ======================================================================

#' @export
print.model_spec <- function(x, ...) {
  cat("# Model Specification\n")
  if (!is.null(x$name)) cat("Model:      ", x$name, "\n", sep="")
  cat("Likelihood: ", if (!is.null(x$likelihood)) x$likelihood$name else "(missing)", "\n", sep="")
  cat("Estimand:   ", if (!is.null(x$estimand))  x$estimand$name  else "(missing)", "\n", sep="")
  cat("Nuisance:   ", if (!is.null(x$nuisance))  x$nuisance$name  else "(missing)", "\n", sep="")
  cat("Optimizer:  ", if (!is.null(x$optimizer)) x$optimizer$name else "(missing)", "\n", sep="")
  cat("Execution:  ", if (!is.null(x$execution)) x$execution$name else "(missing)", "\n", sep="")
  invisible(x)
}

# ======================================================================
# END
# ======================================================================

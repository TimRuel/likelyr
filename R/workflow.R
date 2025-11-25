# ======================================================================
# Likelihood Workflow API (constructor + incremental add() builder)
# ======================================================================

#' Create a Likelihood Workflow
#'
#' @description
#' A workflow is a declarative container describing all components needed
#' for generating profile or integrated likelihoods. No computation occurs
#' here; calibration and generation are handled by [calibrate()] and
#' [generate()].
#'
#' You may construct a workflow all at once:
#'
#'   workflow(model, estimand, nuisance, optimizer, execution)
#'
#' or incrementally using a pipe:
#'
#'   workflow() |> add(model_spec(...)) |> add(estimand_spec(...)) |> ...
#'
#' Components are validated once all required slots are filled.
#'
#' @param model     Optional object from [model_spec()]
#' @param estimand  Optional object from [estimand_spec()]
#' @param nuisance  Optional object from [nuisance_spec()]
#' @param optimizer Optional object from [optimizer_spec()]
#' @param execution Optional object from [serial_spec()] or [parallel_spec()]
#'
#' @return A `likelihood_workflow` object.
#' @export
workflow <- function(model = NULL,
                     estimand = NULL,
                     nuisance = NULL,
                     optimizer = NULL,
                     execution = NULL) {

  x <- list(
    model     = model,
    estimand  = estimand,
    nuisance  = nuisance,
    optimizer = optimizer,
    execution = execution
  )

  class(x) <- "likelihood_workflow"

  # Validate only if complete
  if (.is_workflow_complete(x)) {
    .validate_workflow(x)
  }

  x
}

# ======================================================================
# Incremental DSL: add()
# ======================================================================

#' Add a Specification Component to a Workflow
#'
#' @description
#' Adds a single specification object to a workflow. Supports both:
#'
#' - positional usage: `add(wf, spec)`
#' - named usage inside pipes: `workflow() |> add(model = spec)`
#'
#' All components must be created using *_spec() constructors.
#'
#' @param wf   A `likelihood_workflow`
#' @param spec A spec object (model, estimand, nuisance, optimizer, execution)
#' @param ...  Allows named usage in pipelines
#'
#' @return Updated workflow
#' @export
add <- function(wf, spec, ...) {
  UseMethod("add")
}

#' @export
add.likelihood_workflow <- function(wf, spec) {

  slot <- .identify_spec_slot(spec)

  # "execution" is allowed to be overwritten (user convenience)
  if (slot != "execution" && !is.null(wf[[slot]])) {
    stop(sprintf(
      "Workflow component '%s' has already been assigned.\n",
      slot
    ), call. = FALSE)
  }

  # If execution already exists, notify about replacement
  if (slot == "execution" && !is.null(wf$execution)) {
    message(sprintf(
      "Replacing existing execution spec '%s' with '%s'.",
      wf$execution$name, spec$name
    ))
  }

  wf[[slot]] <- spec

  # If complete, validate
  if (.is_workflow_complete(wf)) {
    .validate_workflow(wf)
  }

  wf
}

# Fallback
#' @export
add.default <- function(wf, spec, ...) {
  stop("add() must be applied to a likelihood_workflow.", call. = FALSE)
}

# ======================================================================
# INTERNAL: Identify which slot a spec belongs to
# ======================================================================

#' @keywords internal
#' @noRd
.identify_spec_slot <- function(x) {

  if (inherits(x, "likelihood_model"))                return("model")
  if (inherits(x, "likelihood_estimand"))             return("estimand")
  if (inherits(x, "likelihood_nuisance"))             return("nuisance")
  if (inherits(x, "likelihood_optimizer"))            return("optimizer")
  if (inherits(x, "likelihood_execution_serial"))     return("execution")
  if (inherits(x, "likelihood_execution_parallel"))   return("execution")

  stop("Unrecognized specification type passed to add().", call. = FALSE)
}

# ======================================================================
# INTERNAL: Workflow completeness check
# ======================================================================

#' @keywords internal
#' @noRd
.is_workflow_complete <- function(wf) {
  all(vapply(wf, function(x) !is.null(x), logical(1)))
}

# ======================================================================
# INTERNAL VALIDATION
# ======================================================================

#' @keywords internal
#' @noRd
.validate_workflow <- function(wf) {

  # Model
  if (!inherits(wf$model, "likelihood_model"))
    stop("workflow$model must be a likelihood_model.", call. = FALSE)

  # Estimand
  if (!inherits(wf$estimand, "likelihood_estimand"))
    stop("workflow$estimand must be a likelihood_estimand.", call. = FALSE)

  # Nuisance
  if (!inherits(wf$nuisance, "likelihood_nuisance"))
    stop("workflow$nuisance must be a likelihood_nuisance.", call. = FALSE)

  # Optimizer
  if (!inherits(wf$optimizer, "likelihood_optimizer"))
    stop("workflow$optimizer must be a likelihood_optimizer.", call. = FALSE)

  # Execution
  if (!inherits(wf$execution, "likelihood_execution"))
    stop("workflow$execution must be a likelihood_execution.", call. = FALSE)

  invisible(wf)
}

# ======================================================================
# Print Method
# ======================================================================

#' @export
print.likelihood_workflow <- function(x, ...) {
  cat("# Likelihood Workflow\n")
  cat("Model:      ", if (!is.null(x$model))     x$model$name     else "(missing)", "\n", sep="")
  cat("Estimand:   ", if (!is.null(x$estimand))  x$estimand$name  else "(missing)", "\n", sep="")
  cat("Nuisance:   ", if (!is.null(x$nuisance))  x$nuisance$name  else "(missing)", "\n", sep="")
  cat("Optimizer:  ", if (!is.null(x$optimizer)) x$optimizer$name else "(missing)", "\n", sep="")
  cat("Execution:  ", if (!is.null(x$execution)) x$execution$name else "(missing)", "\n", sep="")
  invisible(x)
}

# ======================================================================
# END
# ======================================================================

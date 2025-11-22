# ======================================================================
# Likelihood Workflow API (constructor + incremental add() builder)
# ======================================================================

#' Create a likelihood workflow
#'
#' @description
#' A workflow stores the configuration required to compute profile or
#' integrated (pseudo-)likelihood function values for a statistical model.
#'
#' You may construct it all at once:
#'
#' \code{workflow(model, estimand, nuisance, optimizer, execution)}
#'
#' or incrementally using a pipe:
#'
#' \code{workflow() |> add(model_spec) |> add(estimand_spec) |> ...}
#'
#' No estimation is performed here — the workflow is declarative.
#'
#' @param model     Optional object created by [model_spec()]
#' @param estimand  Optional object created by [estimand_spec()]
#' @param nuisance  Optional object created by [nuisance_spec()]
#' @param optimizer Optional object created by [optimizer_spec()]
#' @param execution Optional object created by [execution_spec()]
#'
#' @return An S3 object of class `likelihood_workflow`
#' @export
workflow <- function(model = NULL,
                     estimand = NULL,
                     nuisance = NULL,
                     optimizer = NULL,
                     execution = NULL) {

  wf <- list(
    model     = model,
    estimand  = estimand,
    nuisance  = nuisance,
    optimizer = optimizer,
    execution = execution
  )

  class(wf) <- "likelihood_workflow"

  if (is_workflow_complete(wf)) {
    validate_workflow(wf)
  }

  wf
}

# ======================================================================
# Incremental DSL: add()
# ======================================================================

#' Add a specification component to a workflow
#'
#' @description
#' Adds one specification element to an existing workflow. This function
#' supports both positional usage (`add(wf, model_spec)`) and named usage
#' inside pipes (`workflow() |> add(model = model_spec)`).
#'
#' @param wf   A workflow created by [workflow()]
#' @param spec A single object returned by a *_spec() constructor
#' @param ...  Allows named usage: `add(wf, model = x)` or `add(model = x)`
#'
#' @return Updated likelihood workflow
#' @export
add <- function(wf, spec, ...) {
  UseMethod("add")
}

#' @export
add.likelihood_workflow <- function(wf, spec) {
  slot <- identify_spec_type(spec)

  # Overwrite rule only applies to execution slot
  if (slot == "execution" && !is.null(wf$execution)) {
    message(sprintf(
      "Replacing existing execution spec '%s' with '%s'.",
      wf$execution$name, spec$name
    ))
  } else if (!is.null(wf[[slot]])) {
    stop(sprintf(
      "Component '%s' already assigned. Use replace_%s() if needed.",
      slot, slot
    ), call. = FALSE)
  }

  wf[[slot]] <- spec

  if (is_workflow_complete(wf)) {
    validate_workflow(wf)
  }

  wf
}


#' @export
add.default <- function(wf, spec, ...) {
  stop("add() must be called on a likelihood_workflow object.",
       call. = FALSE)
}

# ======================================================================
# Helper: identify which workflow slot a spec belongs to
# ======================================================================

identify_spec_type <- function(x) {
  if (inherits(x, "likelihood_model"))      return("model")
  if (inherits(x, "likelihood_estimand"))   return("estimand")
  if (inherits(x, "likelihood_nuisance"))   return("nuisance")
  if (inherits(x, "likelihood_optimizer"))  return("optimizer")
  if (inherits(x, "likelihood_execution_serial")) return("execution")
  if (inherits(x, "likelihood_execution_parallel")) return("execution")
  stop("Object is not a recognized spec type.", call. = FALSE)
}


# ======================================================================
# Helper: workflow completion check
# ======================================================================

is_workflow_complete <- function(wf) {
  all(vapply(wf, function(x) !is.null(x), logical(1)))
}

# ======================================================================
# Validation: consistency and completeness
# ======================================================================

validate_workflow <- function(wf) {

  # Class checks
  if (!inherits(wf$model, "likelihood_model"))
    stop("workflow$model must be a likelihood_model.", call. = FALSE)

  if (!inherits(wf$estimand, "likelihood_estimand"))
    stop("workflow$estimand must be a likelihood_estimand.", call. = FALSE)

  if (!inherits(wf$nuisance, "likelihood_nuisance"))
    stop("workflow$nuisance must be a likelihood_nuisance.", call. = FALSE)

  if (!inherits(wf$optimizer, "likelihood_optimizer"))
    stop("workflow$optimizer must be a likelihood_optimizer.", call. = FALSE)

  if (!inherits(wf$execution, "likelihood_execution"))
    stop("workflow$execution must be a likelihood_execution.", call. = FALSE)

  invisible(wf)
}

# ======================================================================
# Print method
# ======================================================================

#' @export
print.likelihood_workflow <- function(x, ...) {
  cat("# Likelihood Workflow\n")
  cat("Model:      ", if (!is.null(x$model))     x$model$name     else "(none)", "\n", sep="")
  cat("Estimand:   ", if (!is.null(x$estimand))  x$estimand$name  else "(none)", "\n", sep="")
  cat("Nuisance:   ", if (!is.null(x$nuisance))  x$nuisance$name  else "(none)", "\n", sep="")
  cat("Optimizer:  ", if (!is.null(x$optimizer)) x$optimizer$name else "(none)", "\n", sep="")
  cat("Execution:  ", if (!is.null(x$execution)) x$execution$name else "(none)", "\n", sep="")
  invisible(x)
}

# ======================================================================
# END
# ======================================================================

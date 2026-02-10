# ======================================================================
# Optimizer Specification (v3.4, unified class system)
# ======================================================================

#' Specify Optimization Settings for Likelihood Computation
#'
#' @description
#' Defines numerical behavior used throughout the likelihood workflow,
#' including:
#'
#' \itemize{
#'   \item nuisance parameter optimization via \code{nloptr::auglag()};
#'   \item restart and continuation behavior for constrained solves;
#'   \item tolerance and stability controls for branch construction;
#'   \item method selection for locating continuous branch modes;
#'   \item stopping and evaluation behavior at ψ bounds during branch sweeps;
#'   \item aggregation and filtering policy for integrated likelihood
#'         curves during inference.
#' }
#'
#' A single \code{optimizer_spec} is shared by both **profile** and
#' **integrated** likelihood routines, and also governs numerical
#' aggregation behavior used when calling \code{infer()} on integrated
#' likelihood results.
#'
#' @param localsolver
#'   Character scalar. Local solver used inside \code{nloptr::auglag()}.
#'   Default is \code{"SLSQP"}.
#'
#' @param control
#'   Named list of control parameters passed to \code{nloptr::auglag()}.
#'
#' @param localtol
#'   Numeric scalar specifying the convergence tolerance for the local solver.
#'
#' @param max_retries
#'   Non-negative integer giving the maximum number of restart attempts allowed
#'   when constrained optimization fails to converge.
#'
#' @param branch_mode_locator_method
#'   Character scalar specifying how the **branch mode**
#'   (the maximizer of the ω̂-conditioned log-likelihood) is located.
#'
#' @param stop_at_bounds
#'   Logical scalar. If TRUE (default), branch sweeps stop when a ψ bound
#'   is reached.
#'
#' @param eval_at_bounds
#'   Logical scalar. If TRUE (default), the branch is evaluated once at the
#'   ψ bound before stopping. Requires \code{stop_at_bounds = TRUE}.
#'
#' @param branch_agg_args
#'   Optional named list of arguments passed to
#'   \code{aggregate_branches()} during inference for integrated
#'   likelihood results. Controls branch filtering and ψ-wise aggregation
#'   without requiring recomputation of branches.
#'
#' @param name
#'   Optional descriptive name for the optimizer specification.
#'
#' @param ...
#'   Additional user metadata stored but not interpreted by the optimizer.
#'
#' @return
#' An object of class \code{c("optimizer_spec", "likelyr")}.
#'
#' @export
optimizer_spec <- function(
  localsolver = "SLSQP",
  control = list(),
  localtol = 1e-6,
  max_retries = 10,
  branch_mode_locator_method = c(
    "hybrid",
    "grid_scan",
    "brent",
    "multiplier_root"
  ),
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE,
  branch_agg_args = NULL,
  name = NULL,
  ...
) {
  branch_mode_locator_method <- match.arg(branch_mode_locator_method)

  x <- list(
    name = name %||% "<optimizer>",
    localsolver = localsolver,
    control = control,
    localtol = localtol,
    max_retries = max_retries,
    branch_mode_locator_method = branch_mode_locator_method,
    stop_at_bounds = stop_at_bounds,
    eval_at_bounds = eval_at_bounds,
    branch_agg_args = branch_agg_args,
    extra = list(...)
  )

  x <- new_optimizer_spec(x)
  .validate_optimizer_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_optimizer_spec <- function(x) {
  # (existing checks unchanged)

  if (!is.null(x$branch_agg_args) && !is.list(x$branch_agg_args)) {
    stop(
      "branch_agg_args must be a named list of arguments passed to aggregate_branches().",
      call. = FALSE
    )
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.optimizer_spec <- function(x, ...) {
  cat("# Optimizer Specification\n")
  cat("- Name:           ", x$name, "\n", sep = "")
  cat("- Local solver:   ", x$localsolver, "\n", sep = "")
  cat("- Local tol:      ", x$localtol, "\n", sep = "")
  cat("- Max retries:    ", x$max_retries, "\n", sep = "")

  cat(
    "- Control list:   ",
    if (length(x$control) == 0) {
      "<empty>"
    } else {
      paste(names(x$control), collapse = ", ")
    },
    "\n",
    sep = ""
  )

  cat(
    "- Branch mode locator method: ",
    x$branch_mode_locator_method,
    "\n",
    sep = ""
  )
  cat("- Stop at ψ bounds:           ", x$stop_at_bounds, "\n", sep = "")
  cat("- Evaluate at bounds:         ", x$eval_at_bounds, "\n", sep = "")

  if (!is.null(x$branch_agg_args)) {
    cat(
      "- Aggregation args:           ",
      paste(names(x$branch_agg_args), collapse = ", "),
      "\n",
      sep = ""
    )
  }

  invisible(x)
}

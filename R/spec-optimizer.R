# ======================================================================
# Optimizer Specification (v3.4, unified class system)
# ======================================================================

#' Specify Optimization Settings for Likelihood Computation
#'
#' @description
#' Defines optimization behavior used throughout the likelihood workflow,
#' including:
#'
#' \itemize{
#'   \item nuisance parameter optimization via \code{nloptr::auglag()};
#'   \item restart and continuation behavior for constrained solves;
#'   \item tolerance and stability controls for branch construction;
#'   \item method selection for locating continuous branch modes;
#'   \item stopping and evaluation behavior at ψ bounds during branch sweeps.
#' }
#'
#' A single \code{optimizer_spec} is shared by both **profile** and
#' **integrated** likelihood routines to ensure consistent numerical behavior.
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
#' @param branch_mode_locator_method
#'   Character scalar specifying how the **branch mode**
#'   (the maximizer of the ω̂-conditioned log-likelihood) is located.
#'
#'   Must be one of:
#'   \itemize{
#'     \item \code{"hybrid"} — grid bracketing followed by local refinement (default)
#'     \item \code{"grid_scan"} — pure grid-based search over ψ
#'     \item \code{"brent"} — direct 1D Brent maximization in ψ
#'     \item \code{"multiplier_root"} — root-finding via constraint multipliers
#'   }
#'
#' @param stop_at_bounds
#'   Logical scalar. If TRUE (default), branch sweeps stop when a ψ bound
#'   is reached.
#'
#' @param eval_at_bounds
#'   Logical scalar. If TRUE (default), the branch is evaluated once at the
#'   ψ bound before stopping. Requires \code{stop_at_bounds = TRUE}.
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
  branch_mode_locator_method = c(
    "hybrid",
    "grid_scan",
    "brent",
    "multiplier_root"
  ),
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE,
  name = NULL,
  ...
) {
  # -------------------------------------------------------------------
  # Normalize branch mode locator method
  # -------------------------------------------------------------------
  branch_mode_locator_method <- match.arg(branch_mode_locator_method)

  x <- list(
    name = name %||% "<optimizer>",
    localsolver = localsolver,
    control = control,
    localtol = localtol,
    branch_mode_locator_method = branch_mode_locator_method,
    stop_at_bounds = stop_at_bounds,
    eval_at_bounds = eval_at_bounds,
    extra = list(...)
  )

  x <- new_optimizer_spec(x)
  .validate_optimizer_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' Validate optimizer specification
#'
#' @keywords internal
#' @noRd
.validate_optimizer_spec <- function(x) {
  # Local solver ---------------------------------------------------------
  if (
    !is.character(x$localsolver) ||
      length(x$localsolver) != 1 ||
      !nzchar(x$localsolver)
  ) {
    stop("localsolver must be a non-empty character scalar.", call. = FALSE)
  }

  # Control list ---------------------------------------------------------
  if (!is.list(x$control)) {
    stop(
      "control must be a named list of nloptr / auglag options.",
      call. = FALSE
    )
  }

  # Local tolerance ------------------------------------------------------
  if (
    !is.numeric(x$localtol) ||
      length(x$localtol) != 1 ||
      !is.finite(x$localtol) ||
      x$localtol <= 0
  ) {
    stop("localtol must be a positive numeric scalar.", call. = FALSE)
  }

  # ψ-bound behavior -----------------------------------------------------
  if (!is.logical(x$stop_at_bounds) || length(x$stop_at_bounds) != 1L) {
    stop("stop_at_bounds must be a single logical value.", call. = FALSE)
  }

  if (!is.logical(x$eval_at_bounds) || length(x$eval_at_bounds) != 1L) {
    stop("eval_at_bounds must be a single logical value.", call. = FALSE)
  }

  if (!x$stop_at_bounds && x$eval_at_bounds) {
    stop(
      "eval_at_bounds = TRUE requires stop_at_bounds = TRUE.",
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

  cat("- Stop at ψ bounds:     ", x$stop_at_bounds, "\n", sep = "")
  cat("- Evaluate at bounds:   ", x$eval_at_bounds, "\n", sep = "")

  invisible(x)
}

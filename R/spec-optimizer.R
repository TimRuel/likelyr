# ======================================================================
# Optimizer Specification (v3.2, unified class system)
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
#'   \item optional control parameters for locating continuous branch modes.
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
#' @param max_retries
#'   Non-negative integer giving the maximum number of restart attempts allowed
#'   when constrained optimization fails to converge.
#'
#' @param drop_mult
#'   Numeric scalar greater than 1 controlling how large a log-likelihood drop
#'   is allowed relative to the previous drop during continuation-based sweeps.
#'   Typical values range from 3 to 10.
#'
#' @param branch_mode_params
#'   Optional named list of control parameters passed to
#'   \code{branch_mode_solve()} when locating the **continuous branch mode**
#'   prior to branch construction.
#'
#'   Recognized entries include:
#'   \describe{
#'     \item{max_iter}{Maximum number of Brent iterations used when solving for
#'       the branch mode ψ̂.}
#'     \item{tol}{Convergence tolerance for the ψ optimization.}
#'   }
#'
#'   If \code{NULL} (default), internal defaults are used.
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
  drop_mult = 5,
  branch_mode_params = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<optimizer>",
    localsolver = localsolver,
    control = control,
    localtol = localtol,
    max_retries = max_retries,
    drop_mult = drop_mult,
    branch_mode_params = branch_mode_params,
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
#' @description
#' Internal validator for \code{optimizer_spec} objects. Ensures that all
#' required optimizer configuration fields are present and correctly typed
#' before numerical optimization routines are invoked.
#'
#' @param x A list representing an \code{optimizer_spec} object.
#'
#' @return Invisibly returns \code{x} if validation succeeds.
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

  # Retry count ----------------------------------------------------------
  if (
    !is.numeric(x$max_retries) ||
      length(x$max_retries) != 1 ||
      x$max_retries < 0 ||
      x$max_retries != as.integer(x$max_retries)
  ) {
    stop("max_retries must be a non-negative integer.", call. = FALSE)
  }

  # Drop multiplier ------------------------------------------------------
  if (
    !is.numeric(x$drop_mult) ||
      length(x$drop_mult) != 1 ||
      !is.finite(x$drop_mult) ||
      x$drop_mult <= 1
  ) {
    stop("drop_mult must be a numeric scalar > 1.", call. = FALSE)
  }

  # Branch mode params ---------------------------------------------------
  if (!is.null(x$branch_mode_params)) {
    if (!is.list(x$branch_mode_params)) {
      stop(
        "branch_mode_params must be a named list or NULL.",
        call. = FALSE
      )
    }

    if (!is.null(x$branch_mode_params$max_iter)) {
      mi <- x$branch_mode_params$max_iter
      if (!is.numeric(mi) || length(mi) != 1 || mi <= 0) {
        stop(
          "branch_mode_params$max_iter must be a positive numeric scalar.",
          call. = FALSE
        )
      }
    }

    if (!is.null(x$branch_mode_params$tol)) {
      tol <- x$branch_mode_params$tol
      if (!is.numeric(tol) || length(tol) != 1 || tol <= 0) {
        stop(
          "branch_mode_params$tol must be a positive numeric scalar.",
          call. = FALSE
        )
      }
    }
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
  cat("- Drop mult:      ", x$drop_mult, "\n", sep = "")

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
    "- Branch mode:    ",
    if (is.null(x$branch_mode_params)) {
      "<defaults>"
    } else {
      paste(
        names(x$branch_mode_params),
        unlist(x$branch_mode_params),
        sep = "=",
        collapse = ", "
      )
    },
    "\n",
    sep = ""
  )

  invisible(x)
}

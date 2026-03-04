# ======================================================================
# Solver Specification (v1.0)
#
# Controls the inner auglag solver used during branch evaluation:
#   max_θ E[ℓ(θ; ω̂)]  subject to  ψ(θ) = ψ_target
#
# Intentionally limited to inner solver settings. Branch mode location,
# walk strategy, and scoring live in pipeline_spec.
# ======================================================================

#' Specify Inner Solver Settings
#'
#' @description
#' Defines numerical settings for the constrained inner optimization
#' performed at each ψ value during branch evaluation via
#' \code{nloptr::auglag()}.
#'
#' @param localsolver Character scalar. Local solver used inside
#'   \code{nloptr::auglag()}. Default: \code{"SLSQP"}.
#' @param control Named list of control parameters passed directly to
#'   \code{nloptr::auglag()}. Default: \code{list()}.
#' @param localtol Positive numeric scalar. Convergence tolerance for
#'   the local solver. Default: \code{1e-6}.
#' @param max_retries Non-negative integer. Maximum number of restart
#'   attempts when a constrained solve fails to converge.
#'   Default: \code{10}.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{solver_spec} object.
#' @export
solver_spec <- function(
  localsolver = "SLSQP",
  control = list(),
  localtol = 1e-6,
  max_retries = 10L,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<solver>",
    localsolver = localsolver,
    control = control,
    localtol = localtol,
    max_retries = as.integer(max_retries),
    extra = list(...)
  )

  x <- new_solver_spec(x)
  .validate_solver_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_solver_spec <- function(x) {
  if (!is.character(x$localsolver) || length(x$localsolver) != 1L) {
    stop("localsolver must be a single character string.", call. = FALSE)
  }

  if (!is.list(x$control)) {
    stop("control must be a named list.", call. = FALSE)
  }

  if (!is.numeric(x$localtol) || length(x$localtol) != 1L || x$localtol <= 0) {
    stop("localtol must be a positive numeric scalar.", call. = FALSE)
  }

  if (
    !is.integer(x$max_retries) ||
      length(x$max_retries) != 1L ||
      x$max_retries < 0L
  ) {
    stop("max_retries must be a non-negative integer.", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.solver_spec <- function(x, ...) {
  cat("# Solver Specification\n")
  cat("- Name:          ", x$name, "\n", sep = "")
  cat("- Local solver:  ", x$localsolver, "\n", sep = "")
  cat("- Local tol:     ", x$localtol, "\n", sep = "")
  cat("- Max retries:   ", x$max_retries, "\n", sep = "")
  cat(
    "- Control:       ",
    if (length(x$control) == 0) {
      "<default>"
    } else {
      paste(names(x$control), collapse = ", ")
    },
    "\n",
    sep = ""
  )
  invisible(x)
}

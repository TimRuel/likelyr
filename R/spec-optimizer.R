# ======================================================================
# Optimizer Specification (v3.1, unified class system)
# ======================================================================

#' Specify Optimization Settings for nloptr::auglag
#'
#' @description
#' Defines optimization behavior for estimating θ using
#' `nloptr::auglag()`. This specification controls:
#'
#' • Local solver (e.g., "SLSQP")
#' • nloptr control parameters
#' • Local solver tolerance
#' • Restart policy (max_retries)
#'
#' These settings are used by both profile and integrated likelihood
#' routines when solving constrained optimization problems.
#'
#' @param localsolver
#'   Character scalar. Inner solver used inside `nloptr::auglag`.
#'   Default: `"SLSQP"`.
#'
#' @param control
#'   Named list of nloptr/auglag control options.
#'
#' @param localtol
#'   Numeric scalar specifying local solver tolerance.
#'
#' @param max_retries
#'   Numeric scalar giving number of restart attempts allowed when
#'   optimization fails to converge.
#'
#' @param name
#'   Optional descriptive name.
#'
#' @param ...
#'   Additional stored but unused user metadata.
#'
#' @return
#' An `optimizer_spec` object with classes:
#' `c("optimizer_spec", "likelyr")`.
#'
#' @export
optimizer_spec <- function(
    localsolver = "SLSQP",
    control     = list(),
    localtol    = 1e-6,
    max_retries = 10,
    name        = NULL,
    ...
) {

  x <- list(
    name        = name %||% "<optimizer>",
    localsolver = localsolver,
    control     = control,
    localtol    = localtol,
    max_retries = max_retries,
    extra       = list(...)
  )

  x <- new_optimizer_spec(x)
  .validate_optimizer_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

.validate_optimizer_spec <- function(x) {

  # Local solver ---------------------------------------------------------
  if (!is.character(x$localsolver) ||
      length(x$localsolver) != 1 ||
      !nzchar(x$localsolver)) {
    stop("localsolver must be a non-empty character scalar.", call. = FALSE)
  }

  # Control list ---------------------------------------------------------
  if (!is.list(x$control)) {
    stop("control must be a named list of nloptr / auglag options.",
         call. = FALSE)
  }

  # Local tolerance ------------------------------------------------------
  if (!is.numeric(x$localtol) ||
      length(x$localtol) != 1 ||
      !is.finite(x$localtol) ||
      x$localtol <= 0) {
    stop("localtol must be a positive numeric scalar.", call. = FALSE)
  }

  # Retry count ----------------------------------------------------------
  if (!is.numeric(x$max_retries) ||
      length(x$max_retries) != 1 ||
      x$max_retries < 0 ||
      x$max_retries != as.integer(x$max_retries)) {
    stop("max_retries must be a non-negative integer.", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.optimizer_spec <- function(x, ...) {
  cat("# Optimizer Specification\n")
  cat("- Name:          ", x$name, "\n", sep = "")
  cat("- Local solver:  ", x$localsolver, "\n", sep = "")
  cat("- Tolerance:     ", x$localtol, "\n", sep = "")
  cat("- Max retries:   ", x$max_retries, "\n", sep = "")
  cat("- Control list:  ",
      if (length(x$control) == 0) "<empty>" else paste(names(x$control), collapse = ", "),
      "\n", sep = "")
  invisible(x)
}

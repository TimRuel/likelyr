# ======================================================================
# Optimizer Specification (v3.0, unified class system)
# ======================================================================

#' Specify Optimization Settings for nloptr::auglag
#'
#' @description
#' Defines the optimization policy for theta, specifically targeting
#' `nloptr::auglag()`. Arguments follow workflow order:
#'
#'   solver choice → nloptr options → box constraints →
#'   tolerances → retry policy → name.
#'
#' @param localsolver  Inner solver used inside auglag (default "SLSQP")
#' @param control      List of nloptr::auglag / local options
#' @param localtol     Local solver tolerance
#' @param max_retries  Number of restart attempts
#' @param name         Optional descriptive name
#' @param ...          Extra user-defined fields
#'
#' @return An `optimizer_spec` object with classes:
#'         c("optimizer_spec", "likelyr_spec")
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

  # Unified class constructor ensures: c("optimizer_spec","likelyr_spec")
  x <- new_optimizer_spec(x)

  # Validate & return normalized object
  .validate_optimizer_spec(x)

  x
}

# ----------------------------------------------------------------------
# INTERNAL VALIDATOR
# ----------------------------------------------------------------------

.validate_optimizer_spec <- function(x) {

  # localsolver
  if (!is.character(x$localsolver) || length(x$localsolver) != 1)
    stop("localsolver must be a single string.", call. = FALSE)

  # control options
  if (!is.list(x$control))
    stop("control must be a named list of nloptr options.", call. = FALSE)

  # tolerances
  if (!is.numeric(x$localtol) || length(x$localtol) != 1)
    stop("localtol must be a scalar numeric value.", call. = FALSE)

  # retries
  if (!is.numeric(x$max_retries) || length(x$max_retries) != 1)
    stop("max_retries must be a scalar numeric value.", call. = FALSE)

  invisible(x)
}

# ------------------------------------------------------
# PRINT METHOD
# ------------------------------------------------------

#' @export
print.optimizer_spec <- function(x, ...) {
  cat("# Optimizer Specification\n")
  cat("- Name: ", x$name, "\n", sep = "")
  invisible(x)
}


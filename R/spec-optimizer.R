# ======================================================================
# Optimizer Specification (v2.1, reordered for nloptr::auglag)
# ======================================================================

#' Specify Optimization Settings for nloptr::auglag
#'
#' @description
#' Defines the optimization policy for theta, specifically targeting
#' `nloptr::auglag()`. The arguments are ordered to match the typical
#' workflow: solver choice → control options → constraints →
#' initialization → retry behavior → naming.
#'
#' @param localsolver  Solver method used inside auglag (default: "SLSQP").
#' @param control      List of nloptr options (passed to auglag).
#' @param box          List(lower, upper) box constraints.
#' @param localtol     Tolerance for local solver.
#' @param max_retries  Number of retries for the local solver.
#' @param name         Optional descriptive name.
#' @param ...          Extra fields.
#'
#' @return An `optimizer_spec` object.
#' @export
optimizer_spec <- function(
    localsolver = "SLSQP",
    control     = list(),
    box         = list(lower = NULL, upper = NULL),
    localtol    = 1e-6,
    max_retries = 10,
    name        = NULL,
    ...
) {

  x <- list(
    name        = name %||% "<optimizer>",
    localsolver = localsolver,
    control     = control,
    box         = box,
    localtol    = localtol,
    max_retries = max_retries,
    extra       = list(...)
  )

  class(x) <- "optimizer_spec"
  .validate_optimizer_spec(x)
  x
}

# ----------------------------------------------------------------------

.validate_optimizer_spec <- function(x) {

  # solver type
  if (!is.character(x$localsolver) || length(x$localsolver) != 1)
    stop("localsolver must be a single string.", call. = FALSE)

  # control
  if (!is.list(x$control))
    stop("control must be a list of nloptr options.", call. = FALSE)

  # box constraints
  if (!is.list(x$box) || !all(c("lower","upper") %in% names(x$box)))
    stop("box must be list(lower, upper).", call. = FALSE)

  # tolerance and retries
  if (!is.numeric(x$localtol) || length(x$localtol) != 1)
    stop("localtol must be scalar numeric.", call. = FALSE)

  if (!is.numeric(x$max_retries) || length(x$max_retries) != 1)
    stop("max_retries must be scalar numeric.", call. = FALSE)

  invisible(x)
}

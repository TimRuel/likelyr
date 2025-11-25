# ======================================================================
# Optimizer Specification (for nloptr::auglag)
# ======================================================================

#' Specify Optimizer Settings for Constrained Likelihood Problems
#'
#' @description
#' Defines configuration parameters for `nloptr::auglag()`, which is used
#' internally to solve constrained optimization problems of the form
#' \eqn{\thetâ(\psi, \omegâ)} during branch expansion and during ω̂ sampling.
#'
#' This object governs:
#' * the local solver used by `nloptr`,
#' * tolerance levels,
#' * control list passed to `auglag()`,
#' * retry behavior when monotonicity fails in branch sweeps.
#'
#' @param localsolver
#'   Character scalar. Local optimizer used by `auglag()`.
#'   Typically `"SLSQP"` (default) or `"NLOPT_LD_MMA"`, etc.
#'
#' @param localtol
#'   Numeric scalar. Tolerance for the local solver. Default `1e-6`.
#'
#' @param control
#'   A named list of parameters passed directly to `nloptr::auglag()`.
#'
#' @param max_retries
#'   Integer. Maximum number of attempts to restore monotonic branch
#'   descent by jittering initial guesses during branch sweeps.
#'
#' @param name
#'   Optional human-readable label.
#'
#' @param ...
#'   Additional metadata stored under `$extra`.
#'
#' @return
#' An S3 object of class `"likelihood_optimizer"`.
#'
#' @export
optimizer_spec <- function(
    localsolver = "SLSQP",
    localtol    = 1e-6,
    control     = list(),
    max_retries = 10,
    name        = NULL,
    ...
) {
  # Construct object
  x <- list(
    name        = name %||% "<optimizer>",
    localsolver = localsolver,
    localtol    = localtol,
    control     = control,
    max_retries = max_retries,
    extra       = list(...)
  )

  # Assign class then validate
  class(x) <- "likelihood_optimizer"
  .validate_optimizer_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR (not exported)
# ======================================================================

#' @keywords internal
.validate_optimizer_spec <- function(x) {

  # Local solver
  if (!is.character(x$localsolver) || length(x$localsolver) != 1)
    stop("`localsolver` must be a single character string.",
         call. = FALSE)

  # Local tolerance
  if (!is.numeric(x$localtol) || length(x$localtol) != 1 || x$localtol <= 0)
    stop("`localtol` must be a positive numeric scalar.",
         call. = FALSE)

  # Control list
  if (!is.list(x$control))
    stop("`control` must be a list.", call. = FALSE)

  # Retry count
  if (!is.numeric(x$max_retries) ||
      length(x$max_retries) != 1 ||
      x$max_retries < 0)
    stop("`max_retries` must be a non-negative integer.",
         call. = FALSE)

  invisible(x)
}

# ======================================================================
# Print Method
# ======================================================================

#' @export
print.likelihood_optimizer <- function(x, ...) {
  cat("# Optimizer Specification (nloptr::auglag)\n")
  cat("- Name:         ", x$name, "\n", sep = "")
  cat("- Local solver: ", x$localsolver, "\n", sep = "")
  cat("- Local tol:    ", x$localtol, "\n", sep = "")
  cat("- Control list: ",
      if (length(x$control) == 0) "empty" else paste(names(x$control), collapse = ", "),
      "\n", sep = "")
  cat("- Max retries:  ", x$max_retries, "\n", sep = "")
  invisible(x)
}

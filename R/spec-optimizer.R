# ======================================================================
# Optimizer Specification (for nloptr::auglag)
# ======================================================================

#' Optimizer specification
#'
#' @description
#' Defines optimizer configuration for `nloptr::auglag()` used in solving
#' constrained θ̂(ψ, ω̂) problems inside branch expansion.
#'
#' @param localsolver String. Local solver for auglag. Default `"SLSQP"`.
#' @param localtol   Numeric. Tolerance for local solver. Default `1e-6`.
#' @param control    List of control parameters passed directly to auglag().
#' @param max_retries Number of tries to maximize E_loglik at given value of psi before resorting to fallback
#' @param name       Optional human-readable label.
#' @param ...        Additional metadata stored but not used internally.
#'
#' @return An S3 object of class `likelihood_optimizer`.
#' @export
optimizer_spec <- function(
    localsolver = "SLSQP",
    localtol    = 1e-6,
    control     = list(),
    max_retries = 10,
    name        = NULL,
    ...
) {

  x <- list(
    name        = name %||% "<optimizer>",
    localsolver = localsolver,
    localtol    = localtol,
    control     = control,
    max_retries = max_retries,
    extra       = list(...)
  )

  class(x) <- "likelihood_optimizer"
  x
}

#' @export
print.likelihood_optimizer <- function(x, ...) {
  cat("# Optimizer Specification (nloptr::auglag)\n")
  cat("- Name:         ", x$name, "\n", sep = "")
  cat("- Local solver: ", x$localsolver, "\n", sep = "")
  cat("- Local tol:    ", x$localtol, "\n", sep = "")
  cat("- Control list: ",
      if (length(x$control) == 0) "empty" else names(x$control),
      "\n", sep = "")
  cat("- Max retries:  ", x$max_retries, "\n", sep = "")
  invisible(x)
}

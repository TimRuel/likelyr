# ======================================================================
# Likelihood Specification (v6.0)
# ======================================================================

#' Specify a Parametric Likelihood Model
#'
#' @description
#' Defines the *likelihood component* of the model:
#'
#'   • loglik(param, data)  — the log-likelihood function
#'   • param_mle_fn(data)   — analytic initializer for mle of model parameter
#'
#' All parameter–space structure (dimension, bounds, true value,
#' inequality constraints) must now be supplied via `parameter_spec()`.
#' `likelihood_spec()` is intentionally lightweight.
#'
#' @param loglik       Function(param, data) → log-likelihood.
#' @param param_mle_fn Function(data) → initial mle.
#' @param name         Optional descriptive name.
#' @param ...          Additional stored metadata (unused internally).
#'
#' @return A `likelihood_spec` object.
#' @export
likelihood_spec <- function(loglik, param_mle_fn, name = NULL, ...) {
  x <- list(
    name = name %||% "<likelihood>",
    loglik = loglik,
    param_mle_fn = param_mle_fn,
    extra = list(...)
  )

  x <- new_likelihood_spec(x)
  .validate_likelihood_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' Validate likelihood specification
#'
#' @description
#' Internal validator for \code{likelihood_spec} objects. Ensures that
#' all required components needed for likelihood evaluation and
#' initialization are present and correctly specified.
#'
#' @param x A list representing a \code{likelihood_spec} object.
#'
#' @details
#' The following components are validated:
#'
#' \itemize{
#'   \item \code{loglik}: function with signature
#'         \code{function(param, data)} returning the log-likelihood.
#'   \item \code{param_mle_fn}: analytic initializer function with
#'         signature \code{function(data)} returning an MLE guess.
#' }
#'
#' @return Invisibly returns \code{x} if validation succeeds.
#'
#' @keywords internal
#' @noRd
.validate_likelihood_spec <- function(x) {
  # Log-likelihood
  if (!is.function(x$loglik)) {
    stop("loglik must be a function(param, data).", call. = FALSE)
  }

  # Analytic initializer required
  if (!is.function(x$param_mle_fn)) {
    stop("param_mle_fn must be a function(data).", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.likelihood_spec <- function(x, ...) {
  cat("# Likelihood Specification\n")
  cat("- Name:           ", x$name, "\n", sep = "")
  cat("- loglik():        ✔ function\n")
  cat("- param_mle_fn():  ✔ function\n")
  invisible(x)
}

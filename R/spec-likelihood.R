# ======================================================================
# Likelihood Specification (v6.0)
# ======================================================================

#' Specify a Parametric Likelihood Model
#'
#' @description
#' Defines the *likelihood component* of the model:
#'
#'   • loglik(theta, data)  — the log-likelihood function
#'   • theta_mle_fn(data)   — analytic initializer for θ̂
#'
#' All parameter–space structure (dimension, bounds, true value,
#' inequality constraints) must now be supplied via `parameter_spec()`.
#' `likelihood_spec()` is intentionally lightweight.
#'
#' @param loglik       Function(theta, data) → log-likelihood.
#' @param theta_mle_fn Function(data) → initial θ̂.
#' @param name         Optional descriptive name.
#' @param ...          Additional stored metadata (unused internally).
#'
#' @return A `likelihood_spec` object.
#' @export
likelihood_spec <- function(loglik,
                            theta_mle_fn,
                            name = NULL,
                            ...) {

  x <- list(
    name         = name %||% "<likelihood>",
    loglik       = loglik,
    theta_mle_fn = theta_mle_fn,
    extra        = list(...)
  )

  x <- new_likelihood_spec(x)
  .validate_likelihood_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

.validate_likelihood_spec <- function(x) {

  # Log-likelihood
  if (!is.function(x$loglik))
    stop("loglik must be a function(theta, data).", call. = FALSE)

  # Analytic initializer required
  if (!is.function(x$theta_mle_fn))
    stop("theta_mle_fn must be a function(data).", call. = FALSE)

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
  cat("- theta_mle_fn():  ✔ function\n")
  invisible(x)
}

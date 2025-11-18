#' Model specification for likelihood workflows
#'
#' @description
#' Defines a parametric likelihood model for use in likelihood workflows.
#' Both `loglik` and `E_loglik` must be functions of only the parameter vector
#' `theta`, with any observed data already embedded in their closures. This
#' enables clean interoperable optimization for profiling and integrated
#' likelihood branches.
#'
#' @param loglik Function `function(theta)` returning numeric scalar log-likelihood.
#' @param E_loglik Function `function(theta, omega_hat)` returning expected log-likelihood
#'        under a given element `omega_hat` of the ZSE nuisance parameter manifold .
#' @param param_dim Integer > 0. Dimension of the free model parameter vector `theta`.
#' @param grad Optional gradient function `function(theta, omega_hat)` giving the gradient
#'        of `E_loglik` with respect to `theta`. Used only if the chosen optimizer requires it.
#' @param theta_mle Optional numeric vector of length `param_dim` representing a known MLE
#'        of the model parameter. If not supplied, optimizers may compute it, or error
#'        depending on their configured strictness.
#' @param name Optional descriptive model name.
#' @param ... Additional user-defined named model elements, stored in `extra`.
#'
#' @return An S3 object of class `likelihood_model`.
#' @export
model_spec <- function(loglik,
                       E_loglik,
                       param_dim,
                       grad = NULL,
                       theta_mle = NULL,
                       name = NULL,
                       ...) {

  x <- list(
    name        = name %||% "<model>",
    loglik      = loglik,
    E_loglik    = E_loglik,
    param_dim   = param_dim,
    grad        = grad,
    theta_mle   = theta_mle,
    extra       = list(...)
  )
  class(x) <- "likelihood_model"

  validate_model_spec(x)
  x
}

# --- Internal validation ------------------------------------------------------

validate_model_spec <- function(x) {

  # Function requirements
  if (!is.function(x$loglik))
    stop("`loglik` must be a function(theta).", call. = FALSE)

  if (!is.function(x$E_loglik))
    stop("`E_loglik` must be a function(theta, omega_hat).", call. = FALSE)

  # Parameter dimension validation
  if (!is.numeric(x$param_dim) || length(x$param_dim) != 1L || x$param_dim < 1)
    stop("`param_dim` must be a positive integer.", call. = FALSE)

  # Gradient validation
  if (!is.null(x$grad) && !is.function(x$grad))
    stop("`grad` must be a function(theta, omega_hat)` when supplied.", call. = FALSE)

  # MLE validation (optional but must match parameter dimension)
  if (!is.null(x$theta_mle)) {
    if (!is.numeric(x$theta_mle) || length(x$theta_mle) != x$param_dim) {
      stop("`theta_mle` must be a numeric vector of length `param_dim`.", call. = FALSE)
    }
  }

  invisible(x)
}

# --- Print method -------------------------------------------------------------

#' @export
print.likelihood_model <- function(x, ...) {
  cat("# Model Specification\n")
  cat("- Name:                      ", x$name, "\n", sep = "")
  cat("- Log-Likelihood Provided:   ", isTRUE(!is.null(x$loglik)), "\n", sep = "")
  cat("- Expected Log-Likelihood:   ", isTRUE(!is.null(x$E_loglik)), "\n", sep = "")
  cat("- Parameter Dimension:       ", x$param_dim, "\n", sep = "")
  cat("- Gradient Provided:         ", isTRUE(!is.null(x$grad)), "\n", sep = "")
  cat("- MLE Provided:              ", isTRUE(!is.null(x$theta_mle)), "\n", sep = "")
  invisible(x)
}

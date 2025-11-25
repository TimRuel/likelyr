# ======================================================================
# Model specification for likelihood workflows
# ======================================================================

#' Model specification for likelihood workflows
#'
#' @description
#' Defines a parametric likelihood model for profile or integrated
#' likelihood workflows.
#'
#' A model must specify:
#'   - `loglik(theta, data)`
#'   - `E_loglik(theta, omega_hat, data)`
#'
#' Both functions must accept raw theta; any needed data is embedded
#' via closures during workflow calibration.
#'
#' Optional:
#'   - gradient of expected loglik `E_loglik_grad()`
#'   - custom MLE initializer `theta_mle_fn(data)`
#'   - box constraints (`lower`, `upper`)
#'   - inequality constraints (`ineq`, `ineq_jac`)
#'
#' @param loglik Function(theta, data) → numeric loglik
#' @param E_loglik Function(theta, omega_hat, data) → expected loglik
#' @param param_dim Positive integer giving dimension of theta
#' @param E_loglik_grad Optional gradient(theta, omega_hat, data)
#' @param theta_mle_fn Optional function(data) → theta_mle
#' @param lower Optional numeric vector of length `param_dim`
#' @param upper Optional numeric vector of length `param_dim`
#' @param ineq Optional inequality function(theta) ≤ 0
#' @param ineq_jac Optional jacobian of `ineq`
#' @param name Optional descriptive string
#' @param ... Additional fields stored in `$extra`
#'
#' @return A validated `likelihood_model` object
#' @export
model_spec <- function(loglik,
                       E_loglik,
                       param_dim,
                       E_loglik_grad = NULL,
                       theta_mle_fn  = NULL,
                       lower = NULL,
                       upper = NULL,
                       ineq  = NULL,
                       ineq_jac = NULL,
                       name = NULL,
                       ...)
{
  x <- list(
    name          = name %||% "<model>",
    loglik        = loglik,
    E_loglik      = E_loglik,
    E_loglik_grad = E_loglik_grad,
    param_dim     = param_dim,
    theta_mle_fn  = theta_mle_fn,
    lower         = lower,
    upper         = upper,
    ineq          = ineq,
    ineq_jac      = ineq_jac,
    extra         = list(...)
  )

  class(x) <- "likelihood_model"
  .validate_model_spec(x)
  x
}


# ======================================================================
# INTERNAL VALIDATOR (not exported)
# ======================================================================

#' @keywords internal
.validate_model_spec <- function(x) {

  # -- loglik must be a function
  if (!is.function(x$loglik))
    stop("`loglik` must be a function(theta, data).", call. = FALSE)

  # -- E_loglik must be a function
  if (!is.function(x$E_loglik))
    stop("`E_loglik` must be a function(theta, omega_hat, data).", call. = FALSE)

  # -- Gradient check
  if (!is.null(x$E_loglik_grad) && !is.function(x$E_loglik_grad))
    stop("`E_loglik_grad` must be a function(theta, omega_hat, data).", call. = FALSE)

  # -- param_dim
  if (!is.numeric(x$param_dim) ||
      length(x$param_dim) != 1 ||
      x$param_dim < 1)
    stop("`param_dim` must be a positive integer.", call. = FALSE)

  P <- as.integer(x$param_dim)

  # -- theta_mle_fn (optional)
  if (!is.null(x$theta_mle_fn) && !is.function(x$theta_mle_fn))
    stop("`theta_mle_fn` must be a function(data) → theta.", call. = FALSE)

  # -- box constraints
  if (!is.null(x$lower)) {
    if (!is.numeric(x$lower) || length(x$lower) != P)
      stop("`lower` must be numeric of length `param_dim`.", call. = FALSE)
  }

  if (!is.null(x$upper)) {
    if (!is.numeric(x$upper) || length(x$upper) != P)
      stop("`upper` must be numeric of length `param_dim`.", call. = FALSE)
  }

  if (!is.null(x$lower) && !is.null(x$upper)) {
    if (any(x$lower > x$upper))
      stop("Each lower[i] must be <= upper[i].", call. = FALSE)
  }

  # -- inequality constraints
  if (!is.null(x$ineq) && !is.function(x$ineq))
    stop("`ineq` must be a function(theta) → vector ≤ 0.", call. = FALSE)

  if (!is.null(x$ineq_jac) && !is.function(x$ineq_jac))
    stop("`ineq_jac` must be a function(theta) → jacobian.", call. = FALSE)

  invisible(x)
}


# ======================================================================
# Print Method
# ======================================================================

#' @export
print.likelihood_model <- function(x, ...) {
  cat("# Model Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  cat("- Parameter Dimension: ", x$param_dim, "\n", sep = "")

  cat("- theta_mle_fn:        "); print_fn(x$theta_mle_fn)
  cat("- loglik:              "); print_fn(x$loglik)
  cat("- E_loglik:            "); print_fn(x$E_loglik)
  cat("- E_loglik_grad:       "); print_fn(x$E_loglik_grad)

  cat("- lower bounds:        ",
      if (is.null(x$lower)) "NULL" else paste(x$lower, collapse = ", "),
      "\n", sep = "")

  cat("- upper bounds:        ",
      if (is.null(x$upper)) "NULL" else paste(x$upper, collapse = ", "),
      "\n", sep = "")

  cat("- inequality:          "); print_fn(x$ineq)
  cat("- inequality jacobian: "); print_fn(x$ineq_jac)

  invisible(x)
}

# Helper for printing functions compactly
print_fn <- function(f) {
  if (is.null(f)) {
    cat("NULL\n")
  } else {
    args <- tryCatch(names(formals(f)), error = function(e) "<unknown>")
    cat(sprintf("function(%s)\n", paste(args, collapse = ", ")))
  }
}

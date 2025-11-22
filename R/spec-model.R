# ======================================================================
# Model specification for likelihood workflows
# ======================================================================

#' Model specification for likelihood workflows
#'
#' @description
#' Defines a parametric likelihood model for use in profile/integrated
#' likelihood workflows. `loglik` and `E_loglik` must accept only `theta`,
#' with data embedded in closures. Optional box constraints and inequality
#' constraints describe the parameter domain.
#' @param loglik Function(theta, data) → log-likelihood
#' @param E_loglik Function(theta, omega_hat, data) → expected log-likelihood
#' @param param_dim Positive integer dimension of theta
#' @param E_loglik_grad Optional gradient function(theta, omega_hat, data)
#' @param theta_mle_fn Optional Function(data) ->theta_mle
#' @param lower Optional numeric vector (length param_dim)
#' @param upper Optional numeric vector (length param_dim)
#' @param ineq Optional inequality function(theta) → vector <= 0
#' @param ineq_jac Optional Jacobian of `ineq`
#' @param name Optional label
#' @param ... extra metadata stored under `extra`
#' @return S3 object `likelihood_model`
#' @export
model_spec <- function(loglik,
                       E_loglik,
                       param_dim,
                       E_loglik_grad = NULL,
                       theta_mle_fn = NULL,
                       lower = NULL,
                       upper = NULL,
                       ineq = NULL,
                       ineq_jac = NULL,
                       name = NULL,
                       ...) {

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
  validate_model_spec(x)
  x
}

# ======================================================================
# Validation
# ======================================================================
validate_model_spec <- function(x) {

  # Functions
  stopifnot(is.function(x$loglik))
  stopifnot(is.function(x$E_loglik))

  if (!is.null(x$E_loglik_grad))
    stopifnot(is.function(x$E_loglik_grad))

  # Dimension
  if (!is.numeric(x$param_dim) || x$param_dim < 1 || length(x$param_dim) != 1)
    stop("`param_dim` must be a positive integer.", call.=FALSE)

  # MLE function
  if (!is.null(x$theta_mle_fn)) {
    stopifnot(is.function(x$theta_mle_fn))
  }

  # Box constraints
  if (!is.null(x$lower)) {
    if (!is.numeric(x$lower) || length(x$lower) != x$param_dim)
      stop("`lower` must be numeric and match `param_dim`.", call.=FALSE)
  }

  if (!is.null(x$upper)) {
    if (!is.numeric(x$upper) || length(x$upper) != x$param_dim)
      stop("`upper` must be numeric and match `param_dim`.", call.=FALSE)
  }

  if (!is.null(x$lower) && !is.null(x$upper)) {
    if (any(x$lower > x$upper))
      stop("Each `lower[i]` must be <= `upper[i]`.", call.=FALSE)
  }

  # Inequality constraints
  if (!is.null(x$ineq))
    stopifnot(is.function(x$ineq))

  if (!is.null(x$ineq_jac))
    stopifnot(is.function(x$ineq_jac))

  invisible(x)
}

# ======================================================================
# Print
# ======================================================================
#' @export
print.likelihood_model <- function(x, ...) {
  cat("# Model Specification\n")
  cat("- Name:                ", x$name, "\n", sep="")
  cat("- Parameter Dimension: ", x$param_dim, "\n", sep="")

  cat("- theta_mle_fn:        "); print_fn(x$theta_mle_fn)
  cat("- loglik:              "); print_fn(x$loglik)
  cat("- E_loglik:            "); print_fn(x$E_loglik)
  cat("- E_loglik_grad:       "); print_fn(x$E_loglik_grad)

  cat("- lower bounds:        ", if(is.null(x$lower)) "NULL" else paste(x$lower, collapse=", "), "\n", sep="")
  cat("- upper bounds:        ", if(is.null(x$upper)) "NULL" else paste(x$upper, collapse=", "), "\n", sep="")
  cat("- inequality:          "); print_fn(x$ineq)
  cat("- inequality jacobian: "); print_fn(x$ineq_jac)

  invisible(x)
}

# Helper
print_fn <- function(f) {
  if (is.null(f)) {
    cat("NULL\n")
  } else {
    args <- tryCatch(names(formals(f)), error=function(e) "<unknown>")
    cat(sprintf("function(%s)\n", paste(args, collapse=", ")))
  }
}

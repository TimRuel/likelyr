# ======================================================================
# Likelihood Specification (v3.1)
# ======================================================================

#' Specify a Parametric Likelihood Model
#'
#' @description
#' Defines:
#'   • log-likelihood p(y | theta)
#'   • dimension J of theta
#'   • analytic theta_MLE initializer
#'   • model-family constraints: box + inequality
#'
#' @return A `likelihood_spec` object with classes
#'         c("likelihood_spec", "likelyr_spec").
#' @export
likelihood_spec <- function(loglik,
                            theta_dim,
                            theta_mle_fn,
                            theta_lower = NULL,
                            theta_upper = NULL,
                            ineq = NULL,
                            ineq_jac = NULL,
                            name = NULL,
                            ...) {

  x <- list(
    name         = name %||% "<likelihood>",
    loglik       = loglik,
    theta_dim    = theta_dim,
    theta_mle_fn = theta_mle_fn,
    theta_lower  = theta_lower,
    theta_upper  = theta_upper,
    ineq         = ineq,
    ineq_jac     = ineq_jac,
    extra        = list(...)
  )

  # 1. Attach unified classes
  x <- new_likelihood_spec(x)

  # 2. Validate and finalize
  x <- .validate_likelihood_spec(x)

  x
}

# ----------------------------------------------------------------------
# INTERNAL VALIDATOR
# ----------------------------------------------------------------------

.validate_likelihood_spec <- function(x) {

  # loglik ---------------------------------------------------------------
  if (!is.function(x$loglik))
    stop("loglik must be a function(theta, data).", call. = FALSE)

  # theta_dim -------------------------------------------------------------
  if (!is.numeric(x$theta_dim) ||
      length(x$theta_dim) != 1 ||
      x$theta_dim < 1 ||
      x$theta_dim != as.integer(x$theta_dim))
    stop("theta_dim must be a positive integer.", call. = FALSE)

  J <- as.integer(x$theta_dim)

  # analytic MLE ----------------------------------------------------------
  if (!is.function(x$theta_mle_fn))
    stop("theta_mle_fn must be a function(data).", call. = FALSE)

  # box constraints -------------------------------------------------------
  lower <- x$theta_lower
  upper <- x$theta_upper

  if (!is.null(lower)) {
    if (!is.numeric(lower))
      stop("theta_lower must be numeric.", call. = FALSE)
    if (length(lower) == 1)
      lower <- rep(lower, J)
    if (length(lower) != J)
      stop("theta_lower must be scalar or length theta_dim.", call. = FALSE)
  }

  if (!is.null(upper)) {
    if (!is.numeric(upper))
      stop("theta_upper must be numeric.", call. = FALSE)
    if (length(upper) == 1)
      upper <- rep(upper, J)
    if (length(upper) != J)
      stop("theta_upper must be scalar or length theta_dim.", call. = FALSE)
  }

  if (!is.null(lower) && !is.null(upper) && any(lower > upper))
    stop("Each element must satisfy theta_lower[i] <= theta_upper[i].",
         call. = FALSE)

  # finalize normalization
  x$theta_lower <- lower
  x$theta_upper <- upper

  # inequality constraints ------------------------------------------------
  if (!is.null(x$ineq) && !is.function(x$ineq))
    stop("ineq must be NULL or a function(theta) → numeric vector <= 0.",
         call. = FALSE)

  if (!is.null(x$ineq_jac) && !is.function(x$ineq_jac))
    stop("ineq_jac must be NULL or a function(theta) → Jacobian matrix.",
         call. = FALSE)

  # Jacobian dimensionality test -----------------------------------------
  if (!is.null(x$ineq) && !is.null(x$ineq_jac)) {

    test_theta <- if (!is.null(lower) && !is.null(upper)) {
      (lower + upper) / 2
    } else {
      rep(0, J)
    }

    g_val  <- x$ineq(test_theta)
    jacval <- x$ineq_jac(test_theta)

    if (!is.numeric(g_val))
      stop("ineq(theta) must return a numeric vector.", call. = FALSE)

    if (!is.matrix(jacval))
      stop("ineq_jac(theta) must return a matrix.", call. = FALSE)

    if (nrow(jacval) != length(g_val) || ncol(jacval) != J)
      stop("ineq_jac(theta) must be a matrix with dims: n_constraints × theta_dim.",
           call. = FALSE)
  }

  x
}

# ------------------------------------------------------
# PRINT METHOD
# ------------------------------------------------------

#' @export
print.likelihood_spec <- function(x, ...) {
  cat("# Likelihood Specification\n")
  cat("- Name: ", x$name, "\n", sep = "")
  invisible(x)
}


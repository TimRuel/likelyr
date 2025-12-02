# ======================================================================
# Likelihood Specification (v3.1)
# ======================================================================

#' Specify a Parametric Likelihood Model
#'
#' @description
#' Defines:
#'   • the log-likelihood p(y | theta)
#'   • the dimension J of theta
#'   • an analytic θ̂_MLE initializer (required)
#'   • model-level constraints:
#'        – box:       theta_lower <= theta <= theta_upper
#'        – inequality: ineq(theta) <= 0
#'
#' These constraints belong to the *model family*, not the optimizer.
#'
#' @param loglik        Function(theta, data) → numeric log-likelihood.
#' @param theta_dim     Integer J = dim(theta).
#' @param theta_mle_fn  Function(data) → theta_MLE.
#'
#' @param theta_lower   Optional scalar or numeric vector (length J).
#' @param theta_upper   Optional scalar or numeric vector (length J).
#'
#' @param ineq          Optional inequality constraint:
#'                       function(theta) → numeric vector <= 0.
#' @param ineq_jac      Optional Jacobian:
#'                       function(theta) → matrix (K × J).
#'
#' @param name          Optional descriptive name.
#' @param ...           Extra fields.
#'
#' @return A `likelihood_spec` object.
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

  class(x) <- "likelihood_spec"
  .validate_likelihood_spec(x)
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
  # recycle scalar → length J
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

  x$theta_lower <- lower
  x$theta_upper <- upper

  # inequality constraints ------------------------------------------------
  if (!is.null(x$ineq) && !is.function(x$ineq))
    stop("ineq must be NULL or function(theta) → numeric vector <= 0.",
         call. = FALSE)

  if (!is.null(x$ineq_jac) && !is.function(x$ineq_jac))
    stop("ineq_jac must be NULL or a function(theta) → Jacobian matrix.",
         call. = FALSE)

  # ---- Optional: Check inequality Jacobian shape -----------------------
  if (!is.null(x$ineq) && !is.null(x$ineq_jac)) {

    # test at theta = midpoint of box if box exists, else zeros
    test_theta <- if (!is.null(lower) && !is.null(upper)) {
      (lower + upper) / 2
    } else {
      rep(0, J)
    }

    # check dimensions only if the functions run successfully
    g_val <- x$ineq(test_theta)
    jac_val <- x$ineq_jac(test_theta)

    if (!is.numeric(g_val))
      stop("ineq(theta) must return a numeric vector.", call. = FALSE)

    if (!is.matrix(jac_val))
      stop("ineq_jac(theta) must return a matrix.", call. = FALSE)

    if (nrow(jac_val) != length(g_val) || ncol(jac_val) != J)
      stop("ineq_jac(theta) must be a matrix with dimensions: ",
           "n_constraints × theta_dim.",
           call. = FALSE)
  }

  invisible(x)
}

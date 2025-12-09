# ======================================================================
# Parameter Specification (v2.2)
# ======================================================================

#' Specify the Parameter Space for a Likelihood Model
#'
#' @description
#' Defines the parameter space for θ, including:
#'
#'   • dimension of θ
#'   • optional true value θ₀
#'   • box constraints (θ_lower, θ_upper)
#'   • inequality constraints h(θ) ≤ 0 and Jacobian
#'
#' Exactly one of `theta_0` or `theta_dim` must be supplied.
#'
#' @param theta_0 Optional numeric vector giving the true θ.
#' @param theta_dim Optional integer giving the parameter dimension.
#' @param theta_lower Optional numeric scalar or vector of lower bounds.
#' @param theta_upper Optional numeric scalar or vector of upper bounds.
#' @param ineq Optional function(theta) → numeric vector ≤ 0.
#' @param ineq_jac Optional Jacobian function(theta) → matrix.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused.
#'
#' @return A `parameter_spec` object.
#' @export
parameter_spec <- function(theta_0 = NULL,
                           theta_dim = NULL,
                           theta_lower = NULL,
                           theta_upper = NULL,
                           ineq = NULL,
                           ineq_jac = NULL,
                           name = NULL,
                           ...) {

  # --------------------------------------------------------------
  # Construct skeleton object
  # --------------------------------------------------------------
  x <- list(
    name        = name %||% "<parameters>",
    theta_0     = theta_0,
    theta_dim   = theta_dim,
    theta_lower = theta_lower,
    theta_upper = theta_upper,
    ineq        = ineq,
    ineq_jac    = ineq_jac,
    extra       = list(...)
  )

  # Apply unified constructor
  x <- new_parameter_spec(x)

  # Validate and normalize all fields
  x <- .validate_parameter_spec(x)

  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

.validate_parameter_spec <- function(x) {

  theta_0     <- x$theta_0
  theta_dim   <- x$theta_dim
  theta_lower <- x$theta_lower
  theta_upper <- x$theta_upper
  ineq        <- x$ineq
  ineq_jac    <- x$ineq_jac

  # --------------------------------------------------------------
  # 1. Mutually exclusive theta_0 / theta_dim
  # --------------------------------------------------------------
  has_theta0   <- !is.null(theta_0)
  has_thetadim <- !is.null(theta_dim)

  if (has_theta0 && has_thetadim)
    stop("parameter_spec(): Supply either theta_0 OR theta_dim, not both.",
         call. = FALSE)

  if (!has_theta0 && !has_thetadim)
    stop("parameter_spec(): You must supply theta_0 or theta_dim.",
         call. = FALSE)

  # --------------------------------------------------------------
  # 2. Determine dimension
  # --------------------------------------------------------------
  if (has_theta0) {

    if (!is.numeric(theta_0) || any(!is.finite(theta_0)))
      stop("theta_0 must be a finite numeric vector.", call. = FALSE)

    J <- length(theta_0)

  } else {

    if (!is.numeric(theta_dim) ||
        length(theta_dim) != 1 ||
        theta_dim < 1 ||
        theta_dim != as.integer(theta_dim))
      stop("theta_dim must be a positive integer.", call. = FALSE)

    J <- as.integer(theta_dim)
    theta_0 <- NULL
  }

  # --------------------------------------------------------------
  # 3. Normalize box constraints
  # --------------------------------------------------------------

  # Lower
  if (!is.null(theta_lower)) {
    if (!is.numeric(theta_lower))
      stop("theta_lower must be numeric.", call. = FALSE)
    if (length(theta_lower) == 1)
      theta_lower <- rep(theta_lower, J)
    if (length(theta_lower) != J)
      stop("theta_lower must be scalar or length J.", call. = FALSE)
  }

  # Upper
  if (!is.null(theta_upper)) {
    if (!is.numeric(theta_upper))
      stop("theta_upper must be numeric.", call. = FALSE)
    if (length(theta_upper) == 1)
      theta_upper <- rep(theta_upper, J)
    if (length(theta_upper) != J)
      stop("theta_upper must be scalar or length J.", call. = FALSE)
  }

  # Lower ≤ Upper
  if (!is.null(theta_lower) && !is.null(theta_upper)) {
    if (any(theta_lower > theta_upper))
      stop("theta_lower[i] must be <= theta_upper[i].", call. = FALSE)
  }

  # --------------------------------------------------------------
  # 4. Validate theta_0 consistency with constraints
  # --------------------------------------------------------------
  if (!is.null(theta_0)) {

    if (!is.null(theta_lower) && any(theta_0 < theta_lower))
      stop("theta_0 violates theta_lower constraints.", call. = FALSE)

    if (!is.null(theta_upper) && any(theta_0 > theta_upper))
      stop("theta_0 violates theta_upper constraints.", call. = FALSE)
  }

  # --------------------------------------------------------------
  # 5. Validate inequality constraints
  # --------------------------------------------------------------

  if (!is.null(ineq) && !is.function(ineq))
    stop("ineq must be NULL or a function(theta).", call. = FALSE)

  if (!is.null(ineq_jac) && !is.function(ineq_jac))
    stop("ineq_jac must be NULL or a function(theta).", call. = FALSE)

  if (!is.null(ineq) && !is.null(ineq_jac)) {

    test_theta <-
      if (!is.null(theta_0)) theta_0
    else if (!is.null(theta_lower) && !is.null(theta_upper))
      (theta_lower + theta_upper) / 2
    else rep(0, J)

    g <- ineq(test_theta)
    if (!is.numeric(g))
      stop("ineq(theta) must return a numeric vector.", call. = FALSE)

    jac <- ineq_jac(test_theta)
    if (!is.matrix(jac))
      stop("ineq_jac(theta) must return a matrix.", call. = FALSE)

    if (nrow(jac) != length(g) || ncol(jac) != J)
      stop("ineq_jac(theta) must be a matrix of size n_constraints × theta_dim.",
           call. = FALSE)
  }

  # --------------------------------------------------------------
  # 6. Write back normalized fields
  # --------------------------------------------------------------
  x$theta_dim   <- J
  x$theta_0     <- theta_0
  x$theta_lower <- theta_lower
  x$theta_upper <- theta_upper

  x
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.parameter_spec <- function(x, ...) {
  cat("# Parameter Specification\n")
  cat("- Name:        ", x$name, "\n", sep = "")
  cat("- Dimension:   ", x$theta_dim, "\n", sep = "")

  if (!is.null(x$theta_0))
    cat("- True value:  (", paste(format(x$theta_0), collapse = ", "), ")\n", sep = "")

  if (!is.null(x$theta_lower))
    cat("- Lower bounds: (", paste(format(x$theta_lower), collapse = ", "), ")\n", sep = "")

  if (!is.null(x$theta_upper))
    cat("- Upper bounds: (", paste(format(x$theta_upper), collapse = ", "), ")\n", sep = "")

  if (!is.null(x$ineq))
    cat("- Inequality constraints: present\n")

  invisible(x)
}

# ======================================================================
# Parameter Specification (v3.1)
# ======================================================================

#' Specify the Parameter Space for a Likelihood Model
#'
#' @description
#' Defines the parameter space, including:
#'
#'   • dimension
#'   • optional true value
#'   • box constraints (lower, upper)
#'   • inequality constraints h(param) ≤ 0 and Jacobian
#'
#' Exactly one of `param_0` or `param_dim` must be supplied.
#'
#' @param param_0 Optional numeric vector OR 1-column matrix giving true parameters.
#' @param param_dim Optional integer giving the parameter dimension.
#' @param param_lower Optional numeric scalar or vector of lower bounds.
#' @param param_upper Optional numeric scalar or vector of upper bounds.
#' @param ineq Optional function(param) → numeric vector ≤ 0.
#' @param ineq_jac Optional Jacobian function(param) → matrix.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused.
#'
#' @return A `parameter_spec` object.
#' @export
parameter_spec <- function(
  param_0 = NULL,
  param_dim = NULL,
  param_lower = NULL,
  param_upper = NULL,
  ineq = NULL,
  ineq_jac = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<parameters>",
    param_0 = param_0,
    param_dim = param_dim,
    param_lower = param_lower,
    param_upper = param_upper,
    ineq = ineq,
    ineq_jac = ineq_jac,
    extra = list(...)
  )

  x <- new_parameter_spec(x)
  x <- .validate_parameter_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' Validate parameter specification
#'
#' @description
#' Internal validator for \code{parameter_spec} objects. This function
#' enforces the structural and numerical constraints required for
#' parameter definitions used in likelihood optimization.
#'
#' @details
#' The following checks and transformations are performed:
#'
#' \itemize{
#'   \item Exactly one of \code{param_0} (initial value) or
#'         \code{param_dim} (dimension) must be supplied.
#'
#'   \item If \code{param_0} is supplied, it must be a finite numeric
#'         vector or a one-column numeric matrix. Its length defines
#'         the parameter dimension.
#'
#'   \item If \code{param_dim} is supplied, it must be a positive integer.
#'
#'   \item Box constraints \code{param_lower} and \code{param_upper}, if
#'         provided, must be numeric scalars or vectors of length
#'         \code{param_dim}. Scalars are recycled.
#'
#'   \item Lower bounds must not exceed upper bounds.
#'
#'   \item If \code{param_0} is supplied, it must respect all box
#'         constraints.
#'
#'   \item Inequality constraint functions \code{ineq} and
#'         \code{ineq_jac}, if supplied, must be functions. If both are
#'         provided, their dimensions are validated for consistency.
#' }
#'
#' After validation, the normalized fields \code{param_dim},
#' \code{param_0}, \code{param_lower}, and \code{param_upper} are written
#' back into the object.
#'
#' @param x A list representing a \code{parameter_spec} object.
#'
#' @return Returns the validated and normalized \code{parameter_spec}
#' object.
#'
#' @keywords internal
#' @noRd
.validate_parameter_spec <- function(x) {
  param_0 <- x$param_0
  param_dim <- x$param_dim
  param_lower <- x$param_lower
  param_upper <- x$param_upper
  ineq <- x$ineq
  ineq_jac <- x$ineq_jac

  # --------------------------------------------------------------
  # 1. Mutually exclusive param_0 / param_dim
  # --------------------------------------------------------------
  has_param0 <- !is.null(param_0)
  has_paramdim <- !is.null(param_dim)

  if (has_param0 && has_paramdim) {
    stop(
      "parameter_spec(): Supply either param_0 OR param_dim, not both.",
      call. = FALSE
    )
  }

  if (!has_param0 && !has_paramdim) {
    stop(
      "parameter_spec(): You must supply param_0 or param_dim.",
      call. = FALSE
    )
  }

  # --------------------------------------------------------------
  # 2. Determine dimension
  # --------------------------------------------------------------
  if (has_param0) {
    # Allow 1-column matrix
    if (is.matrix(param_0)) {
      if (ncol(param_0) != 1) {
        stop("param_0 matrix must have exactly one column.", call. = FALSE)
      }

      if (!is.numeric(param_0) || any(!is.finite(param_0))) {
        stop("param_0 matrix must be finite numeric.", call. = FALSE)
      }

      J <- nrow(param_0)
    } else {
      if (!is.numeric(param_0) || any(!is.finite(param_0))) {
        stop("param_0 must be a finite numeric vector.", call. = FALSE)
      }

      J <- length(param_0)
    }
  } else {
    if (
      !is.numeric(param_dim) ||
        length(param_dim) != 1 ||
        param_dim < 1 ||
        param_dim != as.integer(param_dim)
    ) {
      stop("param_dim must be a positive integer.", call. = FALSE)
    }

    J <- as.integer(param_dim)
    param_0 <- NULL
  }

  # --------------------------------------------------------------
  # 3. Normalize box constraints
  # --------------------------------------------------------------

  if (!is.null(param_lower)) {
    if (!is.numeric(param_lower)) {
      stop("param_lower must be numeric.", call. = FALSE)
    }

    if (length(param_lower) == 1) {
      param_lower <- rep(param_lower, J)
    }

    if (length(param_lower) != J) {
      stop("param_lower must be scalar or length J.", call. = FALSE)
    }
  }

  if (!is.null(param_upper)) {
    if (!is.numeric(param_upper)) {
      stop("param_upper must be numeric.", call. = FALSE)
    }

    if (length(param_upper) == 1) {
      param_upper <- rep(param_upper, J)
    }

    if (length(param_upper) != J) {
      stop("param_upper must be scalar or length J.", call. = FALSE)
    }
  }

  if (!is.null(param_lower) && !is.null(param_upper)) {
    if (any(param_lower > param_upper)) {
      stop("param_lower[i] must be <= param_upper[i].", call. = FALSE)
    }
  }

  # --------------------------------------------------------------
  # 4. Validate param_0 vs constraints
  # --------------------------------------------------------------
  if (!is.null(param_0)) {
    param_vec <- if (is.matrix(param_0)) {
      as.numeric(param_0)
    } else {
      param_0
    }

    if (!is.null(param_lower) && any(param_vec < param_lower)) {
      stop("param_0 violates param_lower constraints.", call. = FALSE)
    }

    if (!is.null(param_upper) && any(param_vec > param_upper)) {
      stop("param_0 violates param_upper constraints.", call. = FALSE)
    }
  }

  # --------------------------------------------------------------
  # 5. Validate inequality constraints
  # --------------------------------------------------------------

  if (!is.null(ineq) && !is.function(ineq)) {
    stop("ineq must be NULL or a function(param).", call. = FALSE)
  }

  if (!is.null(ineq_jac) && !is.function(ineq_jac)) {
    stop("ineq_jac must be NULL or a function(param).", call. = FALSE)
  }

  if (!is.null(ineq) && !is.null(ineq_jac)) {
    test_param <-
      if (!is.null(param_0)) {
        if (is.matrix(param_0)) as.numeric(param_0) else param_0
      } else if (!is.null(param_lower) && !is.null(param_upper)) {
        (param_lower + param_upper) / 2
      } else {
        rep(0, J)
      }

    g <- ineq(test_param)
    if (!is.numeric(g)) {
      stop("ineq(param) must return a numeric vector.", call. = FALSE)
    }

    jac <- ineq_jac(test_param)
    if (!is.matrix(jac)) {
      stop("ineq_jac(param) must return a matrix.", call. = FALSE)
    }

    if (nrow(jac) != length(g) || ncol(jac) != J) {
      stop(
        "ineq_jac(param) must be a matrix of size n_constraints × param_dim.",
        call. = FALSE
      )
    }
  }

  # --------------------------------------------------------------
  # 6. Write back normalized fields
  # --------------------------------------------------------------
  x$param_dim <- J
  x$param_0 <- param_0
  x$param_lower <- param_lower
  x$param_upper <- param_upper

  x
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.parameter_spec <- function(x, ...) {
  cat("# Parameter Specification\n")
  cat("- Name:        ", x$name, "\n", sep = "")
  cat("- Dimension:   ", x$param_dim, "\n", sep = "")

  if (!is.null(x$param_0)) {
    cat("- True value:\n")

    if (is.matrix(x$param_0)) {
      print(x$param_0)
    } else {
      cat("  (", paste(format(x$param_0), collapse = ", "), ")\n", sep = "")
    }
  }

  if (!is.null(x$param_lower)) {
    cat(
      "- Lower bounds: (",
      paste(format(x$param_lower), collapse = ", "),
      ")\n",
      sep = ""
    )
  }

  if (!is.null(x$param_upper)) {
    cat(
      "- Upper bounds: (",
      paste(format(x$param_upper), collapse = ", "),
      ")\n",
      sep = ""
    )
  }

  if (!is.null(x$ineq)) {
    cat("- Inequality constraints: present\n")
  }

  invisible(x)
}

# ======================================================================
# Parameter Specification (v3.5) — equality + inequality constraints
# ======================================================================

#' Specify the Parameter Space for a Likelihood Model
#'
#' @description
#' Defines the parameter space, including:
#'
#'   • dimension
#'   • optional true value(s)
#'   • box constraints (lower, upper)
#'   • equality constraints h(param) = 0 and Jacobian
#'   • inequality constraints h(param) ≤ 0 and Jacobian
#'   • analytic MLE initializer
#'
#' Exactly one of `param_0` or `param_dim` must be supplied.
#'
#' @param param_mle_fn Function(data) → numeric vector giving the
#'   maximum likelihood estimate of the full unconstrained parameter
#'   vector. Called once during calibration to compute `param_mle`,
#'   which is then used to resolve "auto" bounds, evaluate `psi_mle`,
#'   and initialize branch evaluation. Must return a finite numeric
#'   vector of length `param_dim`.
#' @param param_0 Optional numeric vector, 1-column matrix,
#'   OR list of such objects giving value of true model parameter.
#' @param param_dim Optional integer giving the parameter dimension.
#' @param param_lower Optional numeric scalar or vector of lower bounds,
#'   or the string \code{"auto"} to request bounds computed from
#'   \code{param_mle} during calibration. When \code{"auto"}, bounds
#'   are set to \code{param_mle - radius} where
#'   \code{radius = max(abs(param_mle)) * 3 + 5}.
#' @param param_upper Optional numeric scalar or vector of upper bounds,
#'   or \code{"auto"} (see \code{param_lower}).
#' @param eq Optional function(param) → numeric vector = 0.
#' @param eq_jac Optional Jacobian function(param) → matrix.
#' @param ineq Optional function(param) → numeric vector ≤ 0.
#' @param ineq_jac Optional Jacobian function(param) → matrix.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused.
#'
#' @return A `parameter_spec` object.
#' @export
parameter_spec <- function(
  param_mle_fn,
  param_0 = NULL,
  param_dim = NULL,
  param_lower = NULL,
  param_upper = NULL,
  eq = NULL,
  eq_jac = NULL,
  ineq = NULL,
  ineq_jac = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<parameters>",
    param_mle_fn = param_mle_fn,
    param_0 = param_0,
    param_dim = param_dim,
    param_lower = param_lower,
    param_upper = param_upper,
    eq = eq,
    eq_jac = eq_jac,
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

.validate_parameter_spec <- function(x) {
  param_mle_fn <- x$param_mle_fn
  param_0 <- x$param_0
  param_dim <- x$param_dim
  param_lower <- x$param_lower
  param_upper <- x$param_upper
  eq <- x$eq
  eq_jac <- x$eq_jac
  ineq <- x$ineq
  ineq_jac <- x$ineq_jac

  # --------------------------------------------------------------
  # 0. Analytic MLE initializer — required, validated first
  # --------------------------------------------------------------
  if (!is.function(param_mle_fn)) {
    stop("param_mle_fn must be a function(data).", call. = FALSE)
  }

  # --------------------------------------------------------------
  # 1. Classify bounds — "auto" is valid; resolved at calibration
  # --------------------------------------------------------------
  lower_is_auto <- identical(param_lower, "auto")
  upper_is_auto <- identical(param_upper, "auto")

  # --------------------------------------------------------------
  # 2. Mutually exclusive param_0 / param_dim
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
  # 3. Determine dimension
  # --------------------------------------------------------------
  if (has_param0) {
    if (is.list(param_0)) {
      if (length(param_0) == 0) {
        stop("param_0 list cannot be empty.", call. = FALSE)
      }

      lens <- vapply(
        param_0,
        function(p) {
          if (is.matrix(p)) {
            if (ncol(p) != 1) {
              stop(
                "Each param_0 matrix must have exactly one column.",
                call. = FALSE
              )
            }
            if (!is.numeric(p) || any(!is.finite(p))) {
              stop("param_0 matrices must be finite numeric.", call. = FALSE)
            }
            nrow(p)
          } else {
            if (!is.numeric(p) || any(!is.finite(p))) {
              stop(
                "param_0 list elements must be finite numeric vectors.",
                call. = FALSE
              )
            }
            length(p)
          }
        },
        integer(1)
      )

      param_dim <- sum(lens)
    } else if (is.matrix(param_0)) {
      if (ncol(param_0) != 1) {
        stop("param_0 matrix must have exactly one column.", call. = FALSE)
      }

      if (!is.numeric(param_0) || any(!is.finite(param_0))) {
        stop("param_0 matrix must be finite numeric.", call. = FALSE)
      }

      param_dim <- nrow(param_0)
    } else {
      if (!is.numeric(param_0) || any(!is.finite(param_0))) {
        stop("param_0 must be a finite numeric vector.", call. = FALSE)
      }

      param_dim <- length(param_0)
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

    param_dim <- as.integer(param_dim)
    param_0 <- NULL
  }

  # --------------------------------------------------------------
  # 4. Normalize box constraints
  #    "auto" sentinels are left as-is; resolved in calibrate_parameter
  #    once param_mle is known.
  # --------------------------------------------------------------
  if (!is.null(param_lower) && !lower_is_auto) {
    if (!is.numeric(param_lower)) {
      stop('param_lower must be numeric or "auto".', call. = FALSE)
    }

    if (length(param_lower) == 1L) {
      param_lower <- rep(param_lower, param_dim)
    }

    if (length(param_lower) != param_dim) {
      stop(
        "param_lower must be scalar or length param_dim.",
        call. = FALSE
      )
    }
  }

  if (!is.null(param_upper) && !upper_is_auto) {
    if (!is.numeric(param_upper)) {
      stop('param_upper must be numeric or "auto".', call. = FALSE)
    }

    if (length(param_upper) == 1L) {
      param_upper <- rep(param_upper, param_dim)
    }

    if (length(param_upper) != param_dim) {
      stop(
        "param_upper must be scalar or length param_dim.",
        call. = FALSE
      )
    }
  }

  # Cross-check only when both bounds are resolved numeric values
  if (
    !is.null(param_lower) &&
      !lower_is_auto &&
      !is.null(param_upper) &&
      !upper_is_auto
  ) {
    if (any(param_lower > param_upper)) {
      stop("param_lower[i] must be <= param_upper[i].", call. = FALSE)
    }
  }

  # --------------------------------------------------------------
  # 5. Validate param_0 vs constraints
  #    Skip bound checks when either bound is "auto".
  # --------------------------------------------------------------
  if (!is.null(param_0)) {
    param_list <- if (is.list(param_0)) param_0 else list(param_0)

    for (p in param_list) {
      param_vec <- if (is.matrix(p)) as.numeric(p) else p

      if (
        !lower_is_auto && !is.null(param_lower) && any(param_vec < param_lower)
      ) {
        stop("param_0 violates param_lower constraints.", call. = FALSE)
      }

      if (
        !upper_is_auto && !is.null(param_upper) && any(param_vec > param_upper)
      ) {
        stop("param_0 violates param_upper constraints.", call. = FALSE)
      }
    }
  }

  # --------------------------------------------------------------
  # 6a. Validate equality constraints
  # --------------------------------------------------------------
  if (!is.null(eq) && !is.function(eq)) {
    stop("eq must be NULL or a function(param).", call. = FALSE)
  }

  if (!is.null(eq_jac) && !is.function(eq_jac)) {
    stop("eq_jac must be NULL or a function(param).", call. = FALSE)
  }

  if (!is.null(eq) && !is.null(eq_jac)) {
    test_param <- .make_test_param(
      param_0,
      param_dim,
      param_lower,
      param_upper,
      lower_is_auto,
      upper_is_auto
    )

    h <- eq(test_param)
    if (!is.numeric(h)) {
      stop("eq(param) must return a numeric vector.", call. = FALSE)
    }

    jac <- eq_jac(test_param)
    if (!is.matrix(jac)) {
      stop("eq_jac(param) must return a matrix.", call. = FALSE)
    }

    if (nrow(jac) != length(h) || ncol(jac) != param_dim) {
      stop(
        "eq_jac(param) must be a matrix of size n_constraints x param_dim.",
        call. = FALSE
      )
    }
  }

  # --------------------------------------------------------------
  # 6b. Validate inequality constraints
  # --------------------------------------------------------------
  if (!is.null(ineq) && !is.function(ineq)) {
    stop("ineq must be NULL or a function(param).", call. = FALSE)
  }

  if (!is.null(ineq_jac) && !is.function(ineq_jac)) {
    stop("ineq_jac must be NULL or a function(param).", call. = FALSE)
  }

  if (!is.null(ineq) && !is.null(ineq_jac)) {
    test_param <- .make_test_param(
      param_0,
      param_dim,
      param_lower,
      param_upper,
      lower_is_auto,
      upper_is_auto
    )

    g <- ineq(test_param)
    if (!is.numeric(g)) {
      stop("ineq(param) must return a numeric vector.", call. = FALSE)
    }

    jac <- ineq_jac(test_param)
    if (!is.matrix(jac)) {
      stop("ineq_jac(param) must return a matrix.", call. = FALSE)
    }

    if (nrow(jac) != length(g) || ncol(jac) != param_dim) {
      stop(
        "ineq_jac(param) must be a matrix of size n_constraints x param_dim.",
        call. = FALSE
      )
    }
  }

  # --------------------------------------------------------------
  # 7. Write back all normalized fields
  # --------------------------------------------------------------
  x$param_mle_fn <- param_mle_fn # explicit write-back for consistency
  x$param_dim <- param_dim
  x$param_0 <- param_0
  x$param_lower <- param_lower
  x$param_upper <- param_upper
  x$eq <- eq
  x$eq_jac <- eq_jac
  x$ineq <- ineq
  x$ineq_jac <- ineq_jac

  x
}

# ----------------------------------------------------------------------
# Internal helper: construct a test parameter for constraint validation
# Extracted to avoid duplicating the same logic for eq and ineq checks.
# ----------------------------------------------------------------------

.make_test_param <- function(
  param_0,
  param_dim,
  param_lower,
  param_upper,
  lower_is_auto,
  upper_is_auto
) {
  if (!is.null(param_0)) {
    p0 <- if (is.list(param_0)) param_0[[1]] else param_0
    if (is.matrix(p0)) as.numeric(p0) else p0
  } else if (
    !lower_is_auto &&
      !is.null(param_lower) &&
      !upper_is_auto &&
      !is.null(param_upper)
  ) {
    (param_lower + param_upper) / 2
  } else {
    rep(0, param_dim)
  }
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.parameter_spec <- function(x, ...) {
  cat("# Parameter Specification\n")
  cat("- Name:            ", x$name, "\n", sep = "")
  cat("- Dimension:       ", x$param_dim, "\n", sep = "")
  cat("- param_mle_fn():  ✔ function\n")

  if (!is.null(x$param_0)) {
    cat("- True value(s):\n")

    fmt_vec <- function(v) {
      nms <- names(v)
      vals <- format(v)
      if (!is.null(nms)) {
        paste0(nms, " = ", vals, collapse = ", ")
      } else {
        paste(vals, collapse = ", ")
      }
    }

    if (is.list(x$param_0)) {
      list_names <- names(x$param_0)
      for (i in seq_along(x$param_0)) {
        label <- if (!is.null(list_names) && nzchar(list_names[i])) {
          list_names[i]
        } else {
          i
        }
        cat("  [", label, "]: ", sep = "")
        p <- x$param_0[[i]]
        v <- if (is.matrix(p)) as.numeric(p) else p
        names(v) <- if (!is.null(names(p))) names(p) else names(v)
        cat("(", fmt_vec(v), ")\n", sep = "")
      }
    } else if (is.matrix(x$param_0)) {
      v <- as.numeric(x$param_0)
      names(v) <- rownames(x$param_0)
      cat("  (", fmt_vec(v), ")\n", sep = "")
    } else {
      cat("  (", fmt_vec(x$param_0), ")\n", sep = "")
    }
  }

  fmt_bounds <- function(b) {
    if (identical(b, "auto")) {
      "(auto — resolved at calibration)"
    } else {
      paste0("(", paste(format(b), collapse = ", "), ")")
    }
  }

  if (!is.null(x$param_lower)) {
    cat("- Lower bounds:    ", fmt_bounds(x$param_lower), "\n", sep = "")
  }

  if (!is.null(x$param_upper)) {
    cat("- Upper bounds:    ", fmt_bounds(x$param_upper), "\n", sep = "")
  }

  if (!is.null(x$eq)) {
    cat("- Equality constraints:   present\n")
  }

  if (!is.null(x$ineq)) {
    cat("- Inequality constraints: present\n")
  }

  invisible(x)
}

# ======================================================================
# Parameter Specification (v3.2) — list support for param_0
# ======================================================================

#' Specify the Parameter Space for a Likelihood Model
#'
#' @description
#' Defines the parameter space, including:
#'
#'   • dimension
#'   • optional true value(s)
#'   • box constraints (lower, upper)
#'   • inequality constraints h(param) ≤ 0 and Jacobian
#'
#' Exactly one of `param_0` or `param_dim` must be supplied.
#'
#' @param param_0 Optional numeric vector, 1-column matrix,
#'   OR list of such objects giving true / initial parameters.
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

      # 🔹 NEW: total dimension = sum of sub-dimensions
      J <- sum(lens)
    } else if (is.matrix(param_0)) {
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
    param_list <- if (is.list(param_0)) param_0 else list(param_0)

    for (p in param_list) {
      param_vec <- if (is.matrix(p)) as.numeric(p) else p

      if (!is.null(param_lower) && any(param_vec < param_lower)) {
        stop("param_0 violates param_lower constraints.", call. = FALSE)
      }

      if (!is.null(param_upper) && any(param_vec > param_upper)) {
        stop("param_0 violates param_upper constraints.", call. = FALSE)
      }
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
        p0 <- if (is.list(param_0)) param_0[[1]] else param_0
        if (is.matrix(p0)) as.numeric(p0) else p0
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
    cat("- True value(s):\n")

    # ---------------------------
    # Helper to format one vector
    # ---------------------------
    fmt_vec <- function(v) {
      nms <- names(v)
      vals <- format(v)

      if (!is.null(nms)) {
        paste0(nms, " = ", vals, collapse = ", ")
      } else {
        paste(vals, collapse = ", ")
      }
    }

    # ---------------------------
    # Case: list of starts
    # ---------------------------
    if (is.list(x$param_0)) {
      list_names <- names(x$param_0)

      for (i in seq_along(x$param_0)) {
        label <- if (!is.null(list_names) && nzchar(list_names[i])) {
          paste0(list_names[i])
        } else {
          i
        }

        cat("  [", label, "]: ", sep = "")

        p <- x$param_0[[i]]
        v <- if (is.matrix(p)) as.numeric(p) else p
        names(v) <- if (!is.null(names(p))) names(p) else names(v)

        cat("(", fmt_vec(v), ")\n", sep = "")
      }

      # ---------------------------
      # Case: single matrix
      # ---------------------------
    } else if (is.matrix(x$param_0)) {
      v <- as.numeric(x$param_0)
      names(v) <- rownames(x$param_0)
      cat("  (", fmt_vec(v), ")\n", sep = "")

      # ---------------------------
      # Case: single vector
      # ---------------------------
    } else {
      cat("  (", fmt_vec(x$param_0), ")\n", sep = "")
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

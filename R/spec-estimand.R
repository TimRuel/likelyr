# ======================================================================
# Estimand Specification
# ======================================================================

#' Specify the Parameter of Interest for a Likelihood Model
#'
#' @param psi_fn Required. Function \code{(param, data) -> scalar}.
#'   The parameter of interest as a function of the model parameter.
#' @param psi_jac Optional. Function \code{(param, data) -> matrix}.
#'   Jacobian of \code{psi_fn} with respect to \code{param}, returned
#'   as a 1 x param_dim row vector. When supplied, passed as
#'   \code{heqjac} to the augmented Lagrangian solver.
#' @param psi_lower Optional numeric scalar. Lower bound of the
#'   parameter of interest domain.
#' @param psi_upper Optional numeric scalar. Upper bound of the
#'   parameter of interest domain.
#' @param psi_closed Named logical vector with elements \code{"lower"}
#'   and \code{"upper"} indicating whether the corresponding bound is
#'   closed (inclusive). Default: both \code{FALSE}.
#' @param psi_0 Optional numeric scalar. True value of the parameter of
#'   interest. When supplied, stored directly and not recomputed from
#'   \code{param_0} during calibration.
#' @param make_psi_fns Optional function \code{(data) -> list(psi_fn,
#'   psi_jac)}. When supplied, replaces the standard data-binding of
#'   \code{psi_fn} and \code{psi_jac} during calibration with optimized
#'   closures that precompute all quantities constant in \code{data}
#'   (e.g. the reference covariate \code{x_0} and the number of
#'   categories \code{J}). The returned \code{psi_fn} and \code{psi_jac}
#'   take only \code{param} — they have \code{data}-derived constants
#'   already closed over. This avoids recomputing \code{x_reference(data)}
#'   and \code{get_X_design(data)} on every auglag iteration. When
#'   \code{NULL} (default), the standard binding path is used.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return An \code{estimand_spec} object.
#' @export
estimand_spec <- function(
  psi_fn,
  psi_jac = NULL,
  psi_lower = NULL,
  psi_upper = NULL,
  psi_closed = c(lower = FALSE, upper = FALSE),
  psi_0 = NULL,
  make_psi_fns = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<estimand>",
    psi_fn = psi_fn,
    psi_jac = psi_jac,
    psi_lower = psi_lower,
    psi_upper = psi_upper,
    psi_closed = psi_closed,
    psi_0 = psi_0,
    make_psi_fns = make_psi_fns,
    extra = list(...)
  )

  x <- new_estimand_spec(x)
  x <- .validate_estimand_spec(x)
  x
}

# ======================================================================
# INTERNAL CONSTRUCTOR
# ======================================================================

#' @keywords internal
#' @noRd
new_estimand_spec <- function(x) .new_spec(x, "estimand_spec")

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_estimand_spec <- function(x) {
  if (!is.function(x$psi_fn)) {
    stop("psi_fn must be a function(param, data).", call. = FALSE)
  }
  if (!is.null(x$psi_jac) && !is.function(x$psi_jac)) {
    stop("psi_jac must be NULL or a function(param, data).", call. = FALSE)
  }
  if (
    !is.null(x$psi_lower) &&
      (!is.numeric(x$psi_lower) ||
        length(x$psi_lower) != 1L)
  ) {
    stop("psi_lower must be NULL or a numeric scalar.", call. = FALSE)
  }
  if (
    !is.null(x$psi_upper) &&
      (!is.numeric(x$psi_upper) ||
        length(x$psi_upper) != 1L)
  ) {
    stop("psi_upper must be NULL or a numeric scalar.", call. = FALSE)
  }
  if (
    !is.null(x$psi_lower) && !is.null(x$psi_upper) && x$psi_lower >= x$psi_upper
  ) {
    stop("psi_lower must be strictly less than psi_upper.", call. = FALSE)
  }
  if (
    !is.logical(x$psi_closed) ||
      length(x$psi_closed) != 2L ||
      !all(c("lower", "upper") %in% names(x$psi_closed))
  ) {
    stop(
      'psi_closed must be a named logical vector with elements "lower" and "upper".',
      call. = FALSE
    )
  }
  if (
    !is.null(x$psi_0) &&
      (!is.numeric(x$psi_0) ||
        length(x$psi_0) != 1L ||
        !is.finite(x$psi_0))
  ) {
    stop("psi_0 must be NULL or a finite numeric scalar.", call. = FALSE)
  }
  if (!is.null(x$make_psi_fns) && !is.function(x$make_psi_fns)) {
    stop("make_psi_fns must be NULL or a function(data).", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.estimand_spec <- function(x, ...) {
  cat("# Estimand Specification\n")
  cat("- Name:          ", x$name, "\n", sep = "")
  cat("- psi_fn:         function\n")
  cat(
    "- psi_jac:       ",
    if (!is.null(x$psi_jac)) "function" else "NULL",
    "\n",
    sep = ""
  )
  if (!is.null(x$psi_lower)) {
    cat(
      "- psi_lower:     ",
      x$psi_lower,
      if (isTRUE(x$psi_closed["lower"])) " (closed)" else " (open)",
      "\n",
      sep = ""
    )
  }
  if (!is.null(x$psi_upper)) {
    cat(
      "- psi_upper:     ",
      x$psi_upper,
      if (isTRUE(x$psi_closed["upper"])) " (closed)" else " (open)",
      "\n",
      sep = ""
    )
  }
  if (!is.null(x$psi_0)) {
    cat("- psi_0:         ", x$psi_0, "\n", sep = "")
  }
  cat(
    "- make_psi_fns:  ",
    if (!is.null(x$make_psi_fns)) {
      "function (optimized psi closures)"
    } else {
      "NULL (standard binding path)"
    },
    "\n",
    sep = ""
  )
  invisible(x)
}

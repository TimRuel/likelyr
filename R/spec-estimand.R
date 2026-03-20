# ======================================================================
# Estimand Specification (v4.1)
# ======================================================================

#' Specify an Estimand ψ(θ)
#'
#' @description
#' Defines the estimand function ψ(θ) and its domain. This is
#' intentionally minimal — grid spacing, search intervals, confidence
#' levels, and inference tuning parameters belong to other specs.
#'
#' Optional bounds \code{psi_lower} and \code{psi_upper} describe the
#' geometric domain of ψ (e.g. [1/J, 1) for Simpson's index). These
#' are hard constraints on branch traversal, not likelihood cutoffs.
#'
#' \code{psi_closed} is a named logical vector indicating whether each
#' present bound is closed (reachable) or open (not reachable). Names
#' must be a subset of \code{c("lower", "upper")} and must correspond
#' to non-NULL bounds. A closed lower bound means the branch mode may
#' occur at \code{psi_lower}; an open lower bound means it cannot.
#'
#' The true value \code{psi_0} may be supplied directly when known
#' (e.g. in simulation studies). If provided, it overrides internal
#' computation of ψ₀ during calibration.
#'
#' @param psi_fn  Required. Function(param) → scalar ψ(θ).
#' @param psi_jac Optional. Function(param) → gradient vector ∇ψ(θ).
#' @param psi_lower Optional numeric scalar. Lower bound of ψ domain.
#' @param psi_upper Optional numeric scalar. Upper bound of ψ domain.
#' @param psi_closed Optional named logical vector. Indicates whether
#'   each present bound is closed (\code{TRUE}) or open (\code{FALSE}).
#'   Names must be a subset of \code{c("lower", "upper")} matching the
#'   non-NULL bounds. Omit entirely when no bounds are specified.
#' @param psi_0 Optional numeric scalar. True value of ψ, used in
#'   simulation studies. Overrides internal ψ₀ computation if supplied.
#' @param name Optional descriptive name.
#' @param ... Additional user metadata (stored but unused).
#'
#' @return An \code{estimand_spec} object.
#'
#' @examples
#' # No bounds
#' estimand_spec(psi_fn = function(param) sum(param))
#'
#' # Closed lower, open upper — Simpson's index case
#' estimand_spec(
#'   psi_fn    = function(param) sum(softmax(param)^2),
#'   psi_lower = 1/J,
#'   psi_upper = 1,
#'   psi_closed = c(lower = TRUE, upper = FALSE)
#' )
#'
#' # Closed lower only
#' estimand_spec(
#'   psi_fn    = function(param) exp(param),
#'   psi_lower = 0,
#'   psi_closed = c(lower = TRUE)
#' )
#'
#' @export
estimand_spec <- function(
  psi_fn,
  psi_jac = NULL,
  psi_lower = NULL,
  psi_upper = NULL,
  psi_closed = NULL,
  psi_0 = NULL,
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
    extra = list(...)
  )

  x <- new_estimand_spec(x)
  .validate_estimand_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_estimand_spec <- function(x) {
  # ψ(θ) -------------------------------------------------------------
  if (!is.function(x$psi_fn)) {
    stop("psi_fn must be a function(param).", call. = FALSE)
  }

  # ∇ψ(θ) ------------------------------------------------------------
  if (!is.null(x$psi_jac) && !is.function(x$psi_jac)) {
    stop("psi_jac must be NULL or a function(param).", call. = FALSE)
  }

  # ψ bounds ---------------------------------------------------------
  if (!is.null(x$psi_lower)) {
    if (!is.numeric(x$psi_lower) || length(x$psi_lower) != 1L) {
      stop("psi_lower must be NULL or a numeric scalar.", call. = FALSE)
    }
  }

  if (!is.null(x$psi_upper)) {
    if (!is.numeric(x$psi_upper) || length(x$psi_upper) != 1L) {
      stop("psi_upper must be NULL or a numeric scalar.", call. = FALSE)
    }
  }

  if (!is.null(x$psi_lower) && !is.null(x$psi_upper)) {
    if (x$psi_lower >= x$psi_upper) {
      stop("psi_lower must be strictly less than psi_upper.", call. = FALSE)
    }
  }

  # psi_closed -------------------------------------------------------
  if (!is.null(x$psi_closed)) {
    pc <- x$psi_closed

    if (!is.logical(pc) || is.null(names(pc))) {
      stop(
        "psi_closed must be a named logical vector with names in ",
        "c(\"lower\", \"upper\").",
        call. = FALSE
      )
    }

    invalid_names <- setdiff(names(pc), c("lower", "upper"))
    if (length(invalid_names) > 0) {
      stop(
        "psi_closed has unrecognised names: ",
        paste(invalid_names, collapse = ", "),
        ". Names must be a subset of c(\"lower\", \"upper\").",
        call. = FALSE
      )
    }

    if ("lower" %in% names(pc) && is.null(x$psi_lower)) {
      stop(
        "psi_closed has a \"lower\" entry but psi_lower is NULL.",
        call. = FALSE
      )
    }

    if ("upper" %in% names(pc) && is.null(x$psi_upper)) {
      stop(
        "psi_closed has an \"upper\" entry but psi_upper is NULL.",
        call. = FALSE
      )
    }

    if (!is.null(x$psi_lower) && !"lower" %in% names(pc)) {
      stop(
        "psi_lower is specified but psi_closed has no \"lower\" entry.",
        call. = FALSE
      )
    }

    if (!is.null(x$psi_upper) && !"upper" %in% names(pc)) {
      stop(
        "psi_upper is specified but psi_closed has no \"upper\" entry.",
        call. = FALSE
      )
    }
  } else {
    if (!is.null(x$psi_lower) || !is.null(x$psi_upper)) {
      stop(
        "psi_closed must be supplied when psi_lower or psi_upper is specified.",
        call. = FALSE
      )
    }
  }

  # ψ₀ override ------------------------------------------------------
  if (!is.null(x$psi_0)) {
    if (!is.numeric(x$psi_0) || length(x$psi_0) != 1L) {
      stop("psi_0 must be NULL or a numeric scalar.", call. = FALSE)
    }

    if (!is.null(x$psi_lower) && x$psi_0 < x$psi_lower) {
      stop("psi_0 is below psi_lower.", call. = FALSE)
    }

    if (!is.null(x$psi_upper) && x$psi_0 > x$psi_upper) {
      stop("psi_0 is above psi_upper.", call. = FALSE)
    }
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.estimand_spec <- function(x, ...) {
  cat("# Estimand Specification\n")
  cat("- Name:      ", x$name, "\n", sep = "")
  cat("- psi_fn():  ✔ function\n")

  if (!is.null(x$psi_jac)) {
    cat("- psi_jac(): ✔ function\n")
  }

  if (!is.null(x$psi_lower) || !is.null(x$psi_upper)) {
    lower_str <- if (!is.null(x$psi_lower)) {
      paste0(
        if (isTRUE(x$psi_closed["lower"])) "[" else "(",
        format(x$psi_lower)
      )
    } else {
      "(-Inf"
    }

    upper_str <- if (!is.null(x$psi_upper)) {
      paste0(
        format(x$psi_upper),
        if (isTRUE(x$psi_closed["upper"])) "]" else ")"
      )
    } else {
      "Inf)"
    }

    cat("- ψ domain:  ", lower_str, ", ", upper_str, "\n", sep = "")
  }

  if (!is.null(x$psi_0)) {
    cat("- ψ₀:        ", x$psi_0, "\n", sep = "")
  }

  invisible(x)
}

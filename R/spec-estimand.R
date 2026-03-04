# ======================================================================
# Estimand Specification (v4.0)
# ======================================================================

#' Specify an Estimand ψ(θ)
#'
#' @description
#' Defines the estimand function ψ(θ) and its domain. This is
#' intentionally minimal — grid spacing, search intervals, confidence
#' levels, and inference tuning parameters belong to other specs.
#'
#' Optional bounds `psi_lower` and `psi_upper` describe the geometric
#' domain of ψ (e.g. [1/J, 1] for Simpson's index). These are hard
#' constraints on branch traversal, not likelihood cutoffs.
#'
#' The true value `psi_0` may be supplied directly when known (e.g. in
#' simulation studies). If provided, it overrides internal computation
#' of ψ₀ during calibration.
#'
#' @param psi_fn  Required. Function(param) → scalar ψ(θ).
#' @param psi_jac Optional. Function(param) → gradient vector ∇ψ(θ).
#' @param psi_lower Optional numeric scalar. Lower bound of ψ domain.
#' @param psi_upper Optional numeric scalar. Upper bound of ψ domain.
#' @param psi_0 Optional numeric scalar. True value of ψ, used in
#'   simulation studies. Overrides internal ψ₀ computation if supplied.
#' @param name Optional descriptive name.
#' @param ... Additional user metadata (stored but unused).
#'
#' @return An `estimand_spec` object.
#' @export
estimand_spec <- function(
  psi_fn,
  psi_jac = NULL,
  psi_lower = NULL,
  psi_upper = NULL,
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
  # ψ(θ) ------------------------------------------------------------
  if (!is.function(x$psi_fn)) {
    stop("psi_fn must be a function(param).", call. = FALSE)
  }

  # ∇ψ(θ) -----------------------------------------------------------
  if (!is.null(x$psi_jac) && !is.function(x$psi_jac)) {
    stop("psi_jac must be NULL or a function(param).", call. = FALSE)
  }

  # ψ bounds --------------------------------------------------------
  if (!is.null(x$psi_lower)) {
    if (!is.numeric(x$psi_lower) || length(x$psi_lower) != 1) {
      stop("psi_lower must be NULL or a numeric scalar.", call. = FALSE)
    }
  }

  if (!is.null(x$psi_upper)) {
    if (!is.numeric(x$psi_upper) || length(x$psi_upper) != 1) {
      stop("psi_upper must be NULL or a numeric scalar.", call. = FALSE)
    }
  }

  if (!is.null(x$psi_lower) && !is.null(x$psi_upper)) {
    if (x$psi_lower >= x$psi_upper) {
      stop("psi_lower must be strictly less than psi_upper.", call. = FALSE)
    }
  }

  # ψ₀ override -----------------------------------------------------
  if (!is.null(x$psi_0)) {
    if (!is.numeric(x$psi_0) || length(x$psi_0) != 1) {
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
    cat(
      "- ψ domain:  [",
      x$psi_lower %||% "-Inf",
      ", ",
      x$psi_upper %||% "Inf",
      "]\n",
      sep = ""
    )
  }

  if (!is.null(x$psi_0)) {
    cat("- ψ₀:        ", x$psi_0, "\n", sep = "")
  }

  invisible(x)
}

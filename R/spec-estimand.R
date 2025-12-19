# ======================================================================
# Estimand Specification (v3.2)
# ======================================================================

#' Specify an Estimand ψ(θ) for Profile / Integrated Log-Likelihood
#'
#' @description
#' Defines the estimand function ψ(θ) and all metadata needed for
#' generating profile or integrated log-likelihood curves:
#'
#'   • `psi_fn(theta)` — mapping from θ → ψ
#'   • optional analytic Jacobian ∂ψ/∂θ
#'   • `search_interval_fn(data)` — allowable ψ-range
#'   • grid `increment` for ψ exploration
#'   • `confidence_levels` for LR confidence intervals
#'   • `cutoff_buffer` to ensure branch depth beyond the strictest CI
#'   • `uniroot_expand_factor` to widen search intervals for CI endpoints
#'
#' `cutoff_buffer` ensures that branch construction explores *past* the
#' deepest LR cutoff induced by the smallest CI level.
#'
#' `uniroot_expand_factor` helps `compute_ci()` succeed even when roots
#' lie just outside the discrete ψ-grid envelope.
#'
#' @param psi_fn Function(theta) → scalar ψ(θ). Required.
#' @param psi_jac Optional function(theta) → gradient ∇ψ(θ).
#' @param search_interval_fn Function(data) → c(lower, upper). Required.
#' @param increment Positive scalar giving ψ-grid spacing.
#' @param confidence_levels Numeric vector strictly inside (0, 1).
#'   These are **coverage levels** (not α). Example: `c(0.90, 0.95)`.
#' @param cutoff_buffer Nonnegative scalar. Branches are extended to at
#'   least `(1 + cutoff_buffer)` times the deepest LR cutoff.
#' @param uniroot_expand_factor Nonnegative scalar. Used by CI routines
#'   to expand root-finding intervals.
#' @param name Optional descriptive name.
#' @param ... Additional user metadata (stored but unused).
#'
#' @return An `estimand_spec` object.
#' @export
estimand_spec <- function(psi_fn,
                          psi_jac = NULL,
                          search_interval_fn,
                          increment,
                          confidence_levels,
                          cutoff_buffer = 0.1,
                          uniroot_expand_factor = 0.02,
                          name = NULL,
                          ...) {

  x <- list(
    name                  = name %||% "<estimand>",
    psi_fn                = psi_fn,
    psi_jac               = psi_jac,
    search_interval_fn    = search_interval_fn,
    increment             = increment,
    confidence_levels     = confidence_levels,
    cutoff_buffer         = cutoff_buffer,
    uniroot_expand_factor = uniroot_expand_factor,
    extra                 = list(...)
  )

  x <- new_estimand_spec(x)
  .validate_estimand_spec(x)
  x
}


# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

.validate_estimand_spec <- function(x) {

  # ψ(θ) ---------------------------------------------------------
  if (!is.function(x$psi_fn))
    stop("psi_fn must be a function(theta).", call. = FALSE)

  # ∂ψ/∂θ --------------------------------------------------------
  if (!is.null(x$psi_jac) && !is.function(x$psi_jac))
    stop("psi_jac must be NULL or a function(theta).", call. = FALSE)

  # search interval ---------------------------------------------
  if (!is.function(x$search_interval_fn))
    stop("search_interval_fn must be a function(data) → c(lower, upper).",
         call. = FALSE)

  # increment ----------------------------------------------------
  inc <- x$increment
  if (!is.numeric(inc) || length(inc) != 1 || inc <= 0)
    stop("increment must be a positive numeric scalar.", call. = FALSE)

  # confidence levels -------------------------------------------
  cl <- x$confidence_levels
  if (!is.numeric(cl))
    stop("confidence_levels must be numeric.", call. = FALSE)
  if (any(cl <= 0 | cl >= 1))
    stop("confidence_levels must lie strictly between 0 and 1.", call. = FALSE)
  if (anyDuplicated(cl))
    stop("confidence_levels must not contain duplicates.", call. = FALSE)

  # cutoff buffer ------------------------------------------------
  cb <- x$cutoff_buffer
  if (!is.numeric(cb) || length(cb) != 1 || cb < 0)
    stop("cutoff_buffer must be a non-negative numeric scalar.", call. = FALSE)

  # uniroot expand ----------------------------------------------
  uf <- x$uniroot_expand_factor
  if (!is.numeric(uf) || length(uf) != 1 || uf < 0)
    stop("uniroot_expand_factor must be a non-negative numeric scalar.",
         call. = FALSE)

  invisible(x)
}


# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.estimand_spec <- function(x, ...) {
  cat("# Estimand Specification\n")
  cat("- Name:                  ", x$name, "\n", sep = "")
  cat("- Increment:             ", x$increment, "\n", sep = "")
  cat("- CI levels:             ", paste(format(x$confidence_levels), collapse = ", "), "\n", sep = "")
  cat("- Cutoff buffer:         ", x$cutoff_buffer, "\n", sep = "")
  cat("- uniroot expand factor: ", x$uniroot_expand_factor, "\n", sep = "")
  invisible(x)
}

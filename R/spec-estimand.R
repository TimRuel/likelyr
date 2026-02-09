# ======================================================================
# Estimand Specification (v3.4) — bounded ψ support
# ======================================================================

#' Specify an Estimand ψ(θ) for Profile / Integrated Log-Likelihood
#'
#' @description
#' Defines the estimand function ψ(θ) and all metadata needed for
#' generating profile or integrated log-likelihood curves.
#'
#' Optionally, the true value ψ₀ may be supplied directly via `psi_0`.
#' If provided, this value will override internal computation of ψ₀
#' during calibration.
#'
#' Optional bounds `psi_lower` and `psi_upper` may be supplied to
#' describe the *geometric domain* of ψ. These bounds are treated as
#' hard constraints on branch traversal (not likelihood cutoffs).
#'
#' @param psi_fn Function(param) → scalar ψ(θ). Required.
#' @param psi_jac Optional function(param) → gradient ∇ψ(θ).
#' @param search_interval_fn Function(param_mle, data) → c(lower, upper). Required.
#' @param increment Positive scalar giving ψ-grid spacing.
#' @param confidence_levels Numeric vector strictly inside (0, 1).
#' @param gamma Numeric scalar in (0,1] tempering the conservativeness of
#'   integrated likelihood branch cutoffs.
#' @param cutoff_buffer Nonnegative scalar.
#' @param uniroot_expand_factor Nonnegative scalar.
#' @param psi_lower Optional numeric scalar giving lower bound of ψ.
#' @param psi_upper Optional numeric scalar giving upper bound of ψ.
#' @param psi_0 Optional numeric scalar giving the true value of ψ.
#'   If supplied, overrides internal ψ₀ computation during calibration.
#' @param name Optional descriptive name.
#' @param ... Additional user metadata (stored but unused).
#'
#' @return An `estimand_spec` object.
#' @export
estimand_spec <- function(
  psi_fn,
  psi_jac = NULL,
  search_interval_fn,
  increment,
  confidence_levels,
  gamma = 0.5,
  cutoff_buffer = 0.1,
  uniroot_expand_factor = 0.02,
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
    psi_0 = psi_0,
    psi_lower = psi_lower,
    psi_upper = psi_upper,
    search_interval_fn = search_interval_fn,
    increment = increment,
    confidence_levels = confidence_levels,
    gamma = gamma,
    cutoff_buffer = cutoff_buffer,
    uniroot_expand_factor = uniroot_expand_factor,
    extra = list(...)
  )

  x <- new_estimand_spec(x)
  .validate_estimand_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' Validate estimand specification object
#'
#' @keywords internal
#' @noRd
.validate_estimand_spec <- function(x) {
  # ψ(θ) ---------------------------------------------------------
  if (!is.function(x$psi_fn)) {
    stop("psi_fn must be a function(param).", call. = FALSE)
  }

  # ∂ψ/∂θ --------------------------------------------------------
  if (!is.null(x$psi_jac) && !is.function(x$psi_jac)) {
    stop("psi_jac must be NULL or a function(param).", call. = FALSE)
  }

  # ψ₀ override --------------------------------------------------
  if (!is.null(x$psi_0)) {
    if (!is.numeric(x$psi_0) || length(x$psi_0) != 1) {
      stop("psi_0 must be NULL or a numeric scalar.", call. = FALSE)
    }
  }

  # ψ bounds -----------------------------------------------------
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
    if (x$psi_lower > x$psi_upper) {
      stop("psi_lower must be <= psi_upper.", call. = FALSE)
    }
  }

  # search interval ---------------------------------------------
  if (!is.function(x$search_interval_fn)) {
    stop(
      "search_interval_fn must be a function(param_mle, data) → c(lower, upper).",
      call. = FALSE
    )
  }

  # increment ----------------------------------------------------
  inc <- x$increment
  if (!is.numeric(inc) || length(inc) != 1 || inc <= 0) {
    stop("increment must be a positive numeric scalar.", call. = FALSE)
  }

  # confidence levels -------------------------------------------
  cl <- x$confidence_levels
  if (!is.numeric(cl)) {
    stop("confidence_levels must be numeric.", call. = FALSE)
  }
  if (any(cl <= 0 | cl >= 1)) {
    stop("confidence_levels must lie strictly between 0 and 1.", call. = FALSE)
  }
  if (anyDuplicated(cl)) {
    stop("confidence_levels must not contain duplicates.", call. = FALSE)
  }

  # gamma --------------------------------------------------------
  gamma <- x$gamma
  if (!is.numeric(gamma) || gamma <= 0 || gamma > 1) {
    stop("`gamma` must be in (0, 1].", call. = FALSE)
  }

  # cutoff buffer ------------------------------------------------
  cb <- x$cutoff_buffer
  if (!is.numeric(cb) || length(cb) != 1 || cb < 0) {
    stop("cutoff_buffer must be a non-negative numeric scalar.", call. = FALSE)
  }

  # uniroot expand ----------------------------------------------
  uf <- x$uniroot_expand_factor
  if (!is.numeric(uf) || length(uf) != 1 || uf < 0) {
    stop(
      "uniroot_expand_factor must be a non-negative numeric scalar.",
      call. = FALSE
    )
  }

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
  cat(
    "- CI levels:             ",
    paste(format(x$confidence_levels), collapse = ", "),
    "\n",
    sep = ""
  )
  cat("- Gamma:                 ", x$gamma, "\n", sep = "")
  cat("- Cutoff buffer:         ", x$cutoff_buffer, "\n", sep = "")
  cat("- uniroot expand factor: ", x$uniroot_expand_factor, "\n", sep = "")

  if (!is.null(x$psi_lower) || !is.null(x$psi_upper)) {
    cat(
      "- ψ bounds:              [",
      x$psi_lower %||% "-Inf",
      ", ",
      x$psi_upper %||% "Inf",
      "]\n",
      sep = ""
    )
  }

  cat("- psi_0:                 ", x$psi_0, "\n", sep = "")
  invisible(x)
}

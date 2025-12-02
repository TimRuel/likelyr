# ======================================================================
# Estimand Specification (v3.0)
# ======================================================================

#' Specify an Estimand ψ(θ) for Profile / Integrated Likelihood
#'
#' @description
#' Defines:
#'   • ψ(θ)       — the estimand of interest
#'   • ∂ψ/∂θ      — optional analytic jacobian
#'   • search_interval_fn(data) → c(a, b)
#'   • increment  — step size for ψ grid during branch exploration
#'   • confidence_levels — confidence levels (0 < c < 1)
#'
#' The interval function is a *function of the data only*. If users want
#' θ_MLE inside it, they must recompute θ_MLE internally from the data.
#'
#' @param psi_fn             function(theta, data) → numeric scalar ψ
#' @param psi_jac            Optional function(theta, data) → gradient vector
#' @param search_interval_fn function(data) → numeric vector length 2
#' @param increment          Positive numeric scalar
#' @param confidence_levels  Numeric vector in (0, 1)
#' @param name               Optional descriptive name
#' @param ...                Extra fields
#'
#' @return An `estimand_spec` object
#' @export
estimand_spec <- function(psi_fn,
                          psi_jac = NULL,
                          search_interval_fn,
                          increment,
                          confidence_levels,
                          name = NULL,
                          ...) {

  x <- list(
    name               = name %||% "<estimand>",
    psi_fn             = psi_fn,
    psi_jac            = psi_jac,
    search_interval_fn = search_interval_fn,
    increment          = increment,
    confidence_levels  = confidence_levels,
    extra              = list(...)
  )

  class(x) <- "estimand_spec"
  .validate_estimand_spec(x)
  x
}

# ----------------------------------------------------------------------
# INTERNAL VALIDATOR
# ----------------------------------------------------------------------

.validate_estimand_spec <- function(x) {

  # ψ(θ)
  if (!is.function(x$psi_fn))
    stop("psi_fn must be a function(theta, data).", call. = FALSE)

  # jacobian
  if (!is.null(x$psi_jac) && !is.function(x$psi_jac))
    stop("psi_jac must be NULL or a function(theta, data).", call. = FALSE)

  # interval: must be a *function(data)*, cannot validate contents here
  if (!is.function(x$search_interval_fn))
    stop("search_interval_fn must be a function(data).", call. = FALSE)

  # increment
  inc <- x$increment
  if (!is.numeric(inc) || length(inc) != 1 || inc <= 0)
    stop("increment must be a positive numeric scalar.", call. = FALSE)

  # confidence levels
  cl <- x$confidence_levels
  if (!is.numeric(cl))
    stop("confidence_levels must be numeric.", call. = FALSE)

  if (any(cl <= 0 | cl >= 1))
    stop("All confidence_levels must lie strictly between 0 and 1.", call. = FALSE)

  if (anyDuplicated(cl))
    stop("confidence_levels must not contain duplicates.", call. = FALSE)

  invisible(x)
}

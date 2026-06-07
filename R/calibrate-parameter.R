# ======================================================================
# Parameter Calibration (v1.4) — equality + inequality constraints
#
# This calibration step:
#   • Computes param_MLE using parameter$param_mle_fn(data)
#   • Resolves "auto" bounds from param_mle when requested
#   • Ensures the MLE respects parameter dimension & constraints
#   • Validates equality and inequality constraints at the MLE
#   • Stores param_MLE inside the parameter_spec object
#
# NOTE:
#   • param_mle_fn now lives on parameter_spec, not likelihood_spec.
#     This function no longer requires a likelihood argument.
#   • Equality constraints are treated as *structural feasibility*
#     conditions and must be satisfied (up to tolerance) by param_MLE.
#
# AUTO BOUNDS:
#   Pass param_lower = "auto" and/or param_upper = "auto" in
#   parameter_spec() to request bounds computed from param_mle.
#   The radius formula is:
#
#     radius <- max(abs(param_mle)) * 3 + 5
#
#   which guarantees param_mle is at most 1/4 of the way to the bound
#   in the largest component, while preventing auglag from diverging
#   to ±Inf in unbounded spaces like the multinomial logit.
#
# ATTRIBUTE PRESERVATION:
#   param_mle_fn may attach named attributes to param_mle (e.g.
#   Sigma_hat, fix_Sigma) for use by likelihood calibration. These
#   are preserved across the as.numeric() coercion by saving and
#   restoring the attribute list, so downstream functions that look
#   for them via attr() will find them on the stored param_mle.
# ======================================================================

#' Calibrate Parameter Component
#'
#' @description
#' Computes the analytic MLE for θ using `parameter$param_mle_fn(data)`
#' and stores it inside the parameter_spec.
#'
#' If `param_lower` or `param_upper` on the spec is the string
#' \code{"auto"}, bounds are computed from \code{param_mle} after
#' the MLE is known and stored back onto the spec.
#'
#' This function is called internally by `calibrate()`.
#'
#' @param parameter A `parameter_spec` object.
#' @param data      The user data passed to calibrate().
#'
#' @return The SAME parameter_spec object with added/updated fields:
#'   • `$param_mle`
#'   • `$param_lower`  (resolved from "auto" if requested)
#'   • `$param_upper`  (resolved from "auto" if requested)
#'
#' @keywords internal
calibrate_parameter <- function(parameter, data) {
  stopifnot(inherits(parameter, "parameter_spec"))

  param_dim <- parameter$param_dim

  # -------------------------------------------------------------------
  # 1. Compute analytic MLE via parameter's initializer
  # -------------------------------------------------------------------
  param_mle <- parameter$param_mle_fn(data)

  if (!is.numeric(param_mle) || length(param_mle) != param_dim) {
    stop(
      sprintf(
        "param_mle_fn(data) returned a vector of length %d but param_dim = %d.",
        length(param_mle),
        param_dim
      ),
      call. = FALSE
    )
  }

  # Coerce to plain numeric while preserving any attributes attached by
  # param_mle_fn (e.g. Sigma_hat, fix_Sigma for random effects models).
  # as.numeric() strips attributes, so we save and restore them.
  saved_attrs <- attributes(param_mle)
  param_mle <- as.numeric(param_mle)
  attributes(param_mle) <- saved_attrs

  # -------------------------------------------------------------------
  # 2. Resolve "auto" bounds now that param_mle is known.
  #    radius guarantees param_mle is strictly interior with generous
  #    room for the sampler to explore the constraint surface.
  # -------------------------------------------------------------------
  if (
    identical(parameter$param_lower, "auto") ||
      identical(parameter$param_upper, "auto")
  ) {
    radius <- max(abs(param_mle)) * 3 + 5
    if (identical(parameter$param_lower, "auto")) {
      parameter$param_lower <- rep(-radius, param_dim)
    }
    if (identical(parameter$param_upper, "auto")) {
      parameter$param_upper <- rep(radius, param_dim)
    }
  }

  # -------------------------------------------------------------------
  # 3. Check box constraints (if present)
  # -------------------------------------------------------------------
  if (
    !is.null(parameter$param_lower) &&
      any(param_mle < parameter$param_lower)
  ) {
    stop("Computed param_mle violates param_lower constraints.", call. = FALSE)
  }

  if (
    !is.null(parameter$param_upper) &&
      any(param_mle > parameter$param_upper)
  ) {
    stop("Computed param_mle violates param_upper constraints.", call. = FALSE)
  }

  # -------------------------------------------------------------------
  # 4. Check equality constraints (if present)
  # -------------------------------------------------------------------
  if (!is.null(parameter$eq)) {
    h_val <- parameter$eq(param_mle)

    if (!is.numeric(h_val)) {
      stop("eq(param_mle) must return numeric vector.", call. = FALSE)
    }

    if (any(abs(h_val) > 1e-8)) {
      stop(
        "Computed param_mle violates equality constraints: eq(param) = 0.",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------------------
  # 5. Check inequality constraints (if present)
  # -------------------------------------------------------------------
  if (!is.null(parameter$ineq)) {
    g_val <- parameter$ineq(param_mle)

    if (!is.numeric(g_val)) {
      stop("ineq(param_mle) must return numeric vector.", call. = FALSE)
    }

    if (any(g_val > 0)) {
      stop(
        "Computed param_mle violates inequality constraint: ineq(param) <= 0.",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------------------
  # 6. Store MLE inside the parameter specification
  # -------------------------------------------------------------------
  parameter$param_mle <- param_mle

  parameter
}

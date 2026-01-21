# ======================================================================
# Parameter Calibration (v1.1)
#
# This calibration step:
#   • Computes param_MLE using likelihood$param_mle_fn(data)
#   • Ensures the MLE respects parameter dimension & constraints
#   • Stores param_MLE inside the parameter_spec object
#
# NOTE:
#   • Requires both parameter_spec AND likelihood_spec, because the
#     MLE comes from the likelihood's analytic initializer.
# ======================================================================

#' Calibrate Parameter Component
#'
#' @description
#' Computes the analytic MLE for θ using the likelihood's
#' `param_mle_fn(data)` and stores it inside the parameter_spec.
#'
#' This function is called internally by `calibrate()`.
#'
#' @param parameter  A `parameter_spec` object.
#' @param likelihood A `likelihood_spec` object (provides param_mle_fn).
#' @param data       The user data passed to calibrate().
#'
#' @return The SAME parameter_spec object with added field:
#'   • `$param_mle`
#'
#' @keywords internal
calibrate_parameter <- function(parameter, likelihood, data) {
  stopifnot(
    inherits(parameter, "parameter_spec"),
    inherits(likelihood, "likelihood_spec")
  )

  J <- parameter$param_dim

  # -------------------------------------------------------------------
  # 1. Compute analytic MLE via likelihood's initializer
  # -------------------------------------------------------------------
  param_mle <- likelihood$param_mle_fn(data)

  if (!is.numeric(param_mle) || length(param_mle) != J) {
    stop(
      sprintf(
        "param_mle_fn(data) returned a vector of length %d but param_dim = %d.",
        length(param_mle),
        J
      ),
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # 2. Check box constraints (if present)
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
  # 3. Check inequality constraints (if present)
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
  # 4. Store MLE inside the parameter specification
  # -------------------------------------------------------------------
  parameter$param_mle <- param_mle

  parameter
}

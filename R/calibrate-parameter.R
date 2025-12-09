# ======================================================================
# Parameter Calibration (v1.1)
#
# This calibration step:
#   • Computes theta_MLE using likelihood$theta_mle_fn(data)
#   • Ensures the MLE respects parameter dimension & constraints
#   • Stores theta_MLE inside the parameter_spec object
#
# NOTE:
#   • Requires both parameter_spec AND likelihood_spec, because the
#     MLE comes from the likelihood's analytic initializer.
# ======================================================================

#' Calibrate Parameter Component
#'
#' @description
#' Computes the analytic MLE for θ using the likelihood's
#' `theta_mle_fn(data)` and stores it inside the parameter_spec.
#'
#' This function is called internally by `calibrate()`.
#'
#' @param parameter  A `parameter_spec` object.
#' @param likelihood A `likelihood_spec` object (provides theta_mle_fn).
#' @param data       The user data passed to calibrate().
#'
#' @return The SAME parameter_spec object with added field:
#'   • `$theta_mle`
#'
#' @keywords internal
#' @export
calibrate_parameter <- function(parameter, likelihood, data) {

  stopifnot(
    inherits(parameter,  "parameter_spec"),
    inherits(likelihood, "likelihood_spec")
  )

  J <- parameter$theta_dim

  # -------------------------------------------------------------------
  # 1. Compute analytic MLE via likelihood's initializer
  # -------------------------------------------------------------------
  theta_mle <- likelihood$theta_mle_fn(data)

  if (!is.numeric(theta_mle) || length(theta_mle) != J) {
    stop(
      sprintf(
        "theta_mle_fn(data) returned a vector of length %d but theta_dim = %d.",
        length(theta_mle), J
      ),
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # 2. Check box constraints (if present)
  # -------------------------------------------------------------------
  if (!is.null(parameter$theta_lower) &&
      any(theta_mle < parameter$theta_lower)) {
    stop("Computed theta_mle violates theta_lower constraints.", call. = FALSE)
  }

  if (!is.null(parameter$theta_upper) &&
      any(theta_mle > parameter$theta_upper)) {
    stop("Computed theta_mle violates theta_upper constraints.", call. = FALSE)
  }

  # -------------------------------------------------------------------
  # 3. Check inequality constraints (if present)
  # -------------------------------------------------------------------
  if (!is.null(parameter$ineq)) {

    g_val <- parameter$ineq(theta_mle)

    if (!is.numeric(g_val))
      stop("ineq(theta_mle) must return numeric vector.", call. = FALSE)

    if (any(g_val > 0)) {
      stop("Computed theta_mle violates inequality constraint: ineq(theta) <= 0.",
           call. = FALSE)
    }
  }

  # -------------------------------------------------------------------
  # 4. Store MLE inside the parameter specification
  # -------------------------------------------------------------------
  parameter$theta_mle <- theta_mle

  parameter
}

# ======================================================================
# Likelihood Calibration
# ======================================================================

#' Calibrate Likelihood Component
#'
#' @description
#' Binds data to the likelihood functions and computes theta_MLE.
#'
#' @param likelihood A likelihood_spec object.
#' @param data User data.
#'
#' @return The SAME likelihood_spec object, enriched with:
#'         • $loglik     — data-bound closure
#'         • $theta_mle  — analytic MLE computed on `data`
#' @export
calibrate_likelihood <- function(likelihood, data) {

  stopifnot(inherits(likelihood, "likelihood_spec"))

  # -------------------------------------------------------------
  # 1. Bind data to loglik()
  # -------------------------------------------------------------
  orig_loglik <- likelihood$loglik
  likelihood$loglik <- function(theta) orig_loglik(theta, data)

  # -------------------------------------------------------------
  # 2. Compute analytic MLE from user-supplied updater
  # -------------------------------------------------------------
  theta_mle <- likelihood$theta_mle_fn(data)

  if (length(theta_mle) != likelihood$theta_dim) {
    stop(
      sprintf(
        "theta_mle_fn(data) returned length %d but theta_dim = %d.",
        length(theta_mle), likelihood$theta_dim
      ),
      call. = FALSE
    )
  }

  likelihood$theta_mle <- theta_mle

  likelihood
}

# ======================================================================
# Nuisance Specification (v3.0)
# ======================================================================

#' Specify Nuisance Components for Integrated Likelihood
#'
#' @description
#' Defines the nuisance-parameter contribution to the *expected*
#' log-likelihood under the distribution of Y indexed by ω̂:
#'
#'   E[ log p(Y | theta); ω̂  ]
#'
#' used in integrated likelihood calculations. This includes:
#'   • expected log-likelihood E_loglik(theta, omega_hat, data)
#'   • optional gradient wrt theta
#'
#' Omega-hat sampling and initialization are handled separately
#' (see eval-omega-hat.R).
#'
#' @param E_loglik      Function(theta, omega_hat, data) → numeric.
#' @param E_loglik_grad Optional gradient wrt theta:
#'                        function(theta, omega_hat, data) → vector.
#' @param name          Optional name for display.
#' @param ...           Additional fields stored under `$extra`.
#'
#' @return A `nuisance_spec` object.
#' @export
nuisance_spec <- function(E_loglik,
                          E_loglik_grad = NULL,
                          name = NULL,
                          ...) {

  x <- list(
    name          = name %||% "<nuisance>",
    E_loglik      = E_loglik,
    E_loglik_grad = E_loglik_grad,
    extra         = list(...)
  )

  class(x) <- "nuisance_spec"
  .validate_nuisance_spec(x)
  x
}

# ----------------------------------------------------------------------
# INTERNAL VALIDATOR
# ----------------------------------------------------------------------

.validate_nuisance_spec <- function(x) {

  # E_loglik (required)
  if (!is.function(x$E_loglik))
    stop("E_loglik must be a function(theta, omega_hat, data).",
         call. = FALSE)

  # E_loglik_grad (optional)
  if (!is.null(x$E_loglik_grad) && !is.function(x$E_loglik_grad))
    stop("E_loglik_grad must be NULL or a function(theta, omega_hat, data).",
         call. = FALSE)

  invisible(x)
}

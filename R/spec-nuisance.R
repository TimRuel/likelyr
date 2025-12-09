# ======================================================================
# Nuisance Specification (v3.1)
# ======================================================================

#' Specify Nuisance Components for Integrated Likelihood
#'
#' @description
#' Defines the nuisance contribution to the *expected* log-likelihood
#' used in Monte Carlo Integrated Likelihood:
#'
#' \deqn{
#'   E_{\omegâ}[ \log p(Y \mid \theta) ].
#' }
#'
#' The nuisance specification provides:
#'
#' • `E_loglik(theta, omega_hat, data)` — expected log-likelihood
#' • `E_loglik_grad(theta, omega_hat, data)` — optional gradient wrt θ
#'
#' These functions are used *only* for integrated likelihood calculations.
#' They play no role in profile likelihood.
#'
#' @param E_loglik
#'   Required. Function `(theta, omega_hat, data) -> numeric`
#'   giving the expected log-likelihood.
#'
#' @param E_loglik_grad
#'   Optional gradient function `(theta, omega_hat, data) -> numeric vector`.
#'   If supplied, must return a vector of length `theta_dim`.
#'
#' @param name Optional descriptive name for the nuisance component.
#' @param ... Additional fields stored but unused.
#'
#' @return
#' A `nuisance_spec` object with classes:
#' `c("nuisance_spec", "likelyr")`.
#'
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

  x <- new_nuisance_spec(x)
  .validate_nuisance_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

.validate_nuisance_spec <- function(x) {

  # ---- Expected loglik must be supplied ----
  if (!is.function(x$E_loglik)) {
    stop(
      "E_loglik must be a function(theta, omega_hat, data).",
      call. = FALSE
    )
  }

  # ---- Gradient must be a function if present ----
  if (!is.null(x$E_loglik_grad) && !is.function(x$E_loglik_grad)) {
    stop(
      "E_loglik_grad must be NULL or a function(theta, omega_hat, data).",
      call. = FALSE
    )
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.nuisance_spec <- function(x, ...) {
  cat("# Nuisance Specification\n")
  cat("- Name: ", x$name, "\n", sep = "")
  cat("- Expected log-likelihood:   ", if (!is.null(x$E_loglik))      "present" else "missing", "\n", sep = "")
  cat("- Expected loglik gradient:  ", if (!is.null(x$E_loglik_grad)) "present" else "absent", "\n", sep = "")
  invisible(x)
}

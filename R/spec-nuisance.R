# ======================================================================
# Nuisance Specification (v5.1)
# ======================================================================

#' Specify Nuisance Components for Integrated Log-Likelihood
#'
#' @description
#' Defines the nuisance contribution to the *expected* log-likelihood
#' used in Monte Carlo Integrated Log-Likelihood:
#'
#' \deqn{
#'   E_{\omegâ}[ \log p(Y \mid \param) ].
#' }
#'
#' The nuisance specification provides:
#'
#' • `E_loglik(param, omega_hat, data)` — expected log-likelihood
#' • `E_loglik_grad(param, omega_hat, data)` — optional gradient wrt θ
#'
#' These functions are used *only* for integrated log-likelihood calculations.
#'
#' @param E_loglik
#'   Required. Function `(param, omega_hat, data) -> numeric`
#'   giving the expected log-likelihood.
#'
#' @param E_loglik_grad
#'   Optional gradient function `(param, omega_hat, data) -> numeric vector`.
#'
#' @param name Optional descriptive name for the nuisance component.
#' @param ... Additional fields stored but unused.
#'
#' @return
#' A `nuisance_spec` object with classes:
#' `c("nuisance_spec", "likelyr")`.
#'
#' @export
nuisance_spec <- function(E_loglik, E_loglik_grad = NULL, name = NULL, ...) {
  x <- list(
    name = name %||% "<nuisance>",
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad,
    extra = list(...)
  )

  x <- new_nuisance_spec(x)
  .validate_nuisance_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' Validate Nuisance Specification
#'
#' @description
#' Internal validator for `nuisance_spec` objects. Ensures that all
#' required nuisance-related functions are present and correctly typed
#' before downstream likelihood procedures are run.
#'
#' @details
#' The following checks are performed:
#'
#' \itemize{
#'   \item `E_loglik` must be a function with signature
#'         `(param, omega_hat, data)`.
#'   \item `E_loglik_grad`, if supplied, must also be a function with
#'         the same signature.
#' }
#'
#' @param x A list representing a `nuisance_spec` object.
#'
#' @return Invisibly returns `x` if validation succeeds.
#'
#' @keywords internal
#' @noRd
.validate_nuisance_spec <- function(x) {
  # ---- E_loglik ----
  if (!is.function(x$E_loglik)) {
    stop(
      "E_loglik must be a function(param, omega_hat, data).",
      call. = FALSE
    )
  }

  fmls <- names(formals(x$E_loglik))

  if (!"data" %in% fmls) {
    stop(
      "E_loglik must include a `data` argument. ",
      "Signature must be: (param, omega_hat, data).",
      call. = FALSE
    )
  }

  # ---- Gradient ----
  if (!is.null(x$E_loglik_grad)) {
    if (!is.function(x$E_loglik_grad)) {
      stop(
        "E_loglik_grad must be NULL or a function(param, omega_hat, data).",
        call. = FALSE
      )
    }

    fmls_g <- names(formals(x$E_loglik_grad))

    if (!"data" %in% fmls_g) {
      stop(
        "E_loglik_grad must include a `data` argument.",
        call. = FALSE
      )
    }
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' Print Method for `nuisance_spec`
#'
#' @description
#' Displays a concise summary of the nuisance specification.
#'
#' @param x A `nuisance_spec` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.nuisance_spec <- function(x, ...) {
  cat("# Nuisance Specification\n")
  cat("- Name: ", x$name, "\n", sep = "")
  cat("- Expected log-likelihood:  present\n")
  cat(
    "- Expected loglik gradient:  ",
    if (!is.null(x$E_loglik_grad)) "present" else "absent",
    "\n",
    sep = ""
  )
  invisible(x)
}

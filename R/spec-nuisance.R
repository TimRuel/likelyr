# ======================================================================
# Nuisance Specification (v3.0)
# ======================================================================

#' Specify Nuisance Components for Integrated Likelihood
#'
#' @description
#' Defines the nuisance-parameter contribution to the *expected*
#' log-likelihood under the distribution of Y indexed by ω̂:
#'
#'   E[ log p(Y | theta); ω̂ ]
#'
#' Required for integrated likelihood calculations. Includes:
#'   • expected log-likelihood E_loglik(theta, omega_hat, data)
#'   • optional gradient wrt theta
#'
#' @param E_loglik      Function(theta, omega_hat, data) → numeric
#' @param E_loglik_grad Optional gradient function(theta, omega_hat, data)
#' @param name          Optional descriptive name
#' @param ...           Extra stored fields
#'
#' @return A `nuisance_spec` object with classes:
#'         c("nuisance_spec", "likelyr_spec")
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

  # unified class constructor
  x <- new_nuisance_spec(x)

  # validate and return
  .validate_nuisance_spec(x)
  x
}

# ----------------------------------------------------------------------
# INTERNAL VALIDATOR
# ----------------------------------------------------------------------

.validate_nuisance_spec <- function(x) {

  # E_loglik required
  if (!is.function(x$E_loglik))
    stop("E_loglik must be a function(theta, omega_hat, data).",
         call. = FALSE)

  # optional gradient
  if (!is.null(x$E_loglik_grad) && !is.function(x$E_loglik_grad))
    stop("E_loglik_grad must be NULL or a function(theta, omega_hat, data).",
         call. = FALSE)

  invisible(x)
}

# ------------------------------------------------------
# PRINT METHOD
# ------------------------------------------------------

#' @export
print.nuisance_spec <- function(x, ...) {
  cat("# Nuisance Specification\n")
  cat("- Name: ", x$name, "\n", sep = "")
  invisible(x)
}


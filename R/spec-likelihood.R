# ======================================================================
# Likelihood Specification (v7.0)
#
# Combines the former likelihood_spec and objective_spec into a single
# spec. loglik is required for all analyses. E_loglik and E_loglik_grad
# are optional at construction time but required by integrate().
# ======================================================================

#' Specify a Parametric Likelihood Model
#'
#' @description
#' Defines the likelihood component of the model, including the
#' observed data log-likelihood and optionally the expected
#' log-likelihood objective used by the integrated likelihood method.
#'
#' \code{loglik} is required for all analyses (profile and integrated).
#'
#' \code{E_loglik} and \code{E_loglik_grad} are optional at
#' construction time but required when calling \code{integrate()}.
#' A clear error is raised at that point if they are absent.
#'
#' @param loglik Required. Function \code{(param, data) -> numeric}
#'   giving the observed data log-likelihood.
#' @param E_loglik Optional. Function
#'   \code{(param, omega_hat, data) -> numeric} giving the expected
#'   log-likelihood objective maximized by the inner solver during
#'   branch evaluation:
#'   \deqn{E_{\hat\omega}[ \log p(Y \mid \theta) ]}
#'   The \code{data} argument must be present in the signature; it is
#'   bound at calibration time. Required for \code{integrate()}.
#' @param E_loglik_grad Optional. Function
#'   \code{(param, omega_hat, data) -> numeric vector} giving the
#'   gradient of \code{E_loglik} with respect to \code{param}. When
#'   supplied, the inner solver uses analytic gradients rather than
#'   finite differences. Must include a \code{data} argument.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{likelihood_spec} object.
#' @export
likelihood_spec <- function(
  loglik,
  E_loglik = NULL,
  E_loglik_grad = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<likelihood>",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad,
    extra = list(...)
  )

  x <- new_likelihood_spec(x)
  .validate_likelihood_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_likelihood_spec <- function(x) {
  # loglik — required -----------------------------------------------
  if (!is.function(x$loglik)) {
    stop("loglik must be a function(param, data).", call. = FALSE)
  }

  # E_loglik — optional at construction, required for integrate() ---
  if (!is.null(x$E_loglik)) {
    if (!is.function(x$E_loglik)) {
      stop(
        "E_loglik must be a function(param, omega_hat, data).",
        call. = FALSE
      )
    }

    if (!"data" %in% names(formals(x$E_loglik))) {
      stop(
        "E_loglik must include a `data` argument. ",
        "Signature must be: (param, omega_hat, data).",
        call. = FALSE
      )
    }
  }

  # E_loglik_grad — optional ----------------------------------------
  if (!is.null(x$E_loglik_grad)) {
    if (!is.function(x$E_loglik_grad)) {
      stop(
        "E_loglik_grad must be NULL or a function(param, omega_hat, data).",
        call. = FALSE
      )
    }

    if (!"data" %in% names(formals(x$E_loglik_grad))) {
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

#' @export
print.likelihood_spec <- function(x, ...) {
  cat("# Likelihood Specification\n")
  cat("- Name:             ", x$name, "\n", sep = "")
  cat("- loglik():         ✔ function\n")
  cat(
    "- E_loglik():       ",
    if (!is.null(x$E_loglik)) "✔ function" else "absent (profile only)",
    "\n",
    sep = ""
  )
  cat(
    "- E_loglik_grad():  ",
    if (!is.null(x$E_loglik_grad)) {
      "✔ function"
    } else {
      "absent (finite differences)"
    },
    "\n",
    sep = ""
  )
  invisible(x)
}

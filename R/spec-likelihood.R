# ======================================================================
# Likelihood Specification (v7.3)
#
# Combines the former likelihood_spec and objective_spec into a single
# spec. loglik is required for all analyses. E_loglik and E_loglik_grad
# are optional at construction time but required by integrate().
#
# needs_param_mle signals that E_loglik and E_loglik_grad require the
# MLE parameter vector as an additional argument. When TRUE,
# calibrate_likelihood() binds param_mle alongside data, reducing the
# signatures to function(param, omega_hat).
#
# omega_hat_from_param_mle is an optional converter used by
# .generate_profile() to obtain the appropriate omega_hat reference
# for the profile evaluator from param_mle. When NULL, param_mle is
# used directly as omega_hat (the default for applications where they
# share the same space). Data is bound into it during
# calibrate_likelihood(), reducing its signature from
# function(param_mle, data) to function(param_mle).
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
#' @param needs_param_mle Logical. When \code{TRUE},
#'   \code{calibrate_likelihood()} binds \code{param_mle} into
#'   \code{E_loglik} and \code{E_loglik_grad} alongside \code{data},
#'   reducing their signatures from
#'   \code{function(param, omega_hat, data, param_mle)} to
#'   \code{function(param, omega_hat)}. Use when the expected
#'   log-likelihood requires the MLE as a reference — e.g. for a
#'   rank-1 adjustment of \code{B_mle} to construct an
#'   observation-specific \code{theta_hat}. Default: \code{FALSE}.
#' @param omega_hat_from_param_mle Optional. Function
#'   \code{(param_mle, data) -> numeric vector} converting
#'   \code{param_mle} to the appropriate \code{omega_hat} reference
#'   for the profile evaluator. When \code{NULL}, \code{param_mle} is
#'   used directly as \code{omega_hat} — appropriate when both live in
#'   the same parameter space. When supplied, \code{data} is bound at
#'   calibration time, reducing the signature to
#'   \code{function(param_mle)}. Default: \code{NULL}.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{likelihood_spec} object.
#' @export
likelihood_spec <- function(
  loglik,
  E_loglik = NULL,
  E_loglik_grad = NULL,
  needs_param_mle = FALSE,
  omega_hat_from_param_mle = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<likelihood>",
    loglik = loglik,
    E_loglik = E_loglik,
    E_loglik_grad = E_loglik_grad,
    needs_param_mle = needs_param_mle,
    omega_hat_from_param_mle = omega_hat_from_param_mle,
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
  # loglik — required ------------------------------------------------
  if (!is.function(x$loglik)) {
    stop("loglik must be a function(param, data).", call. = FALSE)
  }

  # E_loglik — optional at construction, required for integrate() ----
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

  # needs_param_mle --------------------------------------------------
  if (!is.logical(x$needs_param_mle) || length(x$needs_param_mle) != 1L) {
    stop("needs_param_mle must be a single logical value.", call. = FALSE)
  }

  # omega_hat_from_param_mle — optional ------------------------------
  if (
    !is.null(x$omega_hat_from_param_mle) &&
      !is.function(x$omega_hat_from_param_mle)
  ) {
    stop(
      "omega_hat_from_param_mle must be NULL or a function(param_mle, data).",
      call. = FALSE
    )
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.likelihood_spec <- function(x, ...) {
  cat("# Likelihood Specification\n")
  cat("- Name:                        ", x$name, "\n", sep = "")
  cat("- loglik():                     \u2714 function\n")
  cat(
    "- E_loglik():                  ",
    if (!is.null(x$E_loglik)) "\u2714 function" else "absent (profile only)",
    "\n",
    sep = ""
  )
  cat(
    "- E_loglik_grad():             ",
    if (!is.null(x$E_loglik_grad)) {
      "\u2714 function"
    } else {
      "absent (finite differences)"
    },
    "\n",
    sep = ""
  )
  cat(
    "- needs_param_mle:             ",
    if (isTRUE(x$needs_param_mle)) "TRUE" else "FALSE",
    "\n",
    sep = ""
  )
  cat(
    "- omega_hat_from_param_mle():  ",
    if (!is.null(x$omega_hat_from_param_mle)) {
      "\u2714 function"
    } else {
      "NULL (param_mle used directly)"
    },
    "\n",
    sep = ""
  )
  invisible(x)
}

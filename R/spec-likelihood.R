# ======================================================================
# Likelihood Specification
# ======================================================================

#' Specify the Likelihood for a Model
#'
#' @param loglik Function \code{(param, data) -> scalar}. Observed
#'   log-likelihood.
#' @param E_loglik Function \code{(param, omega_hat, data, param_mle) ->
#'   scalar}. Expected log-likelihood under the nuisance configuration
#'   \code{omega_hat}. When \code{needs_param_mle = TRUE}, the
#'   \code{param_mle} argument is bound during calibration.
#' @param E_loglik_grad Optional function with same signature as
#'   \code{E_loglik}, returning the gradient of \code{E_loglik} with
#'   respect to \code{param}. When supplied, passed as \code{gr} to
#'   the augmented Lagrangian solver.
#' @param needs_param_mle Logical. When \code{TRUE}, \code{param_mle}
#'   is bound into \code{E_loglik} and \code{E_loglik_grad} during
#'   calibration alongside \code{data}. Default: \code{FALSE}.
#' @param omega_hat_from_param_mle Optional function
#'   \code{(param_mle, data) -> numeric vector}. Converts \code{param_mle}
#'   to a reference \code{omega_hat} for profile likelihood computation.
#'   Required when \code{omega_hat} and the model parameter live in
#'   different spaces (e.g. MLR, where \code{omega_hat} is a probability
#'   vector in \eqn{\Delta^{J-1}} but \code{param} is \code{vec(B)} in
#'   \eqn{\mathbb{R}^{p(J-1)}}). Bound to \code{data} during calibration.
#' @param make_branch_fns Optional function
#'   \code{(data, param_mle) -> function(omega_hat) -> list(fn, gr)}.
#'   When supplied, replaces the default \code{E_loglik}/\code{E_loglik_grad}
#'   closures in the branch binder with pre-optimized versions that
#'   precompute all quantities that are constant given \code{omega_hat}
#'   (e.g. \code{theta_hat}, \code{X_design}, \code{B_hat}). The outer
#'   function is called once during calibration with \code{data} and
#'   \code{param_mle}; the returned \code{function(omega_hat)} is called
#'   once per branch to produce \code{fn} and \code{gr} for auglag.
#'   When \code{NULL} (default), the standard \code{E_loglik} path is
#'   used. Simple models where \code{E_loglik} is already cheap need not
#'   define this slot.
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
  make_branch_fns = NULL,
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
    make_branch_fns = make_branch_fns,
    extra = list(...)
  )

  x <- new_likelihood_spec(x)
  x <- .validate_likelihood_spec(x)
  x
}

# ======================================================================
# INTERNAL CONSTRUCTOR
# ======================================================================

#' @keywords internal
#' @noRd
new_likelihood_spec <- function(x) .new_spec(x, "likelihood_spec")

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_likelihood_spec <- function(x) {
  if (!is.function(x$loglik)) {
    stop("loglik must be a function(param, data).", call. = FALSE)
  }
  if (!is.null(x$E_loglik) && !is.function(x$E_loglik)) {
    stop("E_loglik must be NULL or a function.", call. = FALSE)
  }
  if (!is.null(x$E_loglik_grad) && !is.function(x$E_loglik_grad)) {
    stop("E_loglik_grad must be NULL or a function.", call. = FALSE)
  }
  if (!is.null(x$E_loglik_grad) && is.null(x$E_loglik)) {
    stop("E_loglik_grad requires E_loglik to be supplied.", call. = FALSE)
  }
  if (!is.logical(x$needs_param_mle) || length(x$needs_param_mle) != 1L) {
    stop("needs_param_mle must be a single logical value.", call. = FALSE)
  }
  if (
    !is.null(x$omega_hat_from_param_mle) &&
      !is.function(x$omega_hat_from_param_mle)
  ) {
    stop("omega_hat_from_param_mle must be NULL or a function.", call. = FALSE)
  }
  if (!is.null(x$make_branch_fns) && !is.function(x$make_branch_fns)) {
    stop("make_branch_fns must be NULL or a function.", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.likelihood_spec <- function(x, ...) {
  cat("# Likelihood Specification\n")
  cat("- Name:                      ", x$name, "\n", sep = "")
  cat("- loglik:                     function\n")
  cat(
    "- E_loglik:                  ",
    if (!is.null(x$E_loglik)) "function" else "NULL",
    "\n",
    sep = ""
  )
  cat(
    "- E_loglik_grad:             ",
    if (!is.null(x$E_loglik_grad)) "function" else "NULL",
    "\n",
    sep = ""
  )
  cat("- needs_param_mle:           ", x$needs_param_mle, "\n", sep = "")
  cat(
    "- omega_hat_from_param_mle:  ",
    if (!is.null(x$omega_hat_from_param_mle)) "function" else "NULL",
    "\n",
    sep = ""
  )
  cat(
    "- make_branch_fns:           ",
    if (!is.null(x$make_branch_fns)) {
      "function (optimized branch objective)"
    } else {
      "NULL (standard E_loglik path)"
    },
    "\n",
    sep = ""
  )
  invisible(x)
}

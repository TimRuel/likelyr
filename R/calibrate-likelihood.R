# ======================================================================
# Likelihood Calibration (v4.1)
# ======================================================================

#' Calibrate Likelihood Component
#'
#' @description
#' Binds data to all likelihood closures. \code{loglik} is always
#' bound. \code{E_loglik} and \code{E_loglik_grad} are bound only
#' when present.
#'
#' @param likelihood A \code{likelihood_spec} object.
#' @param data User data.
#'
#' @return The SAME \code{likelihood_spec} object, enriched with:
#'   \itemize{
#'     \item \code{$loglik}        — \code{function(param)}
#'     \item \code{$E_loglik}      — \code{function(param, omega_hat)},
#'       if supplied
#'     \item \code{$E_loglik_grad} — \code{function(param, omega_hat)},
#'       if supplied
#'   }
#'
#' @keywords internal
calibrate_likelihood <- function(likelihood, data) {
  stopifnot(inherits(likelihood, "likelihood_spec"))

  # -------------------------------------------------------------------
  # 1. Bind data to loglik
  #    loglik has signature (param, data) — different from the
  #    (param, omega_hat, data) signature that .bind_data_env expects,
  #    so we bind it directly.
  # -------------------------------------------------------------------
  orig_loglik <- .bundle_fun_env(likelihood$loglik)
  d <- data

  ll_wrapper <- function(param) orig_loglik(param, d)

  ll_env <- new.env(parent = baseenv())
  ll_env$orig_loglik <- orig_loglik
  ll_env$d <- d
  environment(ll_wrapper) <- ll_env

  likelihood$loglik <- ll_wrapper

  # -------------------------------------------------------------------
  # 2. Bind data to E_loglik (if present)
  # -------------------------------------------------------------------
  if (!is.null(likelihood$E_loglik)) {
    likelihood$E_loglik <- .bind_data_env(likelihood$E_loglik, data)
  }

  # -------------------------------------------------------------------
  # 3. Bind data to E_loglik_grad (if present)
  # -------------------------------------------------------------------
  if (!is.null(likelihood$E_loglik_grad)) {
    likelihood$E_loglik_grad <- .bind_data_env(likelihood$E_loglik_grad, data)
  }

  likelihood
}

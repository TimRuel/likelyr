# ======================================================================
# Likelihood Calibration (v4.5)
# ======================================================================

#' Calibrate Likelihood Component
#'
#' @description
#' Binds \code{data} and optionally \code{param_mle} to all likelihood
#' closures. \code{loglik} is always bound to \code{data}. When
#' \code{param_mle} is supplied, \code{E_loglik} and
#' \code{E_loglik_grad} are bound to both \code{data} and
#' \code{param_mle}, reducing their signatures from
#' \code{function(param, omega_hat, data, param_mle)} to
#' \code{function(param, omega_hat)}. When \code{param_mle} is
#' \code{NULL}, only \code{data} is bound via \code{.bind_data_env()}.
#' When \code{omega_hat_from_param_mle} is present, \code{data} is
#' bound into it, reducing its signature from
#' \code{function(param_mle, data)} to \code{function(param_mle)}.
#'
#' @param likelihood A \code{likelihood_spec} object.
#' @param data       User data.
#' @param param_mle  Optional numeric vector; MLE of the full model
#'   parameter. When supplied, bound into \code{E_loglik} and
#'   \code{E_loglik_grad} alongside \code{data}. Pass when these
#'   functions require the MLE as a reference — e.g. when constructing
#'   an observation-specific \code{theta_hat} from \code{omega_hat} via
#'   a rank-1 adjustment of \code{B_mle}. Default: \code{NULL}.
#'
#' @return The SAME \code{likelihood_spec} object, enriched with:
#'   \itemize{
#'     \item \code{$loglik}                   — \code{function(param)}
#'     \item \code{$E_loglik}                 — \code{function(param, omega_hat)},
#'       if supplied
#'     \item \code{$E_loglik_grad}            — \code{function(param, omega_hat)},
#'       if supplied
#'     \item \code{$omega_hat_from_param_mle} — \code{function(param_mle)},
#'       if supplied
#'   }
#'
#' @keywords internal
calibrate_likelihood <- function(likelihood, data, param_mle = NULL) {
  stopifnot(inherits(likelihood, "likelihood_spec"))

  # -------------------------------------------------------------------
  # 1. Bind data to loglik
  #    loglik has signature (param, data) — different from the
  #    (param, omega_hat, data, param_mle) signature used by E_loglik,
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
  # 2. Bind data (and param_mle if supplied) to E_loglik
  # -------------------------------------------------------------------
  if (!is.null(likelihood$E_loglik)) {
    if (!is.null(param_mle)) {
      orig_fn <- .bundle_fun_env(likelihood$E_loglik)
      d_ <- data
      pm_ <- param_mle
      fn <- function(param, omega_hat) orig_fn(param, omega_hat, d_, pm_)
      fn_env <- new.env(parent = baseenv())
      fn_env$orig_fn <- orig_fn
      fn_env$d_ <- d_
      fn_env$pm_ <- pm_
      environment(fn) <- fn_env
      likelihood$E_loglik <- fn
    } else {
      likelihood$E_loglik <- .bind_data_env(likelihood$E_loglik, data)
    }
  }

  # -------------------------------------------------------------------
  # 3. Bind data (and param_mle if supplied) to E_loglik_grad
  # -------------------------------------------------------------------
  if (!is.null(likelihood$E_loglik_grad)) {
    if (!is.null(param_mle)) {
      orig_fn <- .bundle_fun_env(likelihood$E_loglik_grad)
      d_ <- data
      pm_ <- param_mle
      fn <- function(param, omega_hat) orig_fn(param, omega_hat, d_, pm_)
      fn_env <- new.env(parent = baseenv())
      fn_env$orig_fn <- orig_fn
      fn_env$d_ <- d_
      fn_env$pm_ <- pm_
      environment(fn) <- fn_env
      likelihood$E_loglik_grad <- fn
    } else {
      likelihood$E_loglik_grad <- .bind_data_env(
        likelihood$E_loglik_grad,
        data
      )
    }
  }

  # -------------------------------------------------------------------
  # 4. Bind data to omega_hat_from_param_mle (if present)
  #    Reduces signature from function(param_mle, data)
  #    to function(param_mle) for use by .generate_profile().
  # -------------------------------------------------------------------
  if (!is.null(likelihood$omega_hat_from_param_mle)) {
    orig_fn <- .bundle_fun_env(likelihood$omega_hat_from_param_mle)
    d_ <- data
    fn <- function(param_mle) orig_fn(param_mle, d_)
    fn_env <- new.env(parent = baseenv())
    fn_env$orig_fn <- orig_fn
    fn_env$d_ <- d_
    environment(fn) <- fn_env
    likelihood$omega_hat_from_param_mle <- fn
  }

  likelihood
}

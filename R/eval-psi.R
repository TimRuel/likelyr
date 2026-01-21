# ======================================================================
# eval-psi.R (v5.0)
#
# Fast evaluator for:
#   θ*(ψ, ω̂) = argmax_θ E_{ω̂}[ log L(θ) ] subject to ψ(θ) = ψ_target
#
# Updated for the new calibrated-model API:
#   • likelihood_spec owns constraints (param bounds, inequalities)
#   • estimand_spec owns psi_fn, psi_jac
#   • nuisance_spec owns E_loglik(), E_loglik_grad()
#   • optimizer_spec owns solver settings
#
# ======================================================================

#' Build Fast ψ-Conditional Optimizer for Branch Evaluation (Internal)
#'
#' @description
#' Constructs a *two-stage closure system* for fast repeated solutions:
#'
#' \deqn{
#'   \param^{\*}(\psi, \omega)
#'     = \arg\max_{\param} E_{\omega}[ \log L(\param) ]
#'     \quad\text{s.t.}\quad
#'     ψ(\param)=ψ_{\mathrm{target}}.
#' }
#'
#' Usage:
#'
#' \preformatted{
#' f1 <- build_eval_psi_fun(cal)
#' f2 <- f1(omega_hat)
#' out <- f2(psi_target, param_init)
#' }
#'
#' @param cal A `likelyr_calibrated` model object.
#'
#' @return A nested function:
#'
#' \preformatted{
#'   function(omega_hat) {
#'     function(psi_target, param_init) { ... }
#'   }
#' }
#'
#' @keywords internal
build_eval_psi_fun <- function(cal) {
  # ---------------------------------------------------------------
  # Unpack calibrated components
  # ---------------------------------------------------------------
  param <- cal$parameter
  lik <- cal$likelihood
  est <- cal$estimand
  nuis <- cal$nuisance
  opt <- cal$optimizer

  # Bound, data-bound closures (now inside specs)
  loglik <- lik$loglik
  psi_fn <- est$psi_fn
  psi_jac <- est$psi_jac

  E_loglik <- nuis$E_loglik
  E_loglik_grad <- nuis$E_loglik_grad
  has_grad <- !is.null(E_loglik_grad)

  # ---------------------------------------------------------------
  # Likelihood-level constraints
  # ---------------------------------------------------------------
  param_lower <- param$param_lower
  param_upper <- param$param_upper
  ineq_fn <- param$ineq
  ineq_jac_fn <- param$ineq_jac

  J <- param$param_dim

  # Expand missing bounds
  if (is.null(param_lower)) {
    param_lower <- rep(-Inf, J)
  }
  if (is.null(param_upper)) {
    param_upper <- rep(Inf, J)
  }

  # ---------------------------------------------------------------
  # Optimization environment for auglag()
  # ---------------------------------------------------------------
  eval_env <- list2env(
    list(
      # static constraints
      lower = param_lower,
      upper = param_upper,
      hin = ineq_fn,
      hinjac = ineq_jac_fn,
      heqjac = psi_jac,

      # solver settings
      localsolver = opt$localsolver,
      localtol = opt$localtol,
      control = opt$control,
      deprecatedBehavior = FALSE,

      # dynamic variables updated for each call
      omega_hat = NULL,
      psi_target = NULL,
      x0 = NULL
    ),
    parent = baseenv()
  )

  # ---------------------------------------------------------------
  # Key closures for nloptr::auglag()
  # ---------------------------------------------------------------
  eval_env$fn <- function(param) {
    -E_loglik(param, eval_env$omega_hat)
  }

  eval_env$gr <- if (has_grad) {
    function(param) -E_loglik_grad(param, eval_env$omega_hat)
  } else {
    NULL
  }

  eval_env$heq <- function(param) {
    psi_fn(param) - eval_env$psi_target
  }

  # ---------------------------------------------------------------
  # Stage 1: fix nuisance ω̂
  # ---------------------------------------------------------------
  function(omega_hat) {
    eval_env$omega_hat <- omega_hat

    # -------------------------------------------------------------
    # Stage 2: solve θ(ψ_target, ω̂)
    # -------------------------------------------------------------
    function(psi_target, param_init) {
      eval_env$psi_target <- psi_target
      eval_env$x0 <- param_init

      param_hat <- nloptr::auglag(
        x0 = eval_env$x0,
        fn = eval_env$fn,
        gr = eval_env$gr,
        heq = eval_env$heq,
        heqjac = eval_env$heqjac,
        hin = eval_env$hin,
        hinjac = eval_env$hinjac,
        lower = eval_env$lower,
        upper = eval_env$upper,
        localsolver = eval_env$localsolver,
        localtol = eval_env$localtol,
        control = eval_env$control,
        deprecatedBehavior = eval_env$deprecatedBehavior
      )$par

      list(
        param_hat = param_hat,
        branch_val = loglik(param_hat)
      )
    }
  }
}

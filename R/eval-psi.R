# ======================================================================
# eval-psi.R (v5.0)
#
# Fast evaluator for:
#   θ*(ψ, ω̂) = argmax_θ E_{ω̂}[ log L(θ) ] subject to ψ(θ) = ψ_target
#
# Updated for the new calibrated-model API:
#   • likelihood_spec owns constraints (theta bounds, inequalities)
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
#'   \theta^{\*}(\psi, \omega)
#'     = \arg\max_{\theta} E_{\omega}[ \log L(\theta) ]
#'     \quad\text{s.t.}\quad
#'     ψ(\theta)=ψ_{\mathrm{target}}.
#' }
#'
#' Usage:
#'
#' \preformatted{
#' f1 <- build_eval_psi_fun(cal)
#' f2 <- f1(omega_hat)
#' out <- f2(psi_target, theta_init)
#' }
#'
#' @param cal A `calibrated_model` object.
#'
#' @return A nested function:
#'
#' \preformatted{
#'   function(omega_hat) {
#'     function(psi_target, theta_init) { ... }
#'   }
#' }
#'
#' @keywords internal
#' @noRd
build_eval_psi_fun <- function(cal) {

  # ---------------------------------------------------------------
  # Unpack calibrated components
  # ---------------------------------------------------------------
  param <- cal$parameter
  lik   <- cal$likelihood
  est   <- cal$estimand
  nuis  <- cal$nuisance
  opt   <- cal$optimizer

  # Bound, data-bound closures (now inside specs)
  loglik  <- lik$loglik
  psi_fn  <- est$psi_fn
  psi_jac <- est$psi_jac

  E_loglik      <- nuis$E_loglik
  E_loglik_grad <- nuis$E_loglik_grad
  has_grad      <- !is.null(E_loglik_grad)

  # ---------------------------------------------------------------
  # Likelihood-level constraints
  # ---------------------------------------------------------------
  theta_lower <- param$theta_lower
  theta_upper <- param$theta_upper
  ineq_fn     <- param$ineq
  ineq_jac_fn <- param$ineq_jac

  J <- param$theta_dim

  # Expand missing bounds
  if (is.null(theta_lower)) theta_lower <- rep(-Inf, J)
  if (is.null(theta_upper)) theta_upper <- rep( Inf, J)

  # ---------------------------------------------------------------
  # Optimization environment for auglag()
  # ---------------------------------------------------------------
  eval_env <- list2env(
    list(
      # static constraints
      lower       = theta_lower,
      upper       = theta_upper,
      hin         = ineq_fn,
      hinjac      = ineq_jac_fn,
      heqjac      = psi_jac,

      # solver settings
      localsolver = opt$localsolver,
      localtol    = opt$localtol,
      control     = opt$control,
      deprecatedBehavior = FALSE,

      # dynamic variables updated for each call
      omega_hat   = NULL,
      psi_target  = NULL,
      x0          = NULL
    ),
    parent = emptyenv()
  )

  # ---------------------------------------------------------------
  # Key closures for nloptr::auglag()
  # ---------------------------------------------------------------
  eval_env$fn <- function(theta) {
    -E_loglik(theta, eval_env$omega_hat)
  }

  eval_env$gr <- if (has_grad) {
    function(theta) -E_loglik_grad(theta, eval_env$omega_hat)
  } else NULL

  eval_env$heq <- function(theta) {
    psi_fn(theta) - eval_env$psi_target
  }

  # ---------------------------------------------------------------
  # Stage 1: fix nuisance ω̂
  # ---------------------------------------------------------------
  function(omega_hat) {

    eval_env$omega_hat <- omega_hat

    # -------------------------------------------------------------
    # Stage 2: solve θ(ψ_target, ω̂)
    # -------------------------------------------------------------
    function(psi_target, theta_init) {

      eval_env$psi_target <- psi_target
      eval_env$x0         <- theta_init

      theta_hat <- nloptr::auglag(
        x0          = eval_env$x0,
        fn          = eval_env$fn,
        gr          = eval_env$gr,
        heq         = eval_env$heq,
        heqjac      = eval_env$heqjac,
        hin         = eval_env$hin,
        hinjac      = eval_env$hinjac,
        lower       = eval_env$lower,
        upper       = eval_env$upper,
        localsolver = eval_env$localsolver,
        localtol    = eval_env$localtol,
        control     = eval_env$control,
        deprecatedBehavior = eval_env$deprecatedBehavior
      )$par

      list(
        theta_hat  = theta_hat,
        branch_val = loglik(theta_hat)
      )
    }
  }
}

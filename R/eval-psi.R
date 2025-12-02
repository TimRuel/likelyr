# ======================================================================
# eval-psi.R (v4.1)
#
# Fast evaluator for:
#   θ*(ψ, ω̂) = argmax_θ E_{ω̂}[ log L(θ) ]
#               subject to ψ(θ) = ψ_target
#
# Fully aligned with new API:
#   • likelihood_spec() owns constraints:
#         theta_lower, theta_upper, ineq, ineq_jac
#   • optimizer_spec() owns solver settings
#   • calibrate() binds data
#
# ======================================================================


#' Build Fast ψ-Conditional Optimizer for Branch Evaluation (Internal)
#'
#' @description
#' Returns a **two-stage factory** for extremely fast repeated solutions:
#'
#' \deqn{
#'   \theta^{\*}(\psi, \omega)
#'     = \arg\max_{\theta} E_{\omega}[ \log L(\theta) ]
#'     \quad\text{s.t.}\quad
#'       \psi(\theta)=\psi_{\mathrm{target}}.
#' }
#'
#' The returned object enables:
#'
#' \preformatted{
#'   f1 <- build_eval_psi_fun(cal)
#'   f2 <- f1(omega_hat)
#'   out <- f2(psi_target, theta_init)
#' }
#'
#' @param cal A `calibrated_model` returned by `calibrate()`.
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
  # Unpack model components
  # ---------------------------------------------------------------
  lik      <- cal$likelihood
  opt      <- cal$optimizer

  loglik        <- cal$loglik
  E_loglik      <- cal$E_loglik
  E_loglik_grad <- cal$E_loglik_grad
  psi_fn        <- cal$psi_fn
  psi_jac       <- cal$psi_jac

  has_grad <- !is.null(E_loglik_grad)

  # ---------------------------------------------------------------
  # Likelihood-level constraints (correct names!)
  # ---------------------------------------------------------------
  theta_lower <- lik$theta_lower   # may be NULL
  theta_upper <- lik$theta_upper   # may be NULL
  ineq_fn     <- lik$ineq
  ineq_jac_fn <- lik$ineq_jac

  J <- lik$theta_dim

  # Expand NULL bounds to ±Inf
  if (is.null(theta_lower)) theta_lower <- rep(-Inf, J)
  if (is.null(theta_upper)) theta_upper <- rep( Inf, J)

  # ---------------------------------------------------------------
  # Create closed-over environment for auglag()
  # ---------------------------------------------------------------
  eval_env <- list2env(
    list(
      # Static model constraints
      lower       = theta_lower,
      upper       = theta_upper,
      hin         = ineq_fn,
      hinjac      = ineq_jac_fn,
      heqjac      = psi_jac,
      localsolver = opt$localsolver,
      localtol    = opt$localtol,
      control     = opt$control,
      deprecatedBehavior = FALSE,

      # Dynamic fields
      omega_hat   = NULL,
      psi_target  = NULL,
      x0          = NULL
    ),
    parent = emptyenv()
  )

  # ---------------------------------------------------------------
  # Permanent, minimal-overhead closures
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
  # First stage: fix nuisance ω̂
  # ---------------------------------------------------------------
  function(omega_hat) {

    eval_env$omega_hat <- omega_hat

    # -------------------------------------------------------------
    # Second stage: solve θ*(ψ_target, ω̂)
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

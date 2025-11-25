#' Build Fast ψ-Conditional Optimizer for Branch Evaluation (Internal)
#'
#' @description
#' Constructs a **two-stage factory** used in integrated-likelihood
#' branch evaluation.
#'
#' The returned function enables extremely fast repeated solutions of
#'
#' \deqn{
#'   \theta^{\*}(\psi, \omega)
#'     = \arg\max_{\theta}
#'         E\_{\omega}\big[\log L(\theta)\big]
#'     \quad\text{s.t.}\quad
#'       \psi(\theta) = \psi\_{\text{target}},
#' }
#'
#' where:
#' * \eqn{\omega} is a nuisance draw on the ψ-constraint manifold,
#' * the optimization is done with `nloptr::auglag()`,
#' * only dynamic quantities change per evaluation (ψ_target, theta_init, ω̂),
#' * **all static model components are captured once** in a locked environment.
#'
#' This produces a high-performance evaluator used by:
#' * `branch_mode_solve()`
#' * `walk_branch_side()`
#' * `generate_branches()`
#'
#' and is central to the new IL API.
#'
#' ## Architecture
#'
#' `build_eval_psi_fun(cal)` returns:
#'
#' \preformatted{
#'   f1 <- build_eval_psi_fun(cal)
#'   f2 <- f1(omega_hat)                  # fixes nuisance parameters
#'   out <- f2(psi_target, theta_init)   # solves equality-constrained θ̂
#' }
#'
#' The inner evaluator returns:
#'
#' \preformatted{
#' list(
#'   theta_hat  = solution vector,
#'   branch_val = loglik(theta_hat)
#' )
#' }
#'
#' @param cal A list produced by `calibrate_IL()` containing:
#'   * likelihood functions (`loglik`, `E_loglik`, `E_loglik_grad`)
#'   * estimand functions (`psi_fn`, `psi_jac`)
#'   * model constraints
#'   * optimizer specification
#'
#' @details
#' All static arguments to `nloptr::auglag()` (bounds, Jacobians,
#' inequality constraints, solver settings) are stored in a private
#' environment (`eval_env`).
#'
#' The environment also stores dynamic fields that update on each call:
#' * `omega_hat`
#' * `psi_target`
#' * `x0`
#'
#' Permanent closures (`fn`, `gr`, `heq`) read from this environment,
#' giving near-zero allocation overhead per optimization.
#'
#' @return
#' A function:
#'
#' \preformatted{
#'   function(omega_hat) {
#'       function(psi_target, theta_init) { ... }
#'   }
#' }
#'
#' The inner function performs the constrained optimization and returns
#' `list(theta_hat, branch_val)`.
#'
#' @keywords internal
#' @noRd
build_eval_psi_fun <- function(cal) {

  # ---------------------------------------------------------------
  # Unpack workflow components
  # ---------------------------------------------------------------
  model     <- cal$workflow$model
  estimand  <- cal$workflow$estimand
  optimizer <- cal$workflow$optimizer

  loglik        <- cal$loglik
  E_loglik      <- cal$E_loglik
  E_loglik_grad <- cal$E_loglik_grad
  psi_fn        <- cal$psi_fn

  has_grad <- !is.null(E_loglik_grad)

  # ---------------------------------------------------------------
  # eval_env: stores static + dynamic auglag arguments
  # ---------------------------------------------------------------
  eval_env <- list2env(
    list(
      # Static auglag components
      lower       = model$lower,
      upper       = model$upper,
      hin         = model$ineq,
      hinjac      = model$ineq_jac,
      heqjac      = cal$psi_jac,
      localsolver = optimizer$localsolver,
      localtol    = optimizer$localtol,
      control     = optimizer$control,
      deprecatedBehavior = FALSE,

      # Dynamic state
      omega_hat   = NULL,
      psi_target  = NULL,
      x0          = NULL
    ),
    parent = emptyenv()
  )

  # ---------------------------------------------------------------
  # Permanent closures (read dynamic values from eval_env)
  # ---------------------------------------------------------------

  eval_env$fn <- function(theta) {
    -E_loglik(theta, eval_env$omega_hat)
  }

  eval_env$gr <- if (has_grad) {
    function(theta) -E_loglik_grad(theta, eval_env$omega_hat)
  } else {
    NULL
  }

  eval_env$heq <- function(theta) {
    psi_fn(theta) - eval_env$psi_target
  }

  # ---------------------------------------------------------------
  # Return ω̂ → ψ-evaluation factory
  # ---------------------------------------------------------------
  function(omega_hat) {

    # update dynamic value used by fn & gr
    eval_env$omega_hat <- omega_hat

    # -------------------------------------------------------------
    # Return ψ_target → θ̂(ψ_target, ω̂) evaluator
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

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
  # Environment storing BOTH static and dynamic auglag arguments
  # ---------------------------------------------------------------
  eval_env <- list2env(
    list(
      # --- static auglag arguments ---
      lower       = model$lower,
      upper       = model$upper,
      hin         = model$ineq,
      hinjac      = model$ineq_jac,
      heqjac      = estimand$psi_jac,
      localsolver = optimizer$localsolver,
      localtol    = optimizer$localtol,
      control     = optimizer$control,
      deprecatedBehavior = FALSE,

      # --- dynamic values (mutated each evaluation) ---
      omega_hat   = NULL,
      psi_target  = NULL,
      x0          = NULL
    ),
    parent = emptyenv()
  )

  # ---------------------------------------------------------------
  # Permanent closures reading dynamic values from eval_env
  # ---------------------------------------------------------------

  # objective(θ) = -E_loglik(θ, omega_hat)
  objective <- function(theta) {
    -E_loglik(theta, eval_env$omega_hat)
  }

  # gradient(θ) = -E_loglik_grad(θ, omega_hat)   or NULL
  if (has_grad) {
    gradient <- function(theta) {
      -E_loglik_grad(theta, eval_env$omega_hat)
    }
  } else {
    gradient <- NULL
  }

  # heq(θ) = psi_fn(θ) - psi_target
  heq <- function(theta) {
    psi_fn(theta) - eval_env$psi_target
  }

  # register them into environment once
  eval_env$fn  <- objective
  eval_env$gr  <- gradient
  eval_env$heq <- heq

  # ---------------------------------------------------------------
  # Return ω̂ → ψ-evaluation factory
  # ---------------------------------------------------------------
  function(omega_hat) {

    # update dynamic value used by objective / gradient
    eval_env$omega_hat <- omega_hat

    # -------------------------------------------------------------
    # Return ψ_target → θ̂(ψ_target, ω̂) evaluator
    # -------------------------------------------------------------
    function(psi_target, theta_init) {

      # update remaining dynamic auglag arguments
      eval_env$psi_target <- psi_target
      eval_env$x0         <- theta_init

      # direct auglag call using permanent closures
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

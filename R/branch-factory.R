build_branch_fn_factory <- function(
  parameter,
  likelihood,
  estimand,
  nuisance,
  optimizer,
  data
) {
  stopifnot(
    inherits(parameter, "parameter_spec"),
    inherits(likelihood, "likelihood_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(nuisance, "nuisance_spec"),
    inherits(optimizer, "optimizer_spec")
  )

  # -------------------------------------------------------------------
  # Parameter constraints
  # -------------------------------------------------------------------
  J <- parameter$param_dim

  lower <- parameter$param_lower %||% rep(-Inf, J)
  upper <- parameter$param_upper %||% rep(Inf, J)

  hin <- parameter$ineq
  hinjac <- parameter$ineq_jac

  # -------------------------------------------------------------------
  # Static, data-bound components
  # -------------------------------------------------------------------
  loglik <- likelihood$loglik
  psi_fn <- estimand$psi_fn
  psi_jac <- estimand$psi_jac

  E_loglik <- nuisance$E_loglik
  E_loglik_grad <- nuisance$E_loglik_grad
  has_grad <- !is.null(E_loglik_grad)

  # -------------------------------------------------------------------
  # Optimizer settings
  # -------------------------------------------------------------------
  localsolver <- optimizer$localsolver
  localtol <- optimizer$localtol
  control <- optimizer$control

  # -------------------------------------------------------------------
  # Shared evaluation environment (avoids repeated allocations)
  # -------------------------------------------------------------------
  eval_env <- list2env(
    list(
      omega_hat = NULL,
      psi_target = NULL
    ),
    parent = baseenv()
  )

  # -------------------------------------------------------------------
  # Objective and constraints for auglag
  # -------------------------------------------------------------------
  fn <- function(param) {
    -E_loglik(param, eval_env$omega_hat)
  }

  gr <- if (has_grad) {
    function(param) -E_loglik_grad(param, eval_env$omega_hat)
  } else {
    NULL
  }

  heq <- function(param) {
    psi_fn(param) - eval_env$psi_target
  }

  # -------------------------------------------------------------------
  # Stage 1: bind ω̂
  # -------------------------------------------------------------------
  function(omega_hat) {
    eval_env$omega_hat <- omega_hat

    # ---------------------------------------------------------------
    # Stage 2: solve θ*(ψ, ω̂)
    # ---------------------------------------------------------------
    function(psi_target, param_init) {
      eval_env$psi_target <- psi_target

      res <- nloptr::auglag(
        x0 = param_init,
        fn = fn,
        gr = gr,
        heq = heq,
        heqjac = psi_jac,
        hin = hin,
        hinjac = hinjac,
        lower = lower,
        upper = upper,
        localsolver = localsolver,
        localtol = localtol,
        control = control,
        deprecatedBehavior = FALSE
      )

      param_hat <- res$par

      list(
        param_hat = param_hat,
        branch_val = loglik(param_hat)
      )
    }
  }
}

# ======================================================================
# branch-factory.R (v3.2) — equality + inequality constraints
#
# Builds a factory that produces branch evaluators:
#   ψ ↦ max_θ E[ℓ(θ); ω̂] subject to
#     • ψ(θ) = ψ_target
#     • eq(θ) = 0
#     • ineq(θ) ≤ 0
#
# All constraint branching is resolved ONCE for efficiency.
# ======================================================================

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

  eq_fn <- parameter$eq
  eq_jac <- parameter$eq_jac
  hin_fn <- parameter$ineq
  hin_jac <- parameter$ineq_jac

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
  # Objective and gradient
  # -------------------------------------------------------------------
  fn <- function(param) {
    -E_loglik(param, eval_env$omega_hat)
  }

  gr <- if (has_grad) {
    function(param) -E_loglik_grad(param, eval_env$omega_hat)
  } else {
    NULL
  }

  # -------------------------------------------------------------------
  # Equality constraints (branch-free construction)
  # -------------------------------------------------------------------
  heq <- if (is.null(eq_fn)) {
    function(param) {
      psi_fn(param) - eval_env$psi_target
    }
  } else {
    function(param) {
      c(
        psi_fn(param) - eval_env$psi_target,
        eq_fn(param)
      )
    }
  }

  heqjac <- if (is.null(psi_jac) && is.null(eq_jac)) {
    NULL
  } else if (!is.null(psi_jac) && is.null(eq_jac)) {
    function(param) {
      Jpsi <- psi_jac(param)
      if (is.vector(Jpsi)) matrix(Jpsi, nrow = 1) else Jpsi
    }
  } else if (is.null(psi_jac) && !is.null(eq_jac)) {
    function(param) {
      eq_jac(param)
    }
  } else {
    function(param) {
      Jpsi <- psi_jac(param)
      if (is.vector(Jpsi)) {
        Jpsi <- matrix(Jpsi, nrow = 1)
      }
      rbind(Jpsi, eq_jac(param))
    }
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

      x0 <- as.numeric(param_init)
      x0 <- pmax(x0, lower)
      x0 <- pmin(x0, upper)

      res <- nloptr::auglag(
        x0 = x0,
        fn = fn,
        gr = gr,
        heq = heq,
        heqjac = heqjac,
        hin = hin_fn,
        hinjac = hin_jac,
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

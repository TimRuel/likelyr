# ======================================================================
# branch-factory.R (v4.0)
#
# Builds a factory that produces branch evaluators:
#   ψ ↦ max_θ E[ℓ(θ); ω̂] subject to
#     • ψ(θ) = ψ_target
#     • eq(θ) = 0        [optional structural constraints]
#     • ineq(θ) ≤ 0      [optional]
#
# Changes from v3.5:
#   • nuisance_spec removed — E_loglik and E_loglik_grad now live on
#     likelihood_spec after the objective/likelihood merge.
#   • optimizer_spec replaced by solver_spec (inner solver settings
#     only) and pipeline_spec (outer pipeline settings).
# ======================================================================

build_branch_fn_factory <- function(
  parameter,
  likelihood,
  estimand,
  solver,
  pipeline
) {
  stopifnot(
    inherits(parameter, "parameter_spec"),
    inherits(likelihood, "likelihood_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(solver, "solver_spec"),
    inherits(pipeline, "pipeline_spec")
  )

  # -------------------------------------------------------------------
  # Parameter constraints
  # -------------------------------------------------------------------
  J <- parameter$param_dim
  J_full <- J + 1L

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
  E_loglik <- likelihood$E_loglik
  E_loglik_grad <- likelihood$E_loglik_grad
  has_grad <- !is.null(E_loglik_grad)

  if (is.null(E_loglik)) {
    stop(
      "build_branch_fn_factory() requires E_loglik — supply it via likelihood_spec().",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Solver settings
  # -------------------------------------------------------------------
  localsolver <- solver$localsolver
  localtol <- solver$localtol
  control <- solver$control

  # -------------------------------------------------------------------
  # Permutation helper
  # -------------------------------------------------------------------
  .permute_eta <- function(eta, perm) {
    eta_full <- c(as.numeric(eta), 0.0)
    eta_perm <- eta_full[perm]
    eta_perm <- eta_perm - eta_perm[J_full]
    eta_perm[seq_len(J)]
  }

  # -------------------------------------------------------------------
  # Permutation correction
  # -------------------------------------------------------------------
  .best_permutation <- function(theta_hat) {
    best_ll <- -Inf
    best_par <- theta_hat

    for (k in seq_len(J_full)) {
      perm <- c(k, seq_len(J_full)[-k])
      eta_p <- .permute_eta(theta_hat, perm)

      ll <- try(loglik(eta_p), silent = TRUE)
      if (!inherits(ll, "try-error") && is.finite(ll) && ll > best_ll) {
        best_ll <- ll
        best_par <- eta_p
      }
    }

    list(par = best_par, loglik = best_ll)
  }

  # -------------------------------------------------------------------
  # Diagnostics helpers
  # -------------------------------------------------------------------
  .bound_min_slack <- function(theta) {
    d <- c(theta - lower, upper - theta)
    d <- d[is.finite(d)]
    if (length(d) == 0L) Inf else min(d)
  }

  .ineq_max <- function(theta) {
    if (is.null(hin_fn)) {
      return(NA_real_)
    }
    v <- hin_fn(theta)
    if (!is.numeric(v) || length(v) == 0L) {
      return(NA_real_)
    }
    max(as.numeric(v), na.rm = TRUE)
  }

  # -------------------------------------------------------------------
  # Stage 1: bind ω̂ — fresh environment per call
  # -------------------------------------------------------------------
  function(omega_hat) {
    omega_hat <- as.numeric(omega_hat)

    env <- new.env(parent = emptyenv())
    env$omega_hat <- omega_hat
    env$psi_target <- NULL

    E_loglik_max <- E_loglik(omega_hat, omega_hat)

    fn <- function(param) -E_loglik(param, env$omega_hat)
    gr <- if (has_grad) {
      function(param) -E_loglik_grad(param, env$omega_hat)
    } else {
      NULL
    }

    heq <- if (is.null(eq_fn)) {
      function(param) psi_fn(param) - env$psi_target
    } else {
      function(param) c(psi_fn(param) - env$psi_target, eq_fn(param))
    }

    heqjac <- if (is.null(psi_jac) && is.null(eq_jac)) {
      NULL
    } else if (!is.null(psi_jac) && is.null(eq_jac)) {
      function(param) {
        Jpsi <- psi_jac(param)
        if (is.vector(Jpsi)) matrix(Jpsi, nrow = 1L) else Jpsi
      }
    } else if (is.null(psi_jac) && !is.null(eq_jac)) {
      function(param) eq_jac(param)
    } else {
      function(param) {
        Jpsi <- psi_jac(param)
        if (is.vector(Jpsi)) {
          Jpsi <- matrix(Jpsi, nrow = 1L)
        }
        rbind(Jpsi, eq_jac(param))
      }
    }

    .eq_resid_inf <- function(theta) {
      v <- heq(theta)
      if (!is.numeric(v) || length(v) == 0L) {
        return(NA_real_)
      }
      max(abs(as.numeric(v)), na.rm = TRUE)
    }

    # -----------------------------------------------------------------
    # Stage 2: solve θ*(ψ, ω̂) and apply permutation correction
    # -----------------------------------------------------------------
    function(psi_target, param_init) {
      env$psi_target <- psi_target

      x0 <- as.numeric(param_init)
      if (any(!is.finite(x0))) {
        warning(
          "branch_fn: non-finite param_init replaced with omega_hat.",
          call. = FALSE
        )
        x0 <- omega_hat
      }
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

      theta_hat_orig <- as.numeric(res$par)

      best <- .best_permutation(theta_hat_orig)
      theta_hat_best <- best$par
      ll_best <- best$loglik

      E_loglik_at_hat <- E_loglik(theta_hat_orig, omega_hat)
      psi_at_hat <- psi_fn(theta_hat_orig)

      list(
        param_hat = theta_hat_best,
        branch_val = ll_best,

        param_hat_orig = theta_hat_orig,
        ll_orig = loglik(theta_hat_orig),

        E_loglik_at_hat = E_loglik_at_hat,
        E_loglik_gap = E_loglik_max - E_loglik_at_hat,

        psi_at_hat = psi_at_hat,
        psi_target = psi_target,
        psi_residual = psi_at_hat - psi_target,

        eq_resid_inf = .eq_resid_inf(theta_hat_orig),
        ineq_max = .ineq_max(theta_hat_orig),
        bound_min_slack = .bound_min_slack(theta_hat_orig),

        solver_status = res$status %||% NA_integer_,
        solver_message = res$message %||% NA_character_,
        solver_iterations = res$iterations %||% NA_integer_,
        solver_eval_counts = res$evaluations %||%
          list(fn = NA_integer_, gr = NA_integer_)
      )
    }
  }
}

# ======================================================================
# branch-binder.R
#
# Implements a three-level closure chain for branch evaluation:
#
#   branch_binder_constructor(parameter, likelihood, estimand, solver)
#     → function(omega_hat)                        [branch binder]
#         → function(psi_target, param_init)       [branch evaluator]
#               → list(param_hat, branch_val, ...)
#
# Level 1 — Constructor:
#   Called once at calibration time. Closes over all static
#   model components (constraints, objective, solver settings).
#   Returns a branch binder.
#
# Level 2 — Binder:
#   Called once per omega-hat. Closes over ω̂ and builds the
#   constrained objective and Jacobians. When make_branch_fns is
#   present on the likelihood, it is called here to get pre-optimized
#   fn/gr closures with theta_hat already computed for this omega_hat.
#   Otherwise, fn/gr are constructed from E_loglik/E_loglik_grad.
#   Returns a branch evaluator.
#
# Level 3 — Evaluator:
#   Called once per ψ grid point. Solves the constrained
#   optimisation problem:
#     ψ ↦ max_θ E[ℓ(θ; ω̂)] subject to
#       • ψ(θ) = ψ_target
#       • eq(θ) = 0        [optional structural constraints]
#       • ineq(θ) ≤ 0      [optional]
#   Returns the optimised parameter, branch log-likelihood,
#   and solver diagnostics.
#
# The omega_hat space and the model parameter space may differ in
# dimension. omega_dim (from parameter$omega_dim) gives the dimension
# of the omega_hat space; param_dim gives the model parameter dimension.
# When they differ (e.g. MLR where omega_hat ∈ Delta^{J-1} but
# param ∈ R^{p(J-1)}), E_loglik_max is set to NA and any param_init
# with the wrong dimension is caught and replaced with zeros.
# ======================================================================

#' @keywords internal
#' @noRd
branch_binder_constructor <- function(
  parameter,
  likelihood,
  estimand,
  solver
) {
  stopifnot(
    inherits(parameter, "parameter_spec"),
    inherits(likelihood, "likelihood_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(solver, "solver_spec")
  )

  # -------------------------------------------------------------------
  # Parameter space dimensions
  # -------------------------------------------------------------------
  param_dim <- parameter$param_dim
  omega_dim <- parameter$omega_dim %||% parameter$param_dim
  same_space <- isTRUE(omega_dim == param_dim)

  lower <- parameter$param_lower %||% rep(-Inf, param_dim)
  upper <- parameter$param_upper %||% rep(Inf, param_dim)
  use_bounds <- any(is.finite(lower)) || any(is.finite(upper))

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
  make_branch_fns <- likelihood$make_branch_fns # NULL for most applications
  has_grad <- !is.null(E_loglik_grad) || !is.null(make_branch_fns)

  if (is.null(E_loglik) && is.null(make_branch_fns)) {
    stop(
      "branch_binder_constructor() requires E_loglik or make_branch_fns ",
      "— supply at least one via likelihood_spec().",
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

  .eq_resid_inf <- function(theta, heq_fn) {
    v <- heq_fn(theta)
    if (!is.numeric(v) || length(v) == 0L) {
      return(NA_real_)
    }
    max(abs(as.numeric(v)), na.rm = TRUE)
  }

  # -------------------------------------------------------------------
  # Stage 1: bind ω̂ — fresh environment per call
  # -------------------------------------------------------------------
  function(omega_hat) {
    omega_hat <- as.numeric(omega_hat)

    env <- new.env(parent = emptyenv())
    env$omega_hat <- omega_hat
    env$psi_target <- NULL

    # -----------------------------------------------------------------
    # Build fn and gr for auglag.
    #
    # When make_branch_fns is present, call it with omega_hat to get
    # pre-optimized fn/gr with theta_hat already computed for this
    # omega_hat. This is the fast path for applications where E_loglik
    # recomputes expensive quantities (e.g. X_design %*% B_hat) on
    # every call.
    #
    # When make_branch_fns is absent, construct fn/gr from E_loglik/
    # E_loglik_grad as before. This is the standard path and works for
    # all applications.
    # -----------------------------------------------------------------
    if (!is.null(make_branch_fns)) {
      branch_fns <- make_branch_fns(omega_hat)
      fn <- branch_fns$fn
      gr <- branch_fns$gr
    } else {
      fn <- function(param) -E_loglik(param, env$omega_hat)
      gr <- if (!is.null(E_loglik_grad)) {
        function(param) -E_loglik_grad(param, env$omega_hat)
      } else {
        NULL
      }
    }

    # E_loglik_max for diagnostics — only meaningful when same_space
    E_loglik_max <- if (same_space && !is.null(E_loglik)) {
      E_loglik(omega_hat, omega_hat)
    } else {
      NA_real_
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

    # -----------------------------------------------------------------
    # Stage 2: solve θ*(ψ, ω̂)
    # -----------------------------------------------------------------
    function(psi_target, param_init) {
      env$psi_target <- psi_target

      x0 <- as.numeric(param_init)

      if (length(x0) != param_dim) {
        warning(
          "branch_evaluator: param_init has wrong dimension (got ",
          length(x0),
          ", expected ",
          param_dim,
          ") — replaced with zeros.",
          call. = FALSE
        )
        x0 <- rep(0, param_dim)
      } else if (any(!is.finite(x0))) {
        warning(
          "branch_evaluator: non-finite param_init replaced with zeros.",
          call. = FALSE
        )
        x0 <- rep(0, param_dim)
      }

      if (use_bounds) {
        x0 <- pmax(x0, lower)
        x0 <- pmin(x0, upper)
      }

      res <- nloptr::auglag(
        x0 = x0,
        fn = fn,
        gr = gr,
        heq = heq,
        heqjac = heqjac,
        hin = hin_fn,
        hinjac = hin_jac,
        lower = if (use_bounds) lower else NULL,
        upper = if (use_bounds) upper else NULL,
        localsolver = localsolver,
        localtol = localtol,
        control = control,
        deprecatedBehavior = FALSE
      )

      theta_hat <- as.numeric(res$par)
      psi_at_hat <- psi_fn(theta_hat)

      # E_loglik_at_hat via the standard E_loglik (called once post-solve,
      # not in the auglag inner loop, so overhead is acceptable)
      E_loglik_at_hat <- if (!is.null(E_loglik)) {
        E_loglik(theta_hat, omega_hat)
      } else {
        NA_real_
      }

      list(
        param_hat = theta_hat,
        branch_val = loglik(theta_hat),

        E_loglik_at_hat = E_loglik_at_hat,
        E_loglik_gap = E_loglik_max - E_loglik_at_hat,

        psi_at_hat = psi_at_hat,
        psi_target = psi_target,
        psi_residual = psi_at_hat - psi_target,

        eq_resid_inf = .eq_resid_inf(theta_hat, heq),
        ineq_max = .ineq_max(theta_hat),
        bound_min_slack = .bound_min_slack(theta_hat),

        solver_status = res$status %||% NA_integer_,
        solver_message = res$message %||% NA_character_,
        solver_iterations = res$iter %||% NA_integer_,
        solver_eval_counts = res$evaluations %||%
          list(fn = NA_integer_, gr = NA_integer_)
      )
    }
  }
}

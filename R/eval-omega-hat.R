# ======================================================================
# eval-omega-hat.R
#
# Utilities for sampling nuisance parameters ω̂ that satisfy the
# integrated-likelihood constraint ψ(ω̂) = ψ_MLE.
#
# This file exports two factories:
#
#   1. make_omega_hat_initgen(cal)
#        → returns a function(scale) → initial_guess
#
#   2. make_omega_hat_sampler(cal)
#        → returns a function(init_guess) → omega_hat satisfying ψ(ω̂)=ψ_MLE
#
# These factories are used inside calibrate_IL().
#
# ======================================================================


# ======================================================================
# 1. Good Initial-Guess Generator
# ======================================================================
# Produces starting points for auglag that:
#   • Begin near θ_MLE
#   • Are perturbed in directions *tangent* to the constraint manifold
#   • Maintain positivity for rate/variance parameters
#
# This drastically improves auglag convergence and MC manifold sampling.
# ======================================================================

make_omega_hat_initgen <- function(cal) {

  theta_mle <- cal$theta_mle      # full parameter vector
  psi_jac   <- cal$psi_jac        # possibly NULL
  has_jac   <- !is.null(psi_jac)

  function(scale = 0.25) {

    P <- length(theta_mle)

    # -------- Base perturbation: log-normal jitter --------------------
    jitter <- rlnorm(P, meanlog = 0, sdlog = scale) - 1
    candidate <- theta_mle * (1 + jitter)

    # -------- Tangential perturbation via ψ-gradient projection -------
    if (has_jac) {

      g <- psi_jac(theta_mle)  # gradient of ψ wrt θ at θ_MLE

      # Only proceed if jacobian dimension matches parameter dimension
      if (is.numeric(g) && length(g) == P) {

        g_norm <- sqrt(sum(g^2))

        if (is.finite(g_norm) && g_norm > 0) {

          g <- g / g_norm      # normalize

          # random direction
          r <- rnorm(P)

          # remove component along ψ-gradient
          r <- r - sum(r * g) * g

          # small tangent-space displacement
          candidate <- candidate + scale * 0.2 * r
        }
      }
    }

    # -------- Positivity enforcement (safe default for GLM families) ---
    candidate[candidate <= 1e-12] <- 1e-6

    candidate
  }
}



# ======================================================================
# 2. Omega-hat Sampler
# ======================================================================
# Solves the constraint:
#       ψ(ω̂) - ψ_MLE = 0
# using NLOPTR::auglag with a dummy objective (always zero).
#
# Uses an eval_env holding all static + dynamic auglag arguments,
# and zero-closure permanent fn/gr/heq for fastest inner-loop performance.
# ======================================================================

make_omega_hat_sampler <- function(cal) {

  psi_fn   <- cal$psi_fn           # θ → ψ(θ)
  psi_mle  <- cal$psi_mle
  wf       <- cal$workflow
  model    <- wf$model
  optimizer <- wf$optimizer
  estimand  <- wf$estimand

  # ------------------------------------------------------------
  # Define objective and constraint
  # ------------------------------------------------------------
  fn0    <- function(theta) 0
  heq_fn <- function(theta) psi_fn(theta) - psi_mle

  # ------------------------------------------------------------
  # Build eval_env containing fixed + dynamic auglag arguments
  # ------------------------------------------------------------
  eval_env <- list2env(
    list(
      # Static model-level constraints
      lower       = model$lower,
      upper       = model$upper,
      hin         = model$ineq,
      hinjac      = model$ineq_jac,

      # Equality constraint Jacobian (if provided)
      heqjac      = estimand$psi_jac,

      # Optimizer configuration
      localsolver = optimizer$localsolver,
      localtol    = optimizer$localtol,
      control     = optimizer$control,
      deprecatedBehavior = FALSE,

      # Dynamic argument updated per sample
      x0          = NULL
    ),
    parent = emptyenv()
  )

  # Permanent zero-closure callbacks
  eval_env$fn  <- fn0
  eval_env$gr  <- NULL
  eval_env$heq <- heq_fn

  # ------------------------------------------------------------
  # Return sampler: init_guess → omega_hat satisfying ψ(ω̂)=ψ_MLE
  # ------------------------------------------------------------
  function(init_guess, ...) {

    eval_env$x0 <- init_guess

    res <- nloptr::auglag(
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
      deprecatedBehavior = eval_env$deprecatedBehavior,
      ...
    )

    res$par   # full parameter vector satisfying ψ(θ) = ψ_MLE
  }
}

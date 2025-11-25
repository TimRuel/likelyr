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
# These are created inside calibrate_IL() and stored under:
#      cal$il$generate_init
#      cal$il$sample_omega_hat
#
# ======================================================================


#' Initial-Guess Generator for ω̂ Manifold Solver (Internal)
#'
#' @description
#' Constructs a function that generates **good initial guesses** for solving
#' the constraint
#'
#' \deqn{\psi(\omega) = \psi_{\mathrm{MLE}}}
#'
#' via `nloptr::auglag()`.
#' The generated initial guesses:
#' * start near \eqn{\theta_{\mathrm{MLE}}},
#' * are perturbed in directions **tangent** to the constraint manifold,
#' * maintain **positivity**, important for Poisson/NB/GLM-rate models.
#'
#' This dramatically improves convergence and Monte Carlo sampling stability.
#'
#' @param cal A calibration list created by `calibrate_IL()`, containing
#'   `theta_mle` and `psi_jac` (if provided by the estimand).
#'
#' @return
#' A function of the form:
#' \preformatted{
#'   f(scale = 0.25) -> numeric vector (initial_guess)
#' }
#' where `scale` controls the magnitude of the perturbations.
#'
#' @keywords internal
#' @noRd
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



#' Omega-Hat Sampler for ψ(ω̂) = ψ_MLE (Internal)
#'
#' @description
#' Constructs a **fast manifold sampler** that solves the constraint
#'
#' \deqn{\psi(\omega) - \psi_{\mathrm{MLE}} = 0}
#'
#' using `nloptr::auglag()` with a **dummy objective** (`0` everywhere).
#'
#' This produces nuisance-parameter draws ω̂ that lie **exactly on** the
#' integrated-likelihood constraint manifold.
#'
#' Performance notes:
#' * Uses a **zero-closure** design: objective, constraints, and their Jacobians
#'   are stored permanently in `eval_env` for maximum speed.
#' * Only the initial guess `x0` changes per call.
#'
#' @param cal A list returned by `calibrate_IL()`, containing:
#'   `psi_fn`, `psi_mle`, and model/optimizer specifications.
#'
#' @return
#' A function of the form:
#' \preformatted{
#'   f(init_guess) -> omega_hat
#' }
#' where the returned `omega_hat` satisfies \eqn{\psi(\omega)=\psi_{\mathrm{MLE}}}.
#'
#' @keywords internal
#' @noRd
make_omega_hat_sampler <- function(cal) {
  # Force evaluation of cal so we close over its current value,
  # not a lazy promise from calibrate_IL().
  force(cal)

  local({

    psi_fn   <- cal$psi_fn          # θ -> ψ(θ)
    psi_mle  <- cal$psi_mle
    wf       <- cal$workflow
    model    <- wf$model
    optimizer <- wf$optimizer
    estimand  <- wf$estimand

    # Dummy objective (we only care about the constraint)
    fn0    <- function(theta) 0
    heq_fn <- function(theta) psi_fn(theta) - psi_mle

    # Environment holding all auglag args (static + dynamic)
    eval_env <- list2env(
      list(
        # Static model constraints
        lower       = model$lower,
        upper       = model$upper,
        hin         = model$ineq,
        hinjac      = model$ineq_jac,

        # Equality constraint Jacobian, if present
        heqjac      = cal$psi_jac,

        # Optimizer config
        localsolver = optimizer$localsolver,
        localtol    = optimizer$localtol,
        control     = optimizer$control,
        deprecatedBehavior = FALSE,

        # Dynamic: updated per call
        x0          = NULL
      ),
      parent = emptyenv()
    )

    # Permanent, zero-closure callbacks
    eval_env$fn  <- fn0
    eval_env$gr  <- NULL
    eval_env$heq <- heq_fn

    # ----------------------------------------------------------
    # Return sampler: init_guess -> omega_hat satisfying ψ(ω̂)=ψ_MLE
    # ----------------------------------------------------------
    function(init_guess) {

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
        deprecatedBehavior = eval_env$deprecatedBehavior
      )

      res$par
    }
  })
}

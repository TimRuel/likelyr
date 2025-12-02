# ======================================================================
# eval-omega-hat.R (v5.0)
#
# Improvements:
#   • Multi-scale perturbations (local + global)
#   • Tangent-space dispersion using an orthonormal basis
#   • Optional recentering around previous ω̂ samples
#   • Full likelihood_spec() constraint support (bounds + ineq)
#
# This dramatically improves geometric coverage of the manifold
# ψ(ω̂) = ψ_MLE before solving with nloptr::auglag().
# ======================================================================


# ======================================================================
# Helper: Build Tangent-Space Basis
# ======================================================================

#' Compute an orthonormal basis for the tangent space at theta_mle
#'
#' @description
#' Given a gradient g = ∇ψ(θ_MLE), we find an orthonormal basis B
#' for the subspace:
#'     { v ∈ R^J : gᵀ v = 0 }
#'
#' If no ψ_jac exists, returns NULL.
#'
#' @keywords internal
#' @noRd
.tangent_basis <- function(theta_mle, psi_jac) {

  if (is.null(psi_jac)) return(NULL)

  g <- psi_jac(theta_mle)
  if (!is.numeric(g)) return(NULL)

  g <- as.numeric(g)
  J <- length(g)

  if (!all(is.finite(g)) || sqrt(sum(g * g)) == 0) return(NULL)

  # normalize gradient
  g <- g / sqrt(sum(g * g))

  # Build J x J matrix whose first column is g, others standard basis
  M <- cbind(g, diag(J)[, -1, drop = FALSE])   # J x J

  # QR decomposition: Q = [g | tangent basis]
  Q <- qr.Q(qr(M), complete = TRUE)            # J x J

  # Tangent basis = all columns except the first
  B <- Q[, -1, drop = FALSE]                   # J x (J-1)

  B
}


# ======================================================================
# 1. Initial-Guess Generator
# ======================================================================

#' Construct Advanced Initial-Guess Generator for ω̂ Sampling
#'
#' @description
#' This generator provides **geometrically diverse** initial guesses by:
#'
#'   • Combining *local* and *global* tangent-space perturbations
#'   • Using an orthonormal tangent basis at θ_MLE
#'   • Occasionally recentering around a previously sampled ω̂
#'   • Respecting model bounds (θ_lower, θ_upper)
#'
#' The result is far more uniform exploration of the manifold
#' ψ(ω̂) = ψ_MLE before projecting with auglag().
#'
#' @param cal A `calibrated_model`.
#'
#' @return A function \code{f(history, p_recenter = 0.1)} returning
#'         high-dispersion initial guesses.
#' @export
make_omega_hat_initgen <- function(cal) {

  theta_mle <- cal$theta_mle
  psi_jac   <- cal$psi_jac
  lik       <- cal$likelihood

  J      <- lik$theta_dim
  lower  <- lik$theta_lower %||% rep(-Inf, J)
  upper  <- lik$theta_upper %||% rep( Inf,  J)

  # Build tangent-space basis at θ_MLE (may be NULL)
  B <- .tangent_basis(theta_mle, psi_jac)

  # scales for multi-scale perturbations
  local_scale  <- 0.15
  global_scale <- 0.60

  function(history = NULL, p_recenter = 0.10) {

    # ----------------------------------------------------------
    # 1. Choose center: θ_MLE OR a previous ω̂
    # ----------------------------------------------------------
    if (!is.null(history) && runif(1) < p_recenter) {
      center <- history[[sample.int(length(history), 1)]]
    } else {
      center <- theta_mle
    }

    # ----------------------------------------------------------
    # 2. Base perturbation in tangent directions (if B exists)
    # ----------------------------------------------------------
    if (!is.null(B)) {

      # choose local vs global
      if (runif(1) < 0.70) {
        s <- local_scale
      } else {
        s <- global_scale
      }

      # tangent coefficients ~ Normal(0, s^2 I)
      a <- rnorm(ncol(B), sd = s)

      candidate <- center + c(B %*% a)

    } else {

      # fallback: multiplicative jitter if tangent info unavailable
      jitter    <- rlnorm(J, meanlog = 0, sdlog = 0.25) - 1
      candidate <- center * (1 + jitter)
    }

    # ----------------------------------------------------------
    # 3. Clip to model bounds
    # ----------------------------------------------------------
    candidate <- pmin(pmax(candidate, lower), upper)

    as.numeric(candidate)
  }
}


# ======================================================================
# 2. Omega-Hat Sampler
# ======================================================================

#' Construct ω̂ Sampler for ψ(ω̂) = ψ_MLE
#'
#' @description
#' Solves the manifold equation:
#'      ψ(ω̂) - ψ_MLE = 0
#' using `nloptr::auglag()` and incorporating:
#'
#'   • θ_lower / θ_upper bounds
#'   • inequality constraints h(θ) ≤ 0
#'   • Jacobians for constraints (if supplied)
#'   • optimizer_spec settings only for SLSQP/local solver
#'
#' @param cal A `calibrated_model`.
#'
#' @return A function \code{f(init_guess)} returning ω̂.
#' @export
make_omega_hat_sampler <- function(cal) {
  force(cal)

  local({

    lik     <- cal$likelihood
    psi_fn  <- cal$psi_fn
    psi_mle <- cal$psi_mle
    opt     <- cal$optimizer

    J <- lik$theta_dim

    lower <- lik$theta_lower %||% rep(-Inf, J)
    upper <- lik$theta_upper %||% rep( Inf, J)

    hin_fn    <- lik$ineq
    hinjac_fn <- lik$ineq_jac

    fn0 <- function(theta) 0
    heq_fn <- function(theta) psi_fn(theta) - psi_mle
    heqjac <- cal$psi_jac

    function(init_guess) {

      res <- nloptr::auglag(
        x0          = init_guess,
        fn          = fn0,
        heq         = heq_fn,
        heqjac      = heqjac,
        hin         = hin_fn,
        hinjac      = hinjac_fn,
        lower       = lower,
        upper       = upper,
        localsolver = opt$localsolver,
        localtol    = opt$localtol,
        control     = opt$control,
        deprecatedBehavior = FALSE
      )

      res$par
    }
  })
}

# ======================================================================
# END
# ======================================================================

# ======================================================================
# eval-omega-hat.R (v5.3)
#
# Improvements:
#   • Multi-scale perturbations (local + global)
#   • Tangent-space dispersion using an orthonormal basis
#   • Optional recentering around previous ω̂ samples
#   • Full parameter_spec() constraint support (bounds + eq + ineq)
#   • Branch-free heq / heqjac closures for solver efficiency
# ======================================================================

# ======================================================================
# Helper: Build Tangent-Space Basis
# ======================================================================

#' Compute an orthonormal basis for the tangent space at param_mle
#'
#' @keywords internal
#' @noRd
.tangent_basis <- function(param_mle, psi_jac) {
  if (is.null(psi_jac)) {
    return(NULL)
  }

  g <- psi_jac(param_mle)
  if (!is.numeric(g)) {
    return(NULL)
  }

  g <- as.numeric(g)
  J <- length(g)

  if (!all(is.finite(g)) || sqrt(sum(g * g)) == 0) {
    return(NULL)
  }

  g <- g / sqrt(sum(g * g))
  M <- cbind(g, diag(J)[, -1, drop = FALSE])
  Q <- qr.Q(qr(M), complete = TRUE)

  Q[, -1, drop = FALSE]
}

# ======================================================================
# 1. Initial-Guess Generator
# ======================================================================
#' Construct an Initial-Guess Generator for ω̂ Sampling
#'
#' @description
#' Creates a stochastic generator for initial guesses of the
#' zero-score expectation (ZSE) parameter \eqn{\hat\omega}.
#'
#' The generator produces candidate parameter vectors by perturbing
#' the MLE \eqn{\hat\theta} using a mixture of:
#' \itemize{
#'   \item \strong{Local tangent-space perturbations} aligned with the
#'         constraint manifold \eqn{\psi(\theta) = \psi_{\text{MLE}}}
#'   \item \strong{Occasional global jumps} to escape local structure
#'   \item \strong{Optional recentering} around previously accepted
#'         \eqn{\hat\omega} samples
#' }
#'
#' When a Jacobian for \eqn{\psi(\theta)} is available, perturbations are
#' restricted to the tangent space orthogonal to \eqn{\nabla\psi}, ensuring
#' first-order feasibility with respect to the ψ-constraint. If no Jacobian
#' is available, multiplicative jitter is applied directly in parameter
#' space.
#'
#' All generated candidates are clipped to respect parameter bounds
#' specified in the associated \code{parameter_spec()}.
#'
#' @param cal
#'   A calibrated model object. Must contain:
#'   \itemize{
#'     \item \code{cal$parameter$param_mle}
#'     \item \code{cal$parameter$param_dim}
#'     \item \code{cal$parameter$param_lower}, \code{param_upper} (optional)
#'     \item \code{cal$estimand$psi_jac} (optional)
#'   }
#'
#' @return
#' A function with signature
#' \code{function(history = NULL, p_recenter = 0.10)} that generates
#' a single numeric parameter vector suitable for use as an initial
#' guess in ω̂-constrained optimization.
#'
#' @details
#' The returned generator supports adaptive exploration:
#' \itemize{
#'   \item If \code{history} is supplied, previous ω̂ samples may be reused
#'         as perturbation centers with probability \code{p_recenter}.
#'   \item Local and global perturbation scales are mixed to balance
#'         stability and exploration.
#' }
#'
#' @keywords internal
make_omega_hat_initgen <- function(cal) {
  param <- cal$parameter
  estimand <- cal$estimand

  param_mle <- param$param_mle
  psi_jac <- estimand$psi_jac

  J <- param$param_dim
  lower <- param$param_lower %||% rep(-Inf, J)
  upper <- param$param_upper %||% rep(Inf, J)

  B <- .tangent_basis(param_mle, psi_jac)

  local_scale <- 0.15
  global_scale <- 0.60

  function(history = NULL, p_recenter = 0.10) {
    if (!is.null(history) && length(history) > 0 && runif(1) < p_recenter) {
      center <- history[[sample.int(length(history), 1)]]
    } else {
      center <- param_mle
    }

    if (!is.null(B)) {
      s <- if (runif(1) < 0.70) local_scale else global_scale
      a <- rnorm(ncol(B), sd = s)
      candidate <- center + c(B %*% a)
    } else {
      jitter <- rlnorm(J, 0, 0.25) - 1
      candidate <- center * (1 + jitter)
    }

    pmin(pmax(candidate, lower), upper)
  }
}

# ======================================================================
# 2. Omega-Hat Sampler
# ======================================================================
#' Construct an ω̂ Sampler via Constrained Augmented Lagrangian Optimization
#'
#' @description
#' Builds a closure that maps an initial parameter guess to a
#' zero-score expectation (ZSE) parameter \eqn{\hat\omega} by solving
#' a constrained optimization problem.
#'
#' The sampler enforces:
#' \itemize{
#'   \item The ψ-constraint: \eqn{\psi(\theta) = \psi_{\text{MLE}}}
#'   \item Any additional equality constraints declared in
#'         \code{parameter_spec()}
#'   \item Any inequality and bound constraints declared in
#'         \code{parameter_spec()}
#' }
#'
#' Optimization is performed using \code{nloptr::auglag()} with a
#' constant objective function. The solution therefore corresponds
#' to a feasible point satisfying all constraints, rather than a
#' likelihood maximizer.
#'
#' Constraint functions and Jacobians are constructed \strong{once}
#' and closed over, avoiding conditional logic inside the solver
#' evaluation loop for improved performance.
#'
#' @param cal
#'   A calibrated model object containing:
#'   \itemize{
#'     \item ψ mapping and Jacobian: \code{cal$estimand$psi_fn},
#'           \code{psi_jac}, \code{psi_mle}
#'     \item Parameter constraints: equality, inequality, and bounds
#'     \item Optimizer settings from \code{optimizer_spec()}
#'   }
#'
#' @return
#' A function with signature \code{function(init_guess)} that returns
#' a numeric vector representing a feasible ω̂ value satisfying all
#' constraints.
#'
#' @details
#' The returned sampler:
#' \itemize{
#'   \item Projects the supplied initial guess onto parameter bounds
#'   \item Solves the constrained feasibility problem using an augmented
#'         Lagrangian method
#'   \item Returns only the optimized parameter vector, discarding
#'         solver diagnostics
#' }
#'
#' This sampler is intended for repeated use inside Monte Carlo
#' integrated likelihood algorithms and branch construction routines.
#'
#' @keywords internal
make_omega_hat_sampler <- function(cal) {
  force(cal)

  local({
    param <- cal$parameter
    estimand <- cal$estimand
    opt <- cal$optimizer

    psi_fn <- estimand$psi_fn
    psi_jac <- estimand$psi_jac
    psi_mle <- estimand$psi_mle

    eq_fn <- param$eq
    eq_jac <- param$eq_jac

    hin_fn <- param$ineq
    hin_jac <- param$ineq_jac

    J <- param$param_dim
    lower <- param$param_lower %||% rep(-Inf, J)
    upper <- param$param_upper %||% rep(Inf, J)

    fn0 <- function(theta) 0.0

    # ------------------------------------------------------------
    # Construct heq_fn (branch-free)
    # ------------------------------------------------------------
    heq_fn <- if (is.null(eq_fn)) {
      function(theta) psi_fn(theta) - psi_mle
    } else {
      function(theta) {
        c(
          psi_fn(theta) - psi_mle,
          eq_fn(theta)
        )
      }
    }

    # ------------------------------------------------------------
    # Construct heqjac (branch-free)
    # ------------------------------------------------------------
    heqjac <- if (is.null(psi_jac) && is.null(eq_jac)) {
      NULL
    } else if (!is.null(psi_jac) && is.null(eq_jac)) {
      function(theta) {
        Jpsi <- psi_jac(theta)
        if (is.vector(Jpsi)) matrix(Jpsi, nrow = 1) else Jpsi
      }
    } else if (is.null(psi_jac) && !is.null(eq_jac)) {
      function(theta) {
        eq_jac(theta)
      }
    } else {
      function(theta) {
        Jpsi <- psi_jac(theta)
        if (is.vector(Jpsi)) {
          Jpsi <- matrix(Jpsi, nrow = 1)
        }
        rbind(Jpsi, eq_jac(theta))
      }
    }

    function(init_guess) {
      x0 <- as.numeric(init_guess)
      x0 <- pmax(x0, lower)
      x0 <- pmin(x0, upper)

      res <- nloptr::auglag(
        x0 = x0,
        fn = fn0,
        heq = heq_fn,
        heqjac = heqjac,
        hin = hin_fn,
        hinjac = hin_jac,
        lower = lower,
        upper = upper,
        localsolver = opt$localsolver,
        localtol = opt$localtol,
        control = opt$control,
        deprecatedBehavior = FALSE
      )

      res$par
    }
  })
}

# ======================================================================
# END
# ======================================================================

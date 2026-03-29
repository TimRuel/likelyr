# ======================================================================
# omega-hat-strategies.R — Built-in Omega-Hat Generation Strategies
#
# Houses all built-in constructors for omega-hat generation. Each
# constructor accepts individual named arguments (not a full model object)
# and returns a closure ready for use by calibrate_pipeline().
#
# Constructors:
#   omega_hat_initgen_gaussian  — tangent-space Gaussian perturbations
#   omega_hat_initgen_shape     — k-dominant shape-family vectors
#   omega_hat_sampler           — feasibility projection via auglag
#   omega_hat_permuter          — symmetry-based permutation diversity
# ======================================================================

# ======================================================================
# INTERNAL HELPERS
# ======================================================================

#' Orthonormal basis for the tangent space of the ψ constraint at param_mle
#'
#' @keywords internal
#' @noRd
.tangent_basis <- function(param_mle, psi_jac) {
  if (is.null(psi_jac)) {
    return(NULL)
  }

  g <- as.numeric(psi_jac(param_mle))
  if (!is.numeric(g) || !all(is.finite(g))) {
    return(NULL)
  }

  norm_g <- sqrt(sum(g * g))
  if (norm_g == 0) {
    return(NULL)
  }

  g <- g / norm_g
  J <- length(g)
  M <- cbind(g, diag(J)[, -1, drop = FALSE])
  Q <- qr.Q(qr(M), complete = TRUE)
  Q[, -1, drop = FALSE]
}

#' Permute a logit vector in full J-space and re-baseline to last category
#'
#' @keywords internal
#' @noRd
.permute_eta <- function(eta, perm) {
  J <- length(eta) + 1L
  eta_full <- c(as.numeric(eta), 0.0)
  eta_perm <- eta_full[perm]
  eta_perm <- eta_perm - eta_perm[J]
  eta_perm[seq_len(J - 1L)]
}

#' Solve for the dominant-category probability of a k-dominant shape vector
#'
#' @description
#' For Simpson's index D = sum(p_j^2), a k-dominant vector assigns
#' probability p to category k and (1-p)/(J-1) to each remaining category.
#' This satisfies D = p^2 + (1-p)^2/(J-1). Solving the resulting
#' quadratic gives p as a function of D and J.
#'
#' Returns NULL if no valid solution exists (D not achievable by a
#' k-dominant vector with the given J).
#'
#' @keywords internal
#' @noRd
.shape_dominant_p <- function(psi_mle, J) {
  # Jp^2 - 2p + (1 - psi_mle*(J-1)) = 0
  a <- J
  b <- -2
  cc <- 1 - psi_mle * (J - 1)
  dis <- b^2 - 4 * a * cc

  if (dis < 0) {
    return(NULL)
  }

  # Two roots — take the larger one (dominant category has higher p)
  p <- (-b + sqrt(dis)) / (2 * a)

  if (p <= 0 || p > 1) {
    return(NULL)
  }

  p
}

# ======================================================================
# 1. Gaussian Initgen
# ======================================================================

#' Omega-Hat Initial-Guess Generator: Gaussian Strategy
#'
#' @description
#' Generates initial guesses for omega-hat by perturbing \code{param_mle}
#' in the tangent space of the ψ constraint surface. When a Jacobian is
#' available, perturbations are restricted to the subspace orthogonal to
#' ∇ψ, providing first-order feasibility. Otherwise, isotropic Gaussian
#' jitter is applied directly.
#'
#' A mixture of local and global scales balances stability and exploration.
#' Optional recentering around previously accepted omega-hats (via
#' \code{history}) biases toward unexplored regions when \code{p_far_recenter}
#' is high.
#'
#' @param param_mle    Numeric vector. MLE of the full parameter.
#' @param param_dim    Integer. Length of param_mle.
#' @param param_lower  Optional numeric vector. Lower bounds.
#' @param param_upper  Optional numeric vector. Upper bounds.
#' @param psi_jac      Optional function(param) → gradient ∇ψ(θ).
#' @param local_scale  Positive scalar. SD for local perturbations.
#' @param global_scale Positive scalar. SD for global perturbations.
#' @param p_local      Numeric in (0,1). Probability of local vs global scale.
#' @param return_intent Logical. If TRUE, returns a named list with
#'   \code{x0}, \code{dir}, and \code{move_type} (intent object).
#'   If FALSE, returns a plain numeric vector.
#' @param p_recenter   Numeric in [0,1). Probability of recentering around
#'   a history point rather than param_mle.
#' @param p_far_recenter Numeric in (0,1]. When recentering, probability of
#'   choosing the farthest history point (vs a random one).
#' @param ... Unused. Absorbed for forward compatibility.
#'
#' @return A closure \code{function(history = NULL)} returning either an
#'   intent list or a numeric vector, depending on \code{return_intent}.
#'
#' @export
omega_hat_initgen_gaussian <- function(
  param_mle,
  param_dim,
  param_lower = NULL,
  param_upper = NULL,
  psi_jac = NULL,
  local_scale = 0.15,
  global_scale = 0.60,
  p_local = 0.70,
  return_intent = TRUE,
  p_recenter = 0.10,
  p_far_recenter = 0.75,
  ...
) {
  lower <- param_lower %||% rep(-Inf, param_dim)
  upper <- param_upper %||% rep(Inf, param_dim)
  B <- .tangent_basis(param_mle, psi_jac)

  function(history = NULL) {
    # --- choose perturbation center ---
    center <- param_mle
    if (!is.null(history) && length(history) > 0 && runif(1) < p_recenter) {
      if (runif(1) < p_far_recenter && length(history) > 1) {
        # farthest point from param_mle (encourages unexplored regions)
        dists <- vapply(history, function(h) sum((h - param_mle)^2), numeric(1))
        center <- history[[which.max(dists)]]
      } else {
        center <- history[[sample.int(length(history), 1L)]]
      }
    }

    # --- perturb in tangent space or isotropically ---
    s <- if (runif(1) < p_local) local_scale else global_scale

    if (!is.null(B)) {
      a <- rnorm(ncol(B), sd = s)
      candidate <- center + c(B %*% a)
    } else {
      candidate <- center + rnorm(param_dim, sd = s)
    }

    candidate <- pmin(pmax(candidate, lower), upper)

    if (!isTRUE(return_intent)) {
      return(candidate)
    }

    # unit direction for sampler warm-starting
    v <- rnorm(param_dim)
    v <- v / sqrt(sum(v * v))

    list(x0 = candidate, dir = v, move_type = "gaussian")
  }
}

# ======================================================================
# 2. Shape-Family Initgen
# ======================================================================

#' Omega-Hat Initial-Guess Generator: Shape-Family Strategy
#'
#' @description
#' Generates initial guesses from k-dominant canonical probability vectors
#' that analytically satisfy ψ(x0) ≈ ψ_mle. For each of the J categories,
#' a "k-dominant" vector assigns the dominant probability mass to category k
#' with the remainder shared equally, then jitters the result to break
#' exact symmetry.
#'
#' This provides structurally diverse initial guesses that span different
#' basins of the constraint surface — complementing the Gaussian strategy,
#' which stays local to param_mle.
#'
#' With probability \code{1 - p_shape}, falls back to a simple Gaussian
#' perturbation of param_mle.
#'
#' @note Currently implemented for the Simpson's-index (sum-of-squares)
#'   psi function. The k-dominant construction assumes psi_fn computes
#'   D = sum(p_j^2) on the simplex. For other psi functions, the analytic
#'   dominant-p derivation will not be exact — the sampler's feasibility
#'   projection will correct small discrepancies.
#'
#' @param param_mle   Numeric vector. MLE of the full parameter (logit space).
#' @param param_dim   Integer. Length of param_mle (= J - 1).
#' @param psi_mle     Numeric scalar. ψ(param_mle).
#' @param param_lower Optional numeric vector. Lower bounds.
#' @param param_upper Optional numeric vector. Upper bounds.
#' @param p_shape     Numeric in (0,1]. Probability of shape-family path
#'   vs Gaussian fallback.
#' @param shape_noise Nonnegative scalar. SD of jitter applied to the
#'   shape-family probability vector before logit conversion. Breaks
#'   exact within-family symmetry.
#' @param return_intent Logical. See \code{omega_hat_initgen_gaussian}.
#' @param p_recenter    Numeric in [0,1). Probability of recentering.
#' @param p_far_recenter Numeric in (0,1]. Probability of choosing the
#'   farthest history point when recentering.
#' @param local_scale  Positive scalar. SD for Gaussian fallback.
#' @param ... Unused. Absorbed for forward compatibility.
#'
#' @return A closure \code{function(history = NULL)} returning an intent
#'   list or numeric vector.
#'
#' @export
omega_hat_initgen_shape <- function(
  param_mle,
  param_dim,
  psi_mle,
  param_lower = NULL,
  param_upper = NULL,
  p_shape = 0.70,
  shape_noise = 0.05,
  return_intent = TRUE,
  p_recenter = 0.10,
  p_far_recenter = 0.75,
  local_scale = 0.15,
  ...
) {
  J <- param_dim + 1L
  lower <- param_lower %||% rep(-Inf, param_dim)
  upper <- param_upper %||% rep(Inf, param_dim)

  # Pre-compute dominant probability for the current psi_mle and J.
  # If no valid solution exists, shape-family path is unavailable and
  # we fall back to Gaussian unconditionally.
  p_dom <- .shape_dominant_p(psi_mle, J)

  function(history = NULL) {
    # --- optional recentering ---
    center <- param_mle
    if (!is.null(history) && length(history) > 0 && runif(1) < p_recenter) {
      if (runif(1) < p_far_recenter && length(history) > 1) {
        dists <- vapply(history, function(h) sum((h - param_mle)^2), numeric(1))
        center <- history[[which.max(dists)]]
      } else {
        center <- history[[sample.int(length(history), 1L)]]
      }
    }

    use_shape <- !is.null(p_dom) && runif(1) < p_shape

    if (use_shape) {
      # --- shape-family path ---
      # Pick a random dominant category k
      k <- sample.int(J, 1L)
      q_rem <- (1 - p_dom) / (J - 1)

      # Build probability vector: p_dom at position k, q_rem elsewhere
      probs <- rep(q_rem, J)
      probs[k] <- p_dom

      # Jitter to break exact symmetry (and so sampler has work to do)
      if (shape_noise > 0) {
        noise <- rnorm(J, sd = shape_noise)
        probs <- probs + noise
        probs <- pmax(probs, 1e-8)
        probs <- probs / sum(probs)
      }

      # Convert to logit (eta) space: eta_j = log(p_j / p_J)
      log_probs <- log(probs)
      candidate <- log_probs[seq_len(J - 1L)] - log_probs[J]
      candidate <- pmin(pmax(candidate, lower), upper)
      move_type <- "shape_family"
    } else {
      # --- Gaussian fallback ---
      candidate <- center + rnorm(param_dim, sd = local_scale)
      candidate <- pmin(pmax(candidate, lower), upper)
      move_type <- "gaussian"
    }

    if (!isTRUE(return_intent)) {
      return(candidate)
    }

    v <- rnorm(param_dim)
    v <- v / sqrt(sum(v * v))

    list(x0 = candidate, dir = v, move_type = move_type)
  }
}

# ======================================================================
# 3. Sampler (feasibility projection)
# ======================================================================

#' Omega-Hat Sampler: Feasibility Projection via Auglag
#'
#' @description
#' Projects an initial guess onto the constraint surface
#' {θ : ψ(θ) = ψ_mle} using \code{nloptr::auglag()} with a zero
#' objective. The result is a feasible omega-hat satisfying all
#' constraints declared in \code{parameter_spec()}.
#'
#' @param psi_fn    Function(param) → scalar ψ(θ). Data-bound.
#' @param psi_jac   Optional function(param) → gradient ∇ψ(θ).
#' @param psi_mle   Numeric scalar. Target ψ value.
#' @param eq_fn     Optional equality constraint function.
#' @param eq_jac    Optional equality constraint Jacobian.
#' @param ineq_fn   Optional inequality constraint function.
#' @param ineq_jac  Optional inequality constraint Jacobian.
#' @param solver    A \code{solver_spec} object.
#' @param param_lower Optional numeric vector. Lower bounds.
#' @param param_upper Optional numeric vector. Upper bounds.
#' @param param_dim   Integer. Parameter dimension.
#' @param attach_diagnostics Logical. If TRUE, returns a list with
#'   \code{par} and solver metadata instead of just \code{par}.
#'   Default: FALSE.
#' @param ... Unused. Absorbed for forward compatibility.
#'
#' @return A closure \code{function(init_guess)} returning a numeric
#'   vector (the projected omega-hat), or a diagnostics list if
#'   \code{attach_diagnostics = TRUE}.
#'
#' @export
omega_hat_sampler <- function(
  psi_fn,
  psi_jac = NULL,
  psi_mle,
  eq_fn = NULL,
  eq_jac = NULL,
  ineq_fn = NULL,
  ineq_jac = NULL,
  solver,
  param_lower = NULL,
  param_upper = NULL,
  param_dim,
  attach_diagnostics = FALSE,
  ...
) {
  lower <- param_lower %||% rep(-Inf, param_dim)
  upper <- param_upper %||% rep(Inf, param_dim)

  # Build equality constraint closure (branch-free)
  heq <- if (is.null(eq_fn)) {
    function(theta) psi_fn(theta) - psi_mle
  } else {
    function(theta) c(psi_fn(theta) - psi_mle, eq_fn(theta))
  }

  heqjac <- if (is.null(psi_jac) && is.null(eq_jac)) {
    NULL
  } else if (!is.null(psi_jac) && is.null(eq_jac)) {
    function(theta) {
      Jpsi <- psi_jac(theta)
      if (is.vector(Jpsi)) matrix(Jpsi, nrow = 1L) else Jpsi
    }
  } else if (is.null(psi_jac) && !is.null(eq_jac)) {
    function(theta) eq_jac(theta)
  } else {
    function(theta) {
      Jpsi <- psi_jac(theta)
      if (is.vector(Jpsi)) {
        Jpsi <- matrix(Jpsi, nrow = 1L)
      }
      rbind(Jpsi, eq_jac(theta))
    }
  }

  fn0 <- function(theta) 0.0

  function(init_guess) {
    x0 <- as.numeric(if (is.list(init_guess)) init_guess$x0 else init_guess)
    x0 <- pmin(pmax(x0, lower), upper)

    res <- nloptr::auglag(
      x0 = x0,
      fn = fn0,
      heq = heq,
      heqjac = heqjac,
      hin = ineq_fn,
      hinjac = ineq_jac,
      lower = lower,
      upper = upper,
      localsolver = solver$localsolver,
      localtol = solver$localtol,
      control = solver$control,
      deprecatedBehavior = FALSE
    )

    if (attach_diagnostics) {
      list(
        par = as.numeric(res$par),
        solver_status = res$status %||% NA_integer_,
        solver_message = res$message %||% NA_character_
      )
    } else {
      as.numeric(res$par)
    }
  }
}

# ======================================================================
# 4. Permuter (symmetry-based diversity)
# ======================================================================

#' Omega-Hat Permuter: Symmetry-Based Diversity
#'
#' @description
#' Generates permuted variants of an accepted omega-hat by applying
#' random label permutations in the full J-category space. Exploits
#' the permutation symmetry of Simpson's index (and other symmetric
#' estimands) to efficiently diversify the omega-hat pool without
#' additional constrained optimization calls.
#'
#' @param param_dim        Integer. Parameter dimension (= J - 1).
#' @param max_perms        Integer. Number of random permutations to generate.
#' @param include_identity Logical. If TRUE, the identity permutation is
#'   always included. Default: FALSE.
#' @param seed             Optional integer seed. Applied per-call if
#'   non-NULL (reproducible permutations). Default: NULL (fresh each call).
#' @param ...              Unused.
#'
#' @return A closure \code{function(omega_hat)} returning a list of
#'   \code{list(omega_hat, perm)} objects, one per permutation.
#'
#' @export
omega_hat_permuter <- function(
  param_dim,
  max_perms = 20L,
  include_identity = FALSE,
  seed = NULL,
  ...
) {
  J_full <- param_dim + 1L

  function(omega_hat) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    eta <- as.numeric(omega_hat)

    perms <- replicate(
      max_perms,
      sample.int(J_full, J_full, replace = FALSE),
      simplify = FALSE
    )

    if (include_identity) {
      perms <- c(list(seq_len(J_full)), perms)
    }

    lapply(perms, function(p) {
      list(omega_hat = .permute_eta(eta, p), perm = p)
    })
  }
}

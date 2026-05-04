# ======================================================================
# profile-traverse.R — Profile Log-Likelihood Traversal
#
# Provides:
#   traverse_profile_side() — one-sided profile sweep with
#                             monotonicity enforcement via jitter retries
# ======================================================================

#' One-Sided Profile Log-Likelihood Sweep Along the ψ-Grid
#'
#' @description
#' Performs a one-sided continuation sweep of the profile log-likelihood
#' by moving outward from the MLE ψ̂ along a fixed ψ-grid. At each grid
#' point ψ_k, the constrained optimization is solved using a warm start
#' derived from the previous solution.
#'
#' Monotonicity is enforced via jitter retries: if a proposed step
#' increases the log-likelihood relative to the previous value, the
#' initial conditions are perturbed and the evaluation retried up to
#' \code{max_retries} times.
#'
#' An optional \code{warmstart_fn} may be supplied to improve the warm
#' start at each step. When provided, it is called before each optimizer
#' evaluation to predict a better initial parameter vector based on the
#' previous solution and the step in ψ. A typical use case is a tangent
#' predictor derived from the implicit function theorem:
#' \deqn{
#'   \hat{\theta}_{k+1} \approx \hat{\theta}_k +
#'   \frac{\delta\psi}{\|\nabla\psi(\hat{\theta}_k)\|^2}
#'   \nabla\psi(\hat{\theta}_k)
#' }
#' which moves the parameter along the gradient of ψ by the amount
#' needed to reach ψ_{k+1} to first order. If \code{warmstart_fn}
#' throws an error, the sweep falls back to using the previous
#' \code{param_hat} directly.
#'
#' @param grid             ψ-grid object from \code{psi_grid_anchor()}.
#' @param k_start          Integer. Starting grid index (+1 or -1).
#' @param cutoff           Numeric scalar. Stopping threshold — sweep
#'   terminates once log-likelihood falls below this value.
#' @param init_guess       Numeric vector. Warm-start parameter at ψ̂.
#' @param profile_evaluator Function \code{(psi, param_init) ->
#'   list(param_hat, branch_val)}.
#' @param max_retries      Non-negative integer. Maximum jitter retries
#'   when monotonicity is violated.
#' @param stop_at_bounds   Logical. Stop when a ψ bound is reached.
#'   Default: \code{TRUE}.
#' @param eval_at_bounds   Logical. Evaluate once at the ψ bound before
#'   stopping. Requires \code{stop_at_bounds = TRUE}.
#'   Default: \code{TRUE}.
#' @param warmstart_fn     Optional function
#'   \code{(psi_curr, psi_next, param_curr) -> numeric vector}.
#'   When supplied, called at each step to predict a warm start for
#'   the next constrained optimization. When \code{NULL} (default),
#'   the previous \code{param_hat} is used directly.
#'
#' @return A tibble with columns \code{k} and \code{loglik}, sorted by
#'   \code{k}, with duplicate indices removed.
#'
#' @keywords internal
traverse_profile_side <- function(
  grid,
  k_start,
  cutoff,
  init_guess,
  profile_evaluator,
  max_retries,
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE,
  warmstart_fn = NULL
) {
  k_direction <- sign(k_start)
  k_curr <- k_start
  current_par <- init_guess
  current_val <- Inf

  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper

  df <- tibble::tibble(k = integer(), loglik = numeric())

  repeat {
    retry <- 0L
    psi_k <- grid$psi_mle + k_curr * grid$increment

    hit_lower <- !is.null(psi_lower) && psi_k < psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k > psi_upper

    if (hit_lower || hit_upper) {
      if (stop_at_bounds) {
        if (eval_at_bounds) {
          psi_k <- if (hit_lower) psi_lower else psi_upper
        } else {
          break
        }
      }
    }

    # Compute warm start for this step
    psi_prev <- grid$psi_mle + (k_curr - k_direction) * grid$increment
    warm_init <- if (!is.null(warmstart_fn)) {
      tryCatch(
        warmstart_fn(psi_prev, psi_k, current_par),
        error = function(e) current_par
      )
    } else {
      current_par
    }

    # Evaluate with monotonicity enforcement
    repeat {
      eval <- profile_evaluator(psi_k, warm_init)
      if (eval$branch_val < current_val || retry >= max_retries) {
        break
      }
      retry <- retry + 1L
      warm_init <- warm_init +
        stats::rnorm(length(warm_init), sd = 0.1 * retry)
    }

    if (eval$branch_val > current_val && max_retries > 0L) {
      warning(
        sprintf(
          "traverse_profile_side(): monotonicity violation at k=%d after %d retries.",
          k_curr,
          retry
        ),
        call. = FALSE
      )
      eval <- profile_evaluator(psi_k, warm_init)
    }

    current_val <- eval$branch_val

    if (!is.finite(current_val)) {
      stop(
        "traverse_profile_side(): non-finite log-likelihood at k = ",
        k_curr,
        call. = FALSE
      )
    }

    df <- df |>
      dplyr::add_row(k = k_curr, loglik = current_val)

    if (!is.null(cutoff) && current_val < cutoff) {
      break
    }
    if (stop_at_bounds && (hit_lower || hit_upper)) {
      break
    }

    current_par <- eval$param_hat
    k_curr <- k_curr + k_direction
  }

  df |>
    dplyr::distinct() |>
    dplyr::arrange(k)
}

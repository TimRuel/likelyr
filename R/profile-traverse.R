# ======================================================================
# profile-traverse.R — Profile Log-Likelihood Traversal
#
# Provides:
#   traverse_profile_side() — one-sided profile sweep with
#                             monotonicity enforcement via jitter retries
#                             and skip-on-exhaustion
# ======================================================================

#' One-Sided Profile Log-Likelihood Sweep Along the ψ-Grid
#'
#' @description
#' Performs a one-sided continuation sweep of the profile log-likelihood
#' by moving outward from the MLE ψ̂ along a fixed ψ-grid. At each grid
#' point ψ_k, the constrained optimization is solved using the previous
#' accepted solution as a warm start.
#'
#' Monotonicity is enforced via jitter retries: if a proposed step
#' increases the log-likelihood relative to the previous accepted value,
#' the initial conditions are perturbed and the evaluation retried up to
#' \code{max_retries} times. If retries are exhausted and the value is
#' still non-monotone, the point is skipped entirely — no value is
#' recorded and the warm start is not updated. This leaves a small hole
#' in the profile rather than a flat plateau, which is more honest and
#' does not distort spline fitting.
#'
#' @param grid             ψ-grid object from \code{psi_grid_anchor()}.
#' @param k_start          Integer. Starting grid index (+1 or -1).
#' @param cutoff           Numeric scalar. Stopping threshold — sweep
#'   terminates once log-likelihood falls below this value.
#' @param init_guess       Numeric vector. Warm-start parameter at ψ̂.
#' @param profile_evaluator Function \code{(psi, param_init) ->
#'   list(param_hat, branch_val)}.
#' @param max_retries      Non-negative integer. Maximum jitter retries
#'   when monotonicity is violated. If all retries are exhausted, the
#'   point is skipped.
#' @param drop_multiplier  Positive numeric scalar. A step whose drop
#'   exceeds this multiple of the recent median drop is skipped as
#'   implausibly large (optimizer jumped to a different local minimum).
#'   Only applied once at least 3 accepted steps have accumulated.
#'   Default: \code{5.0}.
#' @param stop_at_bounds   Logical. Stop when a ψ bound is reached.
#'   Default: \code{TRUE}.
#' @param eval_at_bounds   Logical. Evaluate once at the ψ bound before
#'   stopping. Requires \code{stop_at_bounds = TRUE}.
#'   Default: \code{TRUE}.
#'
#' @return A tibble with columns \code{k} and \code{loglik}, sorted by
#'   \code{k}, with duplicate indices removed. Skipped points produce
#'   no row.
#'
#' @keywords internal
traverse_profile_side <- function(
  grid,
  k_start,
  cutoff,
  init_guess,
  profile_evaluator,
  max_retries,
  drop_multiplier = 5.0,
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE
) {
  k_direction <- sign(k_start)
  k_curr <- k_start
  current_par <- init_guess
  current_val <- Inf
  recent_drops <- numeric(0)

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

    # -------------------------------------------------------------------
    # Evaluate with monotonicity enforcement via jitter retries.
    # Keep the last-accepted param as warm start for retries so that
    # perturbations are anchored to a known-good point.
    # -------------------------------------------------------------------
    retry_par <- current_par
    repeat {
      eval <- profile_evaluator(psi_k, retry_par)
      if (eval$branch_val < current_val || retry >= max_retries) {
        break
      }
      retry <- retry + 1L
      retry_par <- current_par +
        stats::rnorm(length(current_par), sd = 0.1 * retry)
    }

    # -------------------------------------------------------------------
    # If retries exhausted and still non-monotone: skip this point.
    # Also skip if the drop is implausibly large relative to recent
    # steps — this catches cases where the optimizer jumped to a
    # different local minimum while still satisfying monotonicity.
    # Only apply the drop check once enough history has accumulated.
    # -------------------------------------------------------------------
    drop <- current_val - eval$branch_val # positive = downward step

    is_non_monotone <- eval$branch_val >= current_val && retry >= max_retries
    is_implausible <- if (length(recent_drops) >= 3L) {
      ref <- median(recent_drops)
      ref > 0 && drop > drop_multiplier * ref
    } else {
      FALSE
    }

    if (is_non_monotone || is_implausible) {
      k_curr <- k_curr + k_direction
      next
    }

    if (!is.finite(eval$branch_val)) {
      stop(
        "traverse_profile_side(): non-finite log-likelihood at k = ",
        k_curr,
        call. = FALSE
      )
    }

    current_val <- eval$branch_val
    current_par <- eval$param_hat

    if (drop > 0) {
      recent_drops <- c(recent_drops, drop)
      if (length(recent_drops) > 10L) {
        recent_drops <- tail(recent_drops, 10L)
      }
    }

    df <- dplyr::add_row(df, k = k_curr, loglik = current_val)

    if (!is.null(cutoff) && current_val < cutoff) {
      break
    }
    if (stop_at_bounds && (hit_lower || hit_upper)) {
      break
    }

    k_curr <- k_curr + k_direction
  }

  df |>
    dplyr::distinct() |>
    dplyr::arrange(k)
}

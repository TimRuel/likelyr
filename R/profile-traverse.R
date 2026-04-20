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
#' @param spline_tol       Positive numeric scalar. A candidate point
#'   is skipped if it falls more than this many log-likelihood units
#'   below the spline prediction at that ψ value. Only applied once
#'   \code{spline_min_pts} accepted points have accumulated.
#'   Default: \code{1.0}.
#' @param spline_min_pts   Positive integer. Minimum number of accepted
#'   points required before the spline prediction check is applied.
#'   Default: \code{8L}.
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
  spline_tol = 1.0,
  spline_min_pts = 8L,
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE
) {
  k_direction <- sign(k_start)
  k_curr <- k_start
  current_par <- init_guess
  current_val <- Inf
  accepted_psi <- numeric(0)
  accepted_ll <- numeric(0)

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
    # Skip if retries exhausted and still non-monotone.
    # Also skip if a spline fitted to recent accepted points predicts
    # the value at psi_k and the candidate falls more than spline_tol
    # below that prediction — this catches optimizer jumps to a bad
    # local minimum that happen to be monotone.
    # The spline check only fires once spline_min_pts points have been
    # accepted, so it doesn't trigger in the early sweep.
    # -------------------------------------------------------------------
    is_non_monotone <- eval$branch_val >= current_val && retry >= max_retries

    is_implausible <- if (
      !is_non_monotone &&
        length(accepted_psi) >= spline_min_pts
    ) {
      spline_fit <- tryCatch(
        stats::smooth.spline(accepted_psi, accepted_ll),
        error = function(e) NULL
      )
      if (!is.null(spline_fit)) {
        predicted <- stats::predict(spline_fit, psi_k)$y
        eval$branch_val < predicted - spline_tol
      } else {
        FALSE
      }
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

    accepted_psi <- c(accepted_psi, psi_k)
    accepted_ll <- c(accepted_ll, current_val)
    if (length(accepted_psi) > spline_min_pts * 3L) {
      accepted_psi <- tail(accepted_psi, spline_min_pts * 3L)
      accepted_ll <- tail(accepted_ll, spline_min_pts * 3L)
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

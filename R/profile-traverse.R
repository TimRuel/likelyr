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
#' point ψ_k, the constrained optimization is solved using the previous
#' solution as a warm start.
#'
#' Monotonicity is enforced via jitter retries: if a proposed step
#' increases the log-likelihood relative to the previous value, the
#' initial conditions are perturbed and the evaluation retried up to
#' \code{max_retries} times.
#'
#' @param grid           ψ-grid object from \code{psi_grid_anchor()}.
#' @param k_start        Integer. Starting grid index (+1 or -1).
#' @param cutoff         Numeric scalar. Stopping threshold — sweep
#'   terminates once log-likelihood falls below this value.
#' @param init_guess     Numeric vector. Warm-start parameter at ψ̂.
#' @param branch_evaluator Function \code{(psi, param_init) ->
#'   list(param_hat, branch_val)}.
#' @param max_retries    Non-negative integer. Maximum jitter retries
#'   when monotonicity is violated.
#' @param stop_at_bounds Logical. Stop when a ψ bound is reached.
#'   Default: \code{TRUE}.
#' @param eval_at_bounds Logical. Evaluate once at the ψ bound before
#'   stopping. Requires \code{stop_at_bounds = TRUE}.
#'   Default: \code{TRUE}.
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
  eval_at_bounds = TRUE
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

    # Evaluate with monotonicity enforcement
    repeat {
      eval <- profile_evaluator(psi_k, current_par)
      if (eval$branch_val <= current_val || retry >= max_retries) {
        break
      }
      retry <- retry + 1L
      current_par <- current_par +
        stats::rnorm(length(current_par), sd = 0.1 * retry)
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
      eval <- profile_evaluator(psi_k, current_par)
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
      dplyr::add_row(
        k = k_curr,
        loglik = current_val
      )

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

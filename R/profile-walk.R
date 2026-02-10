# ======================================================================
# One-Sided Profile Log-Likelihood Sweep (Internal)
# ======================================================================

#' One-Sided Profile Log-Likelihood Sweep Along the ψ-Grid
#'
#' @description
#' Performs a one-sided continuation sweep of the **profile
#' log-likelihood** by moving outward from the MLE ψ̂ along a fixed
#' ψ-grid. At each grid location ψ_k, the constrained optimization
#' problem is solved using the previous solution as a warm start,
#' enforcing continuity along the manifold \eqn{ψ(θ) = ψ_k}.
#'
#' Monotonicity of the profile log-likelihood is enforced via limited
#' jittering of the initial conditions. If a proposed step increases
#' the log-likelihood relative to the previous value, the evaluation
#' is retried up to \code{max_retries} times using increasingly large
#' perturbations.
#'
#' ψ-bound geometry is handled explicitly via \code{stop_at_bounds}
#' and \code{eval_at_bounds}, mirroring the behavior of
#' \code{walk_branch_side()}.
#'
#' @param grid
#'   Grid specification list containing at least:
#'   \itemize{
#'     \item \code{psi_mle} — numeric scalar ψ̂,
#'     \item \code{increment} — grid spacing Δψ,
#'     \item \code{psi_lower} — optional lower ψ bound,
#'     \item \code{psi_upper} — optional upper ψ bound.
#'   }
#'
#' @param k_start
#'   Integer grid index at which to start the sweep (typically
#'   \code{+1} or \code{-1}).
#'
#' @param cutoff
#'   Numeric scalar giving the stopping threshold for the profile
#'   log-likelihood. The sweep terminates once the log-likelihood
#'   falls below this value.
#'
#' @param init_guess
#'   Numeric vector giving the initial constrained optimizer solution
#'   at ψ̂, used as the warm start for continuation.
#'
#' @param branch_fn
#'   Function with signature
#'   \code{function(psi, param_init)} returning a list with elements:
#'   \itemize{
#'     \item \code{param_hat} — constrained optimizer solution θ̂,
#'     \item \code{branch_val} — profile log-likelihood at ψ.
#'   }
#'
#' @param max_retries
#'   Non-negative integer giving the maximum number of jitter retries
#'   allowed when monotonicity is violated.
#'
#' @param stop_at_bounds
#'   Logical scalar. If TRUE (default), the sweep stops when a ψ bound
#'   is reached.
#'
#' @param eval_at_bounds
#'   Logical scalar. If TRUE (default), the profile log-likelihood is
#'   evaluated once at the ψ bound before stopping. Requires
#'   \code{stop_at_bounds = TRUE}.
#'
#' @return
#' A tibble with columns:
#' \itemize{
#'   \item \code{k} — integer ψ-grid index,
#'   \item \code{loglik} — profile log-likelihood at ψ_k.
#' }
#'
#' Rows are sorted by \code{k}. Duplicate grid indices (if any) are
#' removed.
#'
#' @keywords internal
walk_profile_side <- function(
  grid,
  k_start,
  cutoff,
  init_guess,
  branch_fn,
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

    # --------------------------------------------------------------
    # Convert k → ψ
    # --------------------------------------------------------------
    psi_k <- grid$psi_mle + k_curr * grid$increment

    # --------------------------------------------------------------
    # Geometry: ψ bounds
    # --------------------------------------------------------------
    hit_lower <- !is.null(psi_lower) && psi_k < psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k > psi_upper

    if (hit_lower || hit_upper) {
      if (!stop_at_bounds) {
        # ignore geometry
      } else {
        if (eval_at_bounds) {
          psi_k <- if (hit_lower) psi_lower else psi_upper
        } else {
          break
        }
      }
    }

    # --------------------------------------------------------------
    # Evaluate with monotonicity enforcement
    # --------------------------------------------------------------
    repeat {
      eval <- branch_fn(psi_k, current_par)

      if (eval$branch_val <= current_val || retry >= max_retries) {
        break
      }

      retry <- retry + 1L
      scale <- 0.1 * retry

      current_par <- current_par +
        stats::rnorm(
          n = length(current_par),
          sd = scale
        )
    }

    # Final fallback if monotonicity still violated
    if (eval$branch_val > current_val && max_retries > 0L) {
      warning(
        sprintf(
          "Profile monotonicity violation at k=%d after %d retries; using fallback.",
          k_curr,
          retry
        ),
        call. = FALSE
      )
      eval <- branch_fn(psi_k, current_par)
    }

    # --------------------------------------------------------------
    # Update
    # --------------------------------------------------------------
    current_val <- eval$branch_val

    if (!is.finite(current_val)) {
      stop(
        "walk_profile_side(): Non-finite log-likelihood at k = ",
        k_curr,
        " (value = ",
        current_val,
        ").",
        call. = FALSE
      )
    }

    df <- dplyr::add_row(df, k = k_curr, loglik = current_val)

    # --------------------------------------------------------------
    # Likelihood cutoff
    # --------------------------------------------------------------
    if (!is.null(cutoff) && current_val < cutoff) {
      break
    }

    # --------------------------------------------------------------
    # Stop after evaluating exactly at a bound
    # --------------------------------------------------------------
    if (stop_at_bounds && (hit_lower || hit_upper)) {
      break
    }

    # --------------------------------------------------------------
    # Prepare next step
    # --------------------------------------------------------------
    current_par <- eval$param_hat
    k_curr <- k_curr + k_direction
  }

  dplyr::distinct(df) |>
    dplyr::arrange(.data$k)
}

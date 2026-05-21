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
#' by moving outward from the mode along a fixed ψ-grid. At each grid
#' point ψ_k, the constrained optimization is solved using a warm start
#' derived from the previous solution.
#'
#' Jitter retries are triggered by any combination of the following,
#' controlled by \code{profile_retry_on}:
#' \enumerate{
#'   \item \code{"monotonicity"}: the proposed step increases the
#'     log-likelihood relative to the previous value.
#'   \item \code{"constraint"}: the psi residual at the returned
#'     solution exceeds \code{resid_tol}, indicating auglag found a
#'     feasible-but-wrong local optimum.
#'   \item \code{"drop"}: the proposed drop exceeds \code{max_drop_frac}
#'     times the recent median drop (once at least three recent drops
#'     are available).
#' }
#' The warm start chain only advances when the constraint was satisfied
#' (\code{psi_resid <= resid_tol}), preventing constraint failures from
#' corrupting subsequent steps.
#'
#' @param grid             ψ-grid object from \code{psi_grid_anchor()}.
#' @param k_start          Integer. Starting grid index (+1 or -1).
#' @param cutoff           Numeric scalar. Stopping threshold.
#' @param init_guess       Numeric vector. Warm-start parameter at mode.
#' @param profile_evaluator Function \code{(psi, param_init) ->
#'   list(param_hat, branch_val, psi_residual, E_loglik_at_hat,
#'   solver_iterations)}.
#' @param max_retries      Non-negative integer. Maximum jitter retries
#'   per step.
#' @param stop_at_bounds   Logical. Default: \code{TRUE}.
#' @param eval_at_bounds   Logical. Evaluate once at the ψ bound before
#'   stopping. Requires \code{stop_at_bounds = TRUE}. The boundary
#'   evaluation bypasses all retry and monotonicity checks and breaks
#'   immediately after recording the result, preventing the boundary
#'   point from being evaluated twice on consecutive grid steps.
#'   Default: \code{TRUE}.
#' @param warmstart_fn     Optional function
#'   \code{(psi_curr, psi_next, param_curr) -> numeric vector}.
#' @param max_drop_frac    Positive numeric scalar. Drop threshold
#'   multiplier. Set to \code{Inf} to disable. Default: \code{10.0}.
#' @param resid_tol        Non-negative numeric scalar. Constraint
#'   residual tolerance. Default: \code{1e-3}.
#' @param profile_retry_on Character vector. Which violations trigger
#'   jitter retries. Any subset of \code{c("monotonicity",
#'   "constraint", "drop")}. Default: all three.
#' @param verbose          Logical. Print a row per grid point.
#'   Default: \code{FALSE}.
#'
#' @return A tibble with columns \code{k}, \code{psi}, \code{loglik}.
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
  warmstart_fn = NULL,
  max_drop_frac = 10.0,
  resid_tol = 1e-3,
  profile_retry_on = c("monotonicity", "constraint", "drop"),
  verbose = FALSE
) {
  k_direction <- sign(k_start)
  k_curr <- k_start
  current_par <- init_guess
  current_val <- Inf
  recent_drops <- numeric(0)

  check_monotonicity <- "monotonicity" %in% profile_retry_on
  check_constraint <- "constraint" %in% profile_retry_on
  check_drop <- "drop" %in% profile_retry_on

  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper

  df <- tibble::tibble(k = integer(), psi = numeric(), loglik = numeric())

  .print_verbose_row <- function(psi_k, eval) {
    psi_resid_print <- abs(
      eval$psi_residual %||% (eval$psi_at_hat - psi_k)
    )
    cat(sprintf(
      "%-8.3f %-12.4f %-12.4f %-10.6f %-8d\n",
      psi_k,
      eval$branch_val,
      eval$E_loglik_at_hat %||% NA_real_,
      psi_resid_print,
      eval$solver_iterations %||% NA_integer_
    ))
  }

  repeat {
    retry <- 0L
    psi_k <- grid$psi_mle + k_curr * grid$increment

    hit_lower <- !is.null(psi_lower) && psi_k <= psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k >= psi_upper

    # -------------------------------------------------------------------
    # Boundary handling: evaluate once at the bound then stop immediately.
    # Breaking here (before the normal evaluation loop) ensures the
    # boundary point is never evaluated twice on consecutive grid steps.
    # -------------------------------------------------------------------
    if ((hit_lower || hit_upper) && stop_at_bounds) {
      if (eval_at_bounds) {
        psi_k <- if (hit_lower) psi_lower else psi_upper
        eval <- profile_evaluator(psi_k, current_par)
        if (verbose) {
          .print_verbose_row(psi_k, eval)
        }
        df <- df |>
          dplyr::add_row(k = k_curr, psi = psi_k, loglik = eval$branch_val)
      }
      break
    }

    # -------------------------------------------------------------------
    # Compute warm start for this step
    # -------------------------------------------------------------------
    psi_prev <- grid$psi_mle + (k_curr - k_direction) * grid$increment
    warm_init <- if (!is.null(warmstart_fn)) {
      tryCatch(
        warmstart_fn(psi_prev, psi_k, current_par),
        error = function(e) current_par
      )
    } else {
      current_par
    }

    # -------------------------------------------------------------------
    # Evaluate with selected violation checks
    # -------------------------------------------------------------------
    drop <- -Inf
    repeat {
      eval <- profile_evaluator(psi_k, warm_init)

      drop <- current_val - eval$branch_val
      psi_resid <- abs(eval$psi_residual %||% (eval$psi_at_hat - psi_k))
      typical_drop <- if (length(recent_drops) >= 3L) {
        median(recent_drops)
      } else {
        Inf
      }

      monotone_ok <- !check_monotonicity || eval$branch_val <= current_val
      constraint_ok <- !check_constraint || psi_resid <= resid_tol
      drop_ok <- !check_drop ||
        !(is.finite(max_drop_frac) &&
          length(recent_drops) >= 3L &&
          drop > max_drop_frac * typical_drop)

      if ((monotone_ok && constraint_ok && drop_ok) || retry >= max_retries) {
        break
      }

      retry <- retry + 1L
      warm_init <- warm_init +
        stats::rnorm(length(warm_init), sd = 0.1 * retry)
    }

    # Warn on significant monotonicity violations that survived all retries
    if (
      check_monotonicity && eval$branch_val > current_val && max_retries > 0L
    ) {
      violation <- eval$branch_val - current_val
      if (violation > 1e-3) {
        warning(
          sprintf(
            "traverse_profile_side(): monotonicity violation at k=%d after %d retries (delta = %.6f).",
            k_curr,
            retry,
            violation
          ),
          call. = FALSE
        )
      }
    }

    # Update recent drops — only for genuine decreasing steps
    if (drop > 0 && is.finite(drop)) {
      recent_drops <- c(tail(recent_drops, 9L), drop)
    }

    current_val <- eval$branch_val

    if (!is.finite(current_val)) {
      stop(
        "traverse_profile_side(): non-finite log-likelihood at k = ",
        k_curr,
        call. = FALSE
      )
    }

    if (verbose) {
      .print_verbose_row(psi_k, eval)
    }

    df <- df |>
      dplyr::add_row(k = k_curr, psi = psi_k, loglik = current_val)

    if (!is.null(cutoff) && current_val < cutoff) {
      break
    }

    # Advance warm start only when constraint was satisfied
    psi_resid_final <- abs(eval$psi_residual %||% (eval$psi_at_hat - psi_k))
    if (psi_resid_final <= resid_tol) {
      current_par <- eval$param_hat
    }

    k_curr <- k_curr + k_direction
  }

  df |>
    dplyr::distinct() |>
    dplyr::arrange(k)
}

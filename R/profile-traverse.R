# ======================================================================
# profile-traverse.R — Profile Log-Likelihood Traversal
#
# Provides:
#   traverse_profile_side() — one-sided profile sweep with
#                             multi-start evaluation and hard
#                             monotonicity enforcement
# ======================================================================

#' One-Sided Profile Log-Likelihood Sweep Along the ψ-Grid
#'
#' @description
#' Performs a one-sided continuation sweep of the profile log-likelihood
#' by moving outward from the mode along a fixed ψ-grid. At each grid
#' point ψ_k, the constrained optimization is solved using a set of
#' diverse starting points. The best feasible result is selected.
#'
#' Starting points at each grid point:
#' \enumerate{
#'   \item The current warm-start (chained from the previous step).
#'   \item \code{init_guess} — the mode parameter, providing a global
#'     anchor that breaks chain dependency when the warm start has drifted.
#'   \item \code{max_retries} jittered copies of \code{init_guess},
#'     with jitter scale increasing with retry index.
#'   \item A final fresh attempt with the warm start as last resort.
#' }
#'
#' Selection criterion: the feasible result with the highest
#' log-likelihood. Feasibility is determined by
#' \code{psi_resid <= resid_tol}. If no feasible result is found across
#' all starts, the first infeasible result is used as a fallback,
#' then a fresh warm-start attempt.
#'
#' Monotonicity is enforced as a hard theoretical property: the profile
#' log-likelihood must be non-increasing away from the mode. A result
#' that increases relative to the previous value is never used to
#' advance the warm-start chain, regardless of feasibility. A warning
#' is issued if a monotonicity violation survives all starts.
#'
#' The warm-start chain advances only from steps that are both feasible
#' and non-increasing, preventing constraint failures and upward jumps
#' from corrupting subsequent steps.
#'
#' The first point below the cutoff is included before stopping,
#' matching the behavior of \code{traverse_branch_side()}.
#'
#' @param grid              ψ-grid object from \code{psi_grid_anchor()}.
#' @param k_start           Integer. Starting grid index (+1 or -1).
#' @param cutoff            Numeric scalar. Stopping threshold.
#' @param init_guess        Numeric vector. Warm-start parameter at mode.
#'   Used as a global anchor start at every grid point.
#' @param profile_evaluator Function \code{(psi, param_init) ->
#'   list(param_hat, branch_val, psi_residual, E_loglik_at_hat,
#'   solver_iterations)}.
#' @param max_retries       Non-negative integer. Number of jittered
#'   copies of \code{init_guess} to try beyond the warm-start and
#'   \code{init_guess} itself.
#' @param stop_at_bounds    Logical. Default: \code{TRUE}.
#' @param eval_at_bounds    Logical. Evaluate once at the ψ bound before
#'   stopping. Requires \code{stop_at_bounds = TRUE}. Default: \code{TRUE}.
#' @param warmstart_fn      Optional function
#'   \code{(psi_curr, psi_next, param_curr) -> numeric vector}.
#' @param max_drop_frac     Retained for API compatibility. Not used in
#'   the multi-start implementation.
#' @param resid_tol         Non-negative numeric scalar. Constraint
#'   residual tolerance for feasibility determination. Default: \code{1e-3}.
#' @param profile_retry_on  Retained for API compatibility. Monotonicity
#'   is always enforced; this argument is ignored.
#' @param verbose           Logical. Print a row per grid point.
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

  # -------------------------------------------------------------------
  # Evaluate a single start safely; returns NULL on error or non-finite
  # -------------------------------------------------------------------
  .try_start <- function(psi_k, start) {
    ev <- tryCatch(
      profile_evaluator(psi_k, start),
      error = function(e) NULL
    )
    if (is.null(ev) || !is.finite(ev$branch_val)) {
      return(NULL)
    }
    ev
  }

  # -------------------------------------------------------------------
  # Multi-start evaluation at a single grid point.
  #
  # Starts: warm_init, init_guess, max_retries jittered init_guess
  # copies, and a final fresh warm-start attempt as last resort.
  # Selection: highest branch_val among feasible (psi_resid <= resid_tol)
  # results. Falls back to the first infeasible result, then a fresh
  # warm-start attempt.
  # -------------------------------------------------------------------
  .best_eval <- function(psi_k, warm_init) {
    starts <- c(
      list(warm_init),
      list(init_guess),
      lapply(seq_len(max_retries), function(i) {
        init_guess + stats::rnorm(length(init_guess), sd = 0.3 * i)
      })
    )

    best_feasible <- NULL
    fallback <- NULL

    for (start in starts) {
      ev <- .try_start(psi_k, start)
      if (is.null(ev)) next

      psi_resid <- abs(ev$psi_residual %||% (ev$psi_at_hat - psi_k))
      feasible <- psi_resid <= resid_tol

      if (feasible) {
        if (
          is.null(best_feasible) ||
            ev$branch_val > best_feasible$branch_val
        ) {
          best_feasible <- ev
        }
      } else if (is.null(fallback)) {
        fallback <- ev
      }
    }

    best_feasible %||% fallback %||% .try_start(psi_k, warm_init)
  }

  repeat {
    psi_k <- grid$psi_mle + k_curr * grid$increment

    hit_lower <- !is.null(psi_lower) && psi_k <= psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k >= psi_upper

    # -------------------------------------------------------------------
    # Boundary handling
    # -------------------------------------------------------------------
    if ((hit_lower || hit_upper) && stop_at_bounds) {
      if (eval_at_bounds) {
        psi_k <- if (hit_lower) psi_lower else psi_upper
        eval <- .try_start(psi_k, current_par) %||%
          list(
            branch_val = NA_real_,
            E_loglik_at_hat = NA_real_,
            psi_residual = NA_real_,
            solver_iterations = NA_integer_
          )
        if (verbose) .print_verbose_row(psi_k, eval)
        if (is.finite(eval$branch_val)) {
          df <- df |>
            dplyr::add_row(k = k_curr, psi = psi_k, loglik = eval$branch_val)
        }
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
    # Multi-start evaluation
    # -------------------------------------------------------------------
    eval <- .best_eval(psi_k, warm_init)

    if (is.null(eval)) {
      stop(
        "traverse_profile_side(): all starts failed at k = ",
        k_curr,
        call. = FALSE
      )
    }

    if (!is.finite(eval$branch_val)) {
      stop(
        "traverse_profile_side(): non-finite log-likelihood at k = ",
        k_curr,
        call. = FALSE
      )
    }

    # -------------------------------------------------------------------
    # Hard monotonicity check
    # -------------------------------------------------------------------
    monotonicity_ok <- eval$branch_val <= current_val + 1e-6

    if (!monotonicity_ok) {
      warning(
        sprintf(
          "traverse_profile_side(): monotonicity violation at k=%d after all starts (delta = %.6f).",
          k_curr,
          eval$branch_val - current_val
        ),
        call. = FALSE
      )
    }

    current_val <- eval$branch_val

    if (verbose) .print_verbose_row(psi_k, eval)

    # -------------------------------------------------------------------
    # Record point, then check cutoff — include first point below cutoff
    # to match branch traversal behavior
    # -------------------------------------------------------------------
    df <- df |>
      dplyr::add_row(k = k_curr, psi = psi_k, loglik = current_val)

    if (!is.null(cutoff) && current_val < cutoff) break

    # -------------------------------------------------------------------
    # Advance warm start only from clean steps:
    #   feasible (psi_resid <= resid_tol) AND non-increasing
    # -------------------------------------------------------------------
    psi_resid_final <- abs(
      eval$psi_residual %||% (eval$psi_at_hat - psi_k)
    )

    if (psi_resid_final <= resid_tol && monotonicity_ok) {
      current_par <- eval$param_hat
    }

    k_curr <- k_curr + k_direction
  }

  df |>
    dplyr::distinct() |>
    dplyr::arrange(k)
}
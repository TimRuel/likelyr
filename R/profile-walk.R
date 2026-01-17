# ======================================================================
# One-Sided Profile Log-Likelihood Sweep (Internal)
# ======================================================================

#' One-Sided Profile Log-Likelihood Sweep
#'
#' @description
#' Performs a one-sided sweep of the profile log-likelihood by moving
#' outward from ψ̂ along increments of size \code{increment}. At each ψ_k,
#' the solver is warm-started from the previous θ̂, enforcing a
#' continuation method along the constrained manifold
#' \eqn{ψ(θ) = ψ_k}.
#'
#' Monotonicity of the profile curve is enforced via limited jittering.
#' In addition, large downward jumps are guarded against using an
#' *adaptive drop-consistency rule*: each new log-likelihood decrease
#' must be commensurate with the previous decrease. If a proposed step
#' exhibits an unusually large drop relative to recent history, the
#' evaluation is rejected and retried with jittered initial conditions.
#'
#' This adaptive rule stabilizes the continuation path while preserving
#' the fixed ψ-grid structure required for downstream inference.
#'
#' @param psi_mle
#'   Numeric scalar giving the MLE of ψ (ψ̂).
#'
#' @param increment
#'   Numeric scalar giving the ψ-grid spacing (Δψ).
#'
#' @param k_direction
#'   Integer scalar equal to \code{+1} (right sweep) or \code{-1}
#'   (left sweep).
#'
#' @param cutoff
#'   Numeric scalar giving the stopping threshold for the
#'   log-likelihood value.
#'
#' @param init_guess
#'   Numeric vector giving the initial θ̂ at ψ̂, used as the warm-start.
#'
#' @param eval_psi_fun
#'   Function with signature
#'   \code{function(psi, param_init)} returning a list with elements
#'   \code{param_hat} (optimizer solution) and
#'   \code{branch_val} (profile log-likelihood at ψ).
#'
#' @param max_retries
#'   Non-negative integer giving the maximum number of jitter retries
#'   allowed when monotonicity or drop-consistency is violated.
#'
#' @param drop_mult
#'   Numeric scalar greater than 1 controlling how large a log-likelihood
#'   drop is allowed relative to the previous drop. Larger values make
#'   the continuation more permissive; smaller values enforce stricter
#'   smoothness of the profile curve.
#'
#' @return
#' A tibble with columns \code{k} and \code{loglik}, sorted by \code{k},
#' representing the one-sided profile log-likelihood path.
#'
#' @keywords internal
walk_profile_side <- function(
  psi_mle,
  increment,
  k_direction,
  cutoff,
  init_guess,
  eval_psi_fun,
  max_retries,
  drop_mult
) {
  k_curr <- k_direction
  current_par <- init_guess
  current_val <- Inf

  prev_drop <- NULL

  df <- tibble::tibble(k = integer(), loglik = numeric())

  repeat {
    psi_k <- psi_mle + k_curr * increment

    retry <- 0L
    eval <- NULL

    while (retry <= max_retries) {
      eval <- eval_psi_fun(psi_k, current_par)

      # ----------------------------------
      # Adaptive drop consistency check
      # ----------------------------------
      delta <- eval$branch_val - current_val
      drop <- -delta # positive = downward

      ok_monotone <- delta <= 1e-6

      ok_drop <- TRUE
      if (!is.null(prev_drop)) {
        ok_drop <- drop <= drop_mult * prev_drop
      }

      if ((ok_monotone && ok_drop) || retry == max_retries) {
        break
      }

      retry <- retry + 1L
      jitter <- stats::rnorm(length(current_par), sd = 0.1 * retry)
      current_par <- current_par + jitter
    }

    if (is.null(eval)) {
      stop("walk_profile_side(): eval_psi_fun yielded NULL.", call. = FALSE)
    }

    current_val <- eval$branch_val

    df <- dplyr::add_row(df, k = k_curr, loglik = current_val)

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

    if (current_val < cutoff) {
      break
    }

    # ----------------------------------
    # Update reference drop
    # ----------------------------------
    if (!is.null(prev_drop)) {
      prev_drop <- drop
    } else if (is.finite(current_val)) {
      prev_drop <- abs(delta)
    }

    current_par <- eval$param_hat
    k_curr <- k_curr + k_direction
  }

  dplyr::arrange(df, k)
}

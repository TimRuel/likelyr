# ======================================================================
# One-Sided Profile Likelihood Sweep (Internal)
# ======================================================================

#' One-Sided Profile Likelihood Sweep
#'
#' @description
#' Performs a one-sided sweep of the profile log-likelihood by moving
#' outward from ψ̂ along increments of size `increment`. At each ψ_k, the
#' solver is warm-started from the previous θ̂, enforcing a continuation
#' method along the constrained manifold ψ(θ) = ψ_k.
#'
#' Monotonicity of the profile curve is enforced via limited jittering,
#' identical in spirit to integrated-likelihood branch sweeps.
#'
#' @param psi_mle Numeric ψ̂.
#' @param increment Numeric grid spacing (Δψ).
#' @param k_direction Integer ±1 indicating sweep direction.
#' @param cutoff Numeric stopping threshold for log-likelihood.
#' @param init_guess θ̂ at ψ̂, used as the initial warm-start.
#' @param eval_psi_fun Function(psi, theta_init) → list(theta_hat, branch_val).
#' @param max_retries Integer jitter retry budget for monotonicity enforcement.
#'
#' @return Tibble of (k, value), sorted by k.
#' @keywords internal
walk_profile_side <- function(
    psi_mle,
    increment,
    k_direction,
    cutoff,
    init_guess,
    eval_psi_fun,
    max_retries
) {

  k_curr      <- k_direction
  current_par <- init_guess
  current_val <- Inf

  df <- tibble::tibble(k = integer(), value = numeric())

  repeat {

    psi_k <- psi_mle + k_curr * increment

    # ============================================================
    # Eval with jitter-retry monotonicity protection
    # ============================================================
    retry <- 0L
    eval  <- NULL

    while (retry <= max_retries) {

      eval <- eval_psi_fun(psi_k, current_par)

      if (eval$branch_val <= current_val || retry == max_retries)
        break

      # Warm-start jitter when monotonicity violated
      retry <- retry + 1L
      jitter <- stats::rnorm(length(current_par), sd = 0.1 * retry)
      current_par <- current_par + jitter
    }

    if (is.null(eval))
      stop("walk_profile_side(): eval_psi_fun yielded NULL.", call. = FALSE)

    current_val <- eval$branch_val

    # ============================================================
    # Termination check
    # ============================================================
    if (!is.finite(current_val) || current_val < cutoff)
      break

    # Record k and value
    df <- dplyr::add_row(df, k = k_curr, value = current_val)

    # Update warm-start for continuation
    current_par <- eval$theta_hat

    # Move outward
    k_curr <- k_curr + k_direction
  }

  dplyr::arrange(df, k)
}

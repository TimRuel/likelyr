# ======================================================================
# Profile-Likelihood Builder (Improved)
# ======================================================================

#' Generate a Profile Likelihood Curve
#'
#' @description
#' Internal helper used by [profile()] to construct the *profile
#' likelihood branch* by sweeping left and right from ψ̂ using a fixed
#' increment. Nuisance parameters remain fixed at θ̂ throughout.
#'
#' The function performs:
#'   • Left sweep:  ψ = ψ̂ - k·increment
#'   • Right sweep: ψ = ψ̂ + k·increment
#'   • Termination when loglik < cutoff
#'
#' @param psi_mle Numeric scalar. The ψ-value at θ̂.
#' @param theta_mle Numeric vector. MLE θ̂.
#' @param loglik_at_mle Numeric scalar. log-likelihood at θ̂.
#' @param increment Numeric scalar. ψ-grid spacing.
#' @param cutoff Numeric scalar. loglik cutoff (loglik_at_mle − χ²/2).
#' @param eval_psi_fun Function ψ → loglik(ψ) constructed by
#'   `build_eval_psi_fun(cal)(theta_mle)`.
#' @param max_retries Integer. Max retries for monotonicity enforcement
#'   inside `walk_profile_side()`.
#'
#' @return A tibble with:
#'   * k               – integer grid index
#'   * psi             – ψ-values
#'   * value           – log-likelihood at ψ
#'   * value_centered  – loglik − max(loglik)
#'
#' The tibble is sorted in increasing order of ψ.
#'
#' @keywords internal
generate_profile <- function(
    psi_mle,
    theta_mle,
    loglik_at_mle,
    increment,
    cutoff,
    eval_psi_fun,
    max_retries
) {

  # ------------------------------------------------------------
  # 1. Left sweep
  # ------------------------------------------------------------
  left <- walk_profile_side(
    psi_mle       = psi_mle,
    increment     = increment,
    k_direction   = -1L,
    cutoff        = cutoff,
    init_guess    = theta_mle,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # ------------------------------------------------------------
  # 2. Right sweep
  # ------------------------------------------------------------
  right <- walk_profile_side(
    psi_mle       = psi_mle,
    increment     = increment,
    k_direction   = +1L,
    cutoff        = cutoff,
    init_guess    = theta_mle,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # ------------------------------------------------------------
  # 3. Combine all points including center
  # ------------------------------------------------------------
  profile <- dplyr::bind_rows(
    left,
    tibble::tibble(k = 0L, value = loglik_at_mle),
    right
  ) |>
    dplyr::mutate(
      psi            = psi_mle + k * increment,
      value_centered = value - max(value, na.rm = TRUE)
    ) |>
    dplyr::arrange(psi)

  # Metadata
  attr(profile, "n_points") <- nrow(profile)

  profile
}

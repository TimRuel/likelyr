# ======================================================================
# Profile Log-Likelihood Builder
# ======================================================================

#' Generate a Profile Log-Likelihood Curve
#'
#' @description
#' Internal helper used by [profile()] to construct the **profile
#' log-likelihood curve** by sweeping left and right from the MLE ψ̂
#' along a fixed ψ-grid.
#'
#' At each ψ-grid location, the nuisance parameters are optimized
#' subject to the constraint \eqn{ψ(θ) = ψ_k}, using continuation
#' (warm starts) to stabilize the path.
#'
#' The sweep terminates when the profile log-likelihood drops below
#' the supplied cutoff or when ψ-bounds are reached.
#'
#' @param psi_mle
#'   Numeric scalar giving the MLE of ψ (ψ̂).
#'
#' @param param_mle
#'   Numeric vector giving the constrained optimizer solution θ̂ at ψ̂.
#'
#' @param loglik_at_mle
#'   Numeric scalar giving the profile log-likelihood value at ψ̂.
#'
#' @param increment
#'   Numeric scalar giving the ψ-grid spacing (Δψ).
#'
#' @param cutoff
#'   Numeric scalar giving the stopping threshold for the profile
#'   log-likelihood.
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
#'   allowed when enforcing monotonicity.
#'
#' @param stop_at_bounds
#'   Logical scalar. If TRUE, the sweep stops when a ψ bound is reached.
#'
#' @param eval_at_bounds
#'   Logical scalar. If TRUE, the profile log-likelihood is evaluated
#'   once at the ψ bound before stopping. Requires
#'   \code{stop_at_bounds = TRUE}.
#'
#' @param psi_lower
#'   Optional numeric scalar giving the lower ψ bound.
#'
#' @param psi_upper
#'   Optional numeric scalar giving the upper ψ bound.
#'
#' @return
#' A tibble with columns:
#' \itemize{
#'   \item \code{k} — integer ψ-grid index,
#'   \item \code{psi} — ψ-grid value,
#'   \item \code{loglik} — profile log-likelihood at ψ,
#'   \item \code{loglik_centered} — centered log-likelihood
#' }
#'
#' with attributes:
#' \itemize{
#'   \item \code{n_points} — number of grid points,
#'   \item \code{psi_MLE} — ψ̂,
#'   \item \code{type} — \code{"profile"}
#' }
#'
#' @keywords internal
generate_profile <- function(
  psi_mle,
  param_mle,
  loglik_at_mle,
  increment,
  cutoff,
  branch_fn,
  max_retries,
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE,
  psi_lower = NULL,
  psi_upper = NULL
) {
  # ------------------------------------------------------------
  # Left sweep
  # ------------------------------------------------------------
  left <- walk_profile_side(
    grid = grid,
    k_start = -1L,
    cutoff = cutoff,
    init_guess = param_mle,
    branch_fn = branch_fn,
    max_retries = max_retries,
    stop_at_bounds = stop_at_bounds,
    eval_at_bounds = eval_at_bounds
  )

  # ------------------------------------------------------------
  # Right sweep
  # ------------------------------------------------------------
  right <- walk_profile_side(
    grid = grid,
    k_start = +1L,
    cutoff = cutoff,
    init_guess = param_mle,
    branch_fn = branch_fn,
    max_retries = max_retries,
    stop_at_bounds = stop_at_bounds,
    eval_at_bounds = eval_at_bounds
  )

  # ------------------------------------------------------------
  # Combine center + sweeps
  # ------------------------------------------------------------
  psi_ll_df <- dplyr::bind_rows(
    left,
    tibble::tibble(k = 0L, loglik = loglik_at_mle),
    right
  ) |>
    dplyr::mutate(
      psi = psi_mle + k * increment
    ) |>
    dplyr::arrange(.data$psi) |>
    dplyr::mutate(
      loglik_centered = .data$loglik - max(.data$loglik, na.rm = TRUE)
    )

  # ------------------------------------------------------------
  # Metadata
  # ------------------------------------------------------------
  attr(psi_ll_df, "n_points") <- nrow(psi_ll_df)
  attr(psi_ll_df, "psi_MLE") <- psi_mle
  attr(psi_ll_df, "type") <- "profile"

  psi_ll_df
}

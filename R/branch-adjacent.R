#' @keywords internal
#' @title Compute nearest ψ grid points to a branch-specific mode
#'
#' @description
#' Internal helper that identifies the immediate left and right ψ grid points
#' surrounding a branch-specific ψ mode (`psi_hat_branch`), assuming a uniform
#' grid of the form:
#'
#' \deqn{\psi_k = \psi_{\text{MLE}} + k \cdot \text{increment}}
#'
#' where `k` is an integer grid index.
#'
#' This function does **not** construct the full grid. It computes the
#' nearest grid-aligned values algebraically, which is fast and avoids
#' unnecessary vector generation when extending branch evaluations.
#'
#' @param psi_hat_branch Numeric scalar. The ψ value at which the branch
#'   log-likelihood attains its maximum (branch-specific mode).
#' @param psi_mle Numeric scalar. The global ψ MLE (acts as grid origin).
#' @param increment Positive numeric scalar. The grid spacing between ψ values.
#'
#' @return A named list with components:
#' \describe{
#'   \item{left}{Grid point immediately ≤ `psi_hat_branch`.}
#'   \item{right}{Grid point immediately ≥ `psi_hat_branch`.}
#'   \item{k_left}{Integer index of the left grid point relative to `psi_MLE`.}
#'   \item{k_right}{Integer index of the right grid point relative to `psi_MLE`.}
#' }
#'
#' @examples
#' get_adjacent_psi_points(psi_hat_branch = 1.13, psi_MLE = 0, increment = 0.25)
#'
#' get_adjacent_psi_points(psi_hat_branch = -0.68, psi_MLE = 0, increment = 0.25)
get_adjacent_psi_points <- function(
    psi_hat_branch,
    psi_mle,
    increment
) {

  # Grid index of branch_mode relative to psi_MLE
  k_float <- (psi_hat_branch - psi_mle) / increment

  # Nearest integer grid indices on each side
  k_left  <- floor(k_float)
  k_right <- ceiling(k_float)

  list(
    left    = psi_mle + k_left  * increment,
    right   = psi_mle + k_right * increment,
    k_left  = k_left,
    k_right = k_right
  )
}

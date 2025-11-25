#' @keywords internal
get_adjacent_psi_points <- function(
    psi_hat_branch,
    grid
) {
  k_float <- (psi_hat_branch - grid$psi_mle) / grid$increment

  # floating guards
  k_left  <- floor(k_float + 1e-12)
  k_right <- ceiling(k_float - 1e-12)

  list(
    left    = grid$psi_mle + k_left  * grid$increment,
    right   = grid$psi_mle + k_right * grid$increment,
    k_left  = k_left,
    k_right = k_right
  )
}

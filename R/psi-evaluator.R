# ======================================================================
# Evaluate branch log-likelihood at a specific psi (for a given omega_hat)
# ======================================================================

#' Evaluate branch log-likelihood at a given psi for fixed omega_hat
#'
#' @param psi_target Numeric scalar target value of psi(theta).
#' @param omega_hat  Nuisance draw used in E_loglik.
#' @param cal        A `likelihood_calibration` object.
#' @param solve_theta_fn A constrained solver function created by
#'        `.make_constrained_solver(cal)`.
#' @param theta_init Optional initial guess for theta.
#'
#' @return A list with:
#'   * `theta_hat`    Maximizer of E_loglik under psi(theta) = psi_target
#'   * `branch_val`   The *profile* log-likelihood at that (psi_target, theta_hat)
#'
#' @keywords internal
.eval_branch_ll_at_psi <- function(psi_target,
                                   omega_hat,
                                   cal,
                                   solve_theta_fn,
                                   theta_init = cal$theta_hat) {

  # solve constrained problem for this psi and omega_hat
  sol <- solve_theta_fn(
    psi_target = psi_target,
    omega_hat  = omega_hat,
    theta_init = theta_init
  )

  theta_hat <- sol$theta_hat

  # Branch value: actual log-likelihood at theta_hat
  branch_val <- cal$loglik(theta_hat)

  list(
    theta_hat  = theta_hat,
    branch_val = branch_val,
    opt_obj    = sol$opt_obj
  )
}

# ======================================================================
# INTERNAL: Robust branch mode solver
# ======================================================================
branch_mode_solve <- function(
    psi_mle,
    eval_psi_fun,
    theta_init,
    search_interval,
    retries = 3,
    expand_factor = 1.5
) {
  lower <- search_interval[1]
  upper <- search_interval[2]

  # Negative branch loglikelihood
  obj <- function(psi_val) {
    res <- eval_psi_fun(psi_val, theta_init)
    -res$branch_val
  }

  # Initial optimization
  opt <- optim(
    par     = psi_mle,
    fn      = obj,
    method  = "Brent",
    lower   = lower,
    upper   = upper
  )

  psi_hat <- opt$par

  # If optimizer got stuck at a boundary, expand interval and retry
  k <- 0
  while ((psi_hat == lower || psi_hat == upper) && k < retries) {
    width <- (upper - lower) * expand_factor^k
    center <- psi_mle
    lower2 <- max(lower, center - width)
    upper2 <- min(upper, center + width)

    opt <- optim(
      par     = psi_mle,
      fn      = obj,
      method  = "Brent",
      lower   = lower2,
      upper   = upper2
    )
    psi_hat <- opt$par
    k <- k + 1
  }

  eval_at <- eval_psi_fun(psi_hat, theta_init)

  list(
    psi_hat        = psi_hat,
    theta_hat      = eval_at$theta_hat,
    loglik_at_mode = eval_at$branch_val
  )
}

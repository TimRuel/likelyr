# ============================================================
# Expectations
# ============================================================

E_gamma <- function(
  omega_hat,
  phi_i,
  t,
  n_per_process,
  FUN = lgamma,
  p_cutoff = 1e-12,
  max_y_cap = 1e6
) {
  n <- sum(n_per_process)

  J <- length(n_per_process)

  # --- Split parameters
  theta_hat <- omega_hat[seq_len(J)]
  phi_hat <- omega_hat[J + seq_len(J)]

  # --- Expand parameters
  mu_hat_i <- t * rep(theta_hat, times = n_per_process)
  phi_hat_i <- rep(phi_hat, times = n_per_process)

  out <- numeric(n)

  for (i in seq_len(n)) {
    y_max <- min(
      max_y_cap,
      ceiling(qnbinom(
        1 - p_cutoff,
        size = phi_hat_i[i],
        mu = mu_hat_i[i]
      ))
    )

    y <- 0:y_max
    p <- dnbinom(y, size = phi_hat_i[i], mu = mu_hat_i[i])
    p <- p / sum(p)

    out[i] <- sum(FUN(y + phi_i[i]) * p)
  }

  out
}

E_log_gamma <- function(omega_hat, phi_i, t, n_per_process, ...) {
  E_gamma(omega_hat, phi_i, t, n_per_process, FUN = lgamma, ...)
}

E_digamma <- function(omega_hat, phi_i, t, n_per_process, ...) {
  E_gamma(omega_hat, phi_i, t, n_per_process, FUN = digamma, ...)
}

# ============================================================
# Expected log-likelihood
# ============================================================

E_loglik <- function(param, omega_hat, data) {
  n_per_process <- attr(data, "n_per_process")
  t <- data$t
  J <- length(n_per_process)

  # --- Split parameters
  theta <- param[seq_len(J)]
  phi <- param[J + seq_len(J)]

  # --- Expand
  theta_i <- rep(theta, times = n_per_process)
  phi_i <- rep(phi, times = n_per_process)
  mu_i <- t * theta_i

  mu_hat_i <- t * rep(omega_hat[seq_len(J)], times = n_per_process)

  # --- Terms
  lg_phi <- lgamma(phi_i)
  phi_log <- phi_i * log(phi_i)

  E_lg <- E_log_gamma(
    omega_hat,
    phi_i,
    t,
    n_per_process
  )

  theta_term <-
    mu_hat_i * log(mu_i) - (mu_hat_i + phi_i) * log(mu_i + phi_i)

  sum(E_lg - lg_phi + phi_log + theta_term)
}

# ============================================================
# Gradients
# ============================================================

E_loglik_grad_theta <- function(
  phi_i,
  theta_i,
  mu_i,
  mu_hat_i,
  t,
  obs_to_process
) {
  g_obs <-
    (mu_hat_i / theta_i) -
    t * (mu_hat_i + phi_i) / (mu_i + phi_i)

  as.numeric(rowsum(g_obs, group = obs_to_process))
}


E_loglik_grad_phi <- function(
  phi_i,
  mu_i,
  mu_hat_i,
  E_digamma_i,
  obs_to_process
) {
  g_obs <-
    E_digamma_i -
    digamma(phi_i) +
    log(phi_i) +
    1 -
    log(phi_i + mu_i) -
    (mu_hat_i + phi_i) / (mu_i + phi_i)

  as.numeric(rowsum(g_obs, group = obs_to_process))
}


E_loglik_grad <- function(param, omega_hat, data) {
  n_per_process <- attr(data, "n_per_process")
  t <- data$t
  J <- length(n_per_process)

  # --- Split
  theta <- param[seq_len(J)]
  phi <- param[J + seq_len(J)]

  # --- Expand
  theta_i <- rep(theta, times = n_per_process)
  phi_i <- rep(phi, times = n_per_process)
  mu_i <- t * theta_i

  mu_hat_i <- t * rep(omega_hat[seq_len(J)], times = n_per_process)

  obs_to_process <- rep(seq_len(J), times = n_per_process)

  # --- Theta gradient
  grad_theta <- E_loglik_grad_theta(
    phi_i,
    theta_i,
    mu_i,
    mu_hat_i,
    t,
    obs_to_process
  )

  # --- Phi gradient
  E_digamma_i <- E_digamma(
    omega_hat,
    phi_i,
    t,
    n_per_process
  )

  grad_phi <- E_loglik_grad_phi(
    phi_i,
    mu_i,
    mu_hat_i,
    E_digamma_i,
    obs_to_process
  )

  c(grad_theta, grad_phi)
}

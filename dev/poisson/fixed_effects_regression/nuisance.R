E_loglik <- function(param, omega_hat, data) {
  X <- attr(data, "X")
  E_Y <- data$t * exp(X %*% omega_hat)
  eta <- X %*% param
  mu <- data$t * exp(eta)
  sum(E_Y * (log(data$t) + eta) - mu - lgamma(E_Y + 1))
}

E_loglik_grad <- function(param, omega_hat, data) {
  X <- attr(data, "X")
  theta_omega_hat <- exp(X %*% omega_hat)
  theta_param <- exp(X %*% param)
  as.numeric(t(X) %*% (data$t * (theta_param - theta_omega_hat)))
}

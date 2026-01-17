loglik <- function(param, data) {
  n_per_process <- attr(data, "n_per_process")
  J <- length(n_per_process)

  # Split parameter vector
  theta <- param[seq_len(J)]
  phi <- param[J + seq_len(J)]

  # Block-expand parameters
  theta_i <- rep(theta, times = n_per_process)
  phi_i <- rep(phi, times = n_per_process)

  sum(
    dnbinom(
      x = data$Y,
      size = phi_i,
      mu = theta_i * data$t,
      log = TRUE
    )
  )
}


fit_model <- function(data) {
  stopifnot(all(c("Y", "t", "process") %in% names(data)))
  formula <- attr(data, "formula")

  glmmTMB::glmmTMB(
    formula = formula,
    family = glmmTMB::nbinom2(), # Variance = mu + phi * mu^2
    dispformula = ~ 0 + process, # phi varies by process
    data = data
  )
}

param_mle_fn <- function(data) {
  data |>
    fit_model() |>
    glmmTMB::fixef() |>
    unlist() |>
    exp()
}

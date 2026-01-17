psi_fn <- function(param, data) {
  weights <- attr(data, "weights")

  theta <- param[1:length(weights)]

  sum(theta * weights)
}

psi_jac <- function(param, data) {
  weights <- attr(data, "weights")
  cbind(matrix(weights, nrow = 1), matrix(0, nrow = 1, ncol = length(weights)))
}

get_psi_mle_se <- function(param_mle, data) {
  weights <- attr(data, "weights")

  # --- Exposure moments by process
  se_terms <- data |>
    dplyr::group_by(process) |>
    dplyr::summarise(
      S1 = sum(t),
      S2 = sum(t^2),
      .groups = "drop"
    )

  J <- length(weights)

  # --- Split parameters
  theta <- param_mle[seq_len(J)]
  phi <- param_mle[J + seq_len(J)]

  # --- Delta-method variance
  var_theta <-
    theta / se_terms$S1 + theta^2 / phi * se_terms$S2 / (se_terms$S1^2)

  sqrt(sum(weights^2 * var_theta))
}


search_interval_fn <- function(data) {
  # --- Fit model & extract MLEs
  model <- fit_model(data)
  param_mle <- param_mle_fn(data)

  # --- Target estimand
  psi_mle <- psi_fn(param_mle, data)
  psi_mle_se <- get_psi_mle_se(param_mle, data)

  # --- 6-sigma search window
  psi_mle + c(-1, 1) * 6 * psi_mle_se
}

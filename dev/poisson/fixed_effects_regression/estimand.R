psi_fn <- function(param, data) {
  X <- attr(data, "X")
  weights <- attr(data, "weights")

  theta <- data.frame(
    theta = exp(X %*% param),
    process = factor(rownames(X))
  ) |>
    dplyr::group_by(process) |>
    dplyr::summarise(theta = mean(theta)) |>
    tibble::deframe()

  sum(theta * weights)
}

psi_jac <- function(param, data) {
  X <- attr(data, "X")
  weights <- attr(data, "weights")

  processes <- rownames(X)

  n_per_process <- table(processes)

  W <- as.numeric(
    weights[as.character(processes)] / n_per_process[as.character(processes)]
  )

  theta <- exp(X %*% param)

  as.numeric(t(X) %*% (W * theta))
}

get_psi_mle_se <- function(model, weights, X) {
  # Extract Beta_MLE as numeric vector
  beta_mle <- model |>
    coef() |>
    as.matrix(ncol = 1)
  beta_cov <- vcov(model)

  # Group membership for each observation
  processes <- model$data$process
  process_labels <- levels(processes)

  # Ensure weights are a named vector (one per group)
  if (is.null(names(weights))) {
    if (length(weights) != length(group_labels)) {
      stop(
        "If 'weights' is unnamed, it must have length equal to number of groups."
      )
    }
    weights <- setNames(weights, process_labels)
  }

  # Linear predictors and exp
  eta <- drop(X %*% beta_mle)
  exp_eta <- exp(eta)

  # Process sizes
  n_per_process <- table(processes)

  # Per-observation effective weights = w_j / n_j
  obs_w <- weights[as.character(processes)] /
    n_per_process[as.character(processes)]

  # Gradient: sum_i obs_w[i] * exp(eta_i) * x_i
  grad <- as.numeric(t(X) %*% (obs_w * exp_eta))
  names(grad) <- colnames(X)

  # Delta-method SE
  se <- sqrt(as.numeric(t(grad) %*% beta_cov %*% grad))

  return(se)
}

search_interval_fn <- function(data) {
  X <- attr(data, "X")
  weights <- attr(data, "weights")
  model <- fit_model(data)
  beta_mle <- beta_mle_fn(data)
  psi_mle <- psi_fn(beta_mle, data)
  psi_mle_se <- get_psi_mle_se(model, weights, X)
  psi_mle + c(-1, 1) * 6 * psi_mle_se
}

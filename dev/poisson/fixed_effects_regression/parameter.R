# Generic sampler for config blocks like:
#   distribution: { name: rnorm, args: [0,1] }
sample_from_config <- function(dist_config, n = 1) {
  dist_fun <- match.fun(dist_config$name)
  args <- dist_config$args
  if (is.null(args)) {
    args <- list()
  }

  # Combine n with args; if args is a vector, convert to list
  if (is.vector(args) && !is.list(args)) {
    args <- as.list(args)
  }
  out <- do.call(dist_fun, c(list(n = n), args))

  return(out)
}

# Generate process weights  (reads from config$model$weights)
generate_process_weights <- function(config) {
  weight_config <- config$model$weights
  n_per_process <- expand_processes(config$model$processes)
  J <- length(n_per_process)

  if (
    is.null(weight_config$distribution) ||
      is.null(weight_config$distribution$name)
  ) {
    stop(
      "Weight distribution must be specified as list(distribution=list(name=..., args=[...]))."
    )
  }

  raw <- sample_from_config(weight_config$distribution, J)

  if (!is.null(weight_config$normalize_mean_to)) {
    weights <- raw / mean(raw) * weight_config$normalize_mean_to
  } else if (!is.null(weight_config$normalize_sum_to)) {
    weights <- raw / sum(raw) * weight_config$normalize_sum_to
  } else {
    weights <- raw
  }

  names(weights) <- names(n_per_process)
  weights
}

get_beta_0 <- function(config) {
  n_per_process <- unlist(config$model$processes)
  process_labels <- names(n_per_process)
  J <- length(n_per_process)
  covs <- config$model$covariates
  homo_covs <- Filter(\(c) c$type == "homogeneous", covs)
  hetero_covs <- Filter(\(c) c$type == "heterogeneous", covs)

  # ----- Generate coefficients for beta_0 -----
  # Order: heterogeneous intercepts (α), homogeneous slopes (γ), heterogeneous slopes (ζ)

  # --- 1. Heterogeneous intercepts ---
  alpha_g <- sample_from_config(config$model$intercepts$distribution, J)

  # --- 2. Homogeneous slopes ---
  gamma <- sapply(homo_covs, function(c) {
    sample_from_config(c$coefficient$distribution, 1)
  })

  # --- 3. Heterogeneous slopes ---
  zeta_g <- c()
  for (cov in hetero_covs) {
    zeta_g <- c(zeta_g, sample_from_config(cov$coefficient$distribution, J))
  }

  # --- 5. Concatenate beta vector ---
  beta_vals <- c(alpha_g, gamma, zeta_g)

  # --- 6. Assign rownames with symbols and "_process" suffixes ---
  beta_names <- c(
    paste0(config$model$intercepts$symbol, "_", process_labels), # α_g
    sapply(homo_covs, function(c) c$coefficient$symbol), # γ
    unlist(lapply(hetero_covs, function(c) {
      paste0(c$coefficient$symbol, "_", process_labels)
    })) # ζ_g
  )

  # --- 7. Filter missing values and stack into a matrix ---
  beta_0 <- beta_vals |>
    setNames(beta_names) |>
    as.matrix(ncol = 1)

  return(beta_0)
}

compute_true_marginal_rates <- function(config, beta_0) {
  n_per_process <- unlist(config$model$processes)
  process_labels <- names(n_per_process)
  J <- length(n_per_process)
  n_mc <- as.numeric(config$model$evaluation$marginal$n_mc)
  n_per_process <- rep(n_mc, J)
  names(n_per_process) <- process_labels

  X_mc <- get_X(config, beta_0, n_per_process)

  eta_mc <- X_mc %*% beta_0
  colnames(eta_mc) <- "eta"

  theta_0 <- eta_mc |>
    as_tibble(rownames = "process") |>
    group_by(process) |>
    summarise(mean_exp = mean(exp(eta)), .groups = "drop") |>
    deframe()

  return(theta_0)
}

generate_true_parameters <- function(config) {
  beta_0 <- get_beta_0(config)

  weights <- generate_process_weights(config)

  theta_0 <- compute_true_marginal_rates(config, beta_0)

  n_per_process <- expand_processes(config$model$processes)

  list(
    beta_0 = beta_0,
    theta_0 = theta_0,
    weights = weights,
    n_per_process = n_per_process
  )
}

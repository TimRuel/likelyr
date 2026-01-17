# Generate covariates for all observations
generate_covariate <- function(cov_cfg, n) {
  dist_fn <- match.fun(cov_cfg$distribution$name)
  covariate <- do.call(dist_fn, c(list(n = n), cov_cfg$distribution$args))
  return(covariate)
}

recover_original_covariates <- function(
  design_matrix,
  intercept_prefix = "α",
  homo_cov_prefix = "γ",
  hetero_cov_prefix = "ζ"
) {
  # design_matrix: data.frame or matrix
  # intercept_prefix: prefix for the one-hot process indicator columns
  # homo_cov_prefix: vector of names for covariates that are common across processes
  # hetero_cov_prefix: prefix for the process-specific covariate columns

  coefs <- colnames(design_matrix)

  homo_cov_coefs <- coefs[grepl(homo_cov_prefix, coefs)]

  num_homo_covs <- length(homo_cov_coefs)

  homo_covs <- design_matrix[, homo_cov_coefs, drop = FALSE]

  colnames(homo_covs) <- paste0("X", 1:num_homo_covs)

  J <- design_matrix |>
    rownames() |>
    unique() |>
    length()

  num_hetero_covs <- sum(grepl(hetero_cov_prefix, coefs)) / J

  hetero_cov_coefs <- coefs[grepl(hetero_cov_prefix, coefs)]

  hetero_cov_dummy_mat <- design_matrix[, hetero_cov_coefs, drop = FALSE]

  hetero_covs <- matrix(
    NA,
    nrow = nrow(hetero_cov_dummy_mat),
    ncol = num_hetero_covs
  )

  for (i in 1:num_hetero_covs) {
    hetero_covs[, i] <- rowSums(hetero_cov_dummy_mat[,
      i:(i + J - 1),
      drop = FALSE
    ])
  }

  colnames(hetero_covs) <- paste0(
    "X",
    (num_homo_covs + 1):(num_homo_covs + num_hetero_covs)
  )

  # 4. Combine into a single data.frame
  recovered <- cbind(homo_covs, hetero_covs) |>
    as.data.frame(row.names = 1:nrow(design_matrix))

  return(recovered)
}

# Build design matrix compatible with stacked beta (kappa, gamma, delta_g, zeta_g)
get_X <- function(config, beta_0, n_per_process) {
  process_labels <- names(n_per_process)
  total_n <- sum(n_per_process)
  J <- length(n_per_process)
  cov_cfgs <- config$model$covariates
  row_idx <- rep(seq_len(J), times = n_per_process)
  X <- matrix(0, nrow = total_n, ncol = nrow(beta_0))
  colnames(X) <- rownames(beta_0)
  rownames(X) <- rep(names(n_per_process), times = n_per_process)

  for (symbol in colnames(X)) {
    if (grepl("α", symbol)) {
      g <- symbol |>
        stringr::str_sub(-1, -1) |>
        (\(g) which(LETTERS == g))()
      obs_idx <- which(row_idx == g)
      X[obs_idx, symbol] <- 1
    } else if (grepl("γ", symbol)) {
      cov_cfg <- Filter(function(c) c$coefficient$symbol == symbol, cov_cfgs)[[
        1
      ]]
      X[, symbol] <- generate_covariate(cov_cfg$variable, total_n)
    } else if (grepl("ζ", symbol)) {
      g <- symbol |>
        stringr::str_sub(-1, -1) |>
        (\(g) which(LETTERS == g))()
      obs_idx <- which(row_idx == g)
      cov_cfg <- Filter(
        function(c) c$coefficient$symbol == stringr::str_sub(symbol, 1, 2),
        cov_cfgs
      )[[1]]
      X[obs_idx, symbol] <- generate_covariate(
        cov_cfg$variable,
        n_per_process[g]
      )
    }
  }
  return(X)
}

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

generate_process_weights <- function(config) {
  weight_config <- config$model$weights
  n_per_process <- unlist(config$model$processes)
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

# Generate Poisson outcomes
generate_data <- function(config, beta_0) {
  n_per_process <- unlist(config$model$processes)
  process_labels <- names(n_per_process)
  J <- length(n_per_process)
  total_n <- sum(n_per_process)
  process_id <- rep(process_labels, times = n_per_process)

  exposure_dist <- match.fun(config$model$exposure$distribution$name)
  exposure_args <- config$model$exposure$distribution$args
  t <- do.call(exposure_dist, c(list(n = total_n), exposure_args))

  X <- get_X(config, beta_0, n_per_process)
  eta <- X %*% beta_0
  mu <- exp(eta)
  Y <- rpois(total_n, t * mu)

  covariates <- recover_original_covariates(X)

  # Combine into final data frame
  data <- tibble::tibble(
    process = factor(process_id, levels = process_labels),
    t = t
  ) |>
    dplyr::bind_cols(covariates) |>
    tibble::add_column(Y = Y)

  attr(data, "X") <- X
  attr(data, "weights") <- generate_process_weights(config)
  attr(data, "formula") <- make_formula(config)

  return(data)
}

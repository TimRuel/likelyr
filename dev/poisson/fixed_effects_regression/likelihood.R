loglik <- function(param, data) {
  X <- attr(data, "X")

  eta <- X %*% param

  mu <- data$t * exp(eta)

  sum(data$Y * (log(data$t) + eta) - mu - lgamma(data$Y + 1))
}

make_formula <- function(config) {
  # Build RHS parts
  rhs_parts <- c("0", "process")

  covs <- config$model$covariates
  homo_covs <- Filter(\(c) c$type == "homogeneous", covs)
  hetero_covs <- Filter(\(c) c$type == "heterogeneous", covs)

  # Homogeneous covariates (same slope across groups)
  if (length(homo_covs) > 0) {
    homo_covs <- sapply(homo_covs, \(x) x$variable$symbol)
    rhs_parts <- c(rhs_parts, homo_covs)
  }

  # Heterogeneous covariates (different slope across groups, can be interpreted as interaction with process)
  if (length(hetero_covs) > 0) {
    hetero_covs <- sapply(hetero_covs, \(x) x$variable$symbol)
    rhs_parts <- c(rhs_parts, paste0(hetero_covs, ":process"))
  }

  # Add process main effect (intercepts per process)
  rhs <- paste(rhs_parts, collapse = " + ")

  # Full formula
  f <- as.formula(paste("Y ~", rhs))

  return(f)
}

fit_model <- function(data) {
  formula <- attr(data, "formula")

  stopifnot(inherits(formula, "formula"))

  glm(
    formula,
    offset = log(t),
    family = poisson(),
    data = data
  )
}

rename_coefs <- function(coef_names) {
  # Separate intercepts, main effects, and interactions
  intercepts <- coef_names[!grepl("X", coef_names)] # process names
  main_effects <- coef_names[grepl("^X\\d+$", coef_names)] # X1, X2, ...
  interactions <- coef_names[grepl(":", coef_names)] # process:X2, ...

  # Figure out which X's are fixed vs varying
  # Fixed slopes: main effects only
  fixed_Xs <- gsub("^X", "", main_effects)
  # Varying slopes: from interactions
  varying_Xs <- unique(gsub(".*:X", "", interactions))

  # Replacements
  new_names <- character(length(coef_names))

  # Intercepts → α_process
  new_names[match(intercepts, coef_names)] <- paste0(
    "α_",
    sub("^process", "", intercepts)
  )

  # Fixed slopes → γ#
  new_names[match(main_effects, coef_names)] <-
    paste0("γ", gsub("^X", "", main_effects))

  # Varying slopes → ζ#_group
  for (int in interactions) {
    parts <- strsplit(int, ":")[[1]] # e.g., c("processA", "X2")
    process <- sub("^process", "", parts[1])
    xnum <- sub("^X", "", parts[2])
    new_names[match(int, coef_names)] <- paste0("ζ", xnum, "_", process)
  }

  return(new_names)
}

beta_mle_fn <- function(data) {
  model <- fit_model(data)

  coefs <- coef(model)
  beta_mle <- as.matrix(coefs, ncol = 1)

  rownames(beta_mle) <- rename_coefs(names(coefs))

  beta_mle
}

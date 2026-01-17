# devtools::document()
# devtools::install(upgrade = "never")
# library(likelyr)
devtools::load_all()
ctx <- rstudioapi::getActiveDocumentContext()
if (!is.null(ctx$path) && nzchar(ctx$path)) {
  script_dir <- dirname(ctx$path)
  setwd(script_dir)
}
config <- yaml::read_yaml("config.yml")

# ============================================================
# Specify model parameter
# ============================================================
source("parameter.R")
seed <- config$seed
set.seed(seed)
true_parameters <- generate_true_parameters(config)

param_0 <- unlist(true_parameters[1:2])
param_lower <- 1e-12
n_per_process <- true_parameters$n_per_process
weights <- true_parameters$weights

parameter <- parameter_spec(
  name = "Joint negative binomial model parameter",
  param_0 = param_0,
  param_lower = param_lower,
  n_per_process = n_per_process,
  weights = weights
)

# ============================================================
# Specify likelihood
# ============================================================
source("likelihood.R")

likelihood <- likelihood_spec(
  loglik = loglik,
  param_mle_fn = param_mle_fn,
  name = "Likelihood Spec"
)

# # ============================================================
# # Specify estimand
# # ============================================================
source("estimand.R")
increment <- 0.1
confidence_levels <- c(0.90, 0.95, 0.99)
cutoff_buffer <- 0.01
uniroot_expand_factor <- 0.02

estimand <- estimand_spec(
  psi_fn = psi_fn,
  psi_jac = psi_jac,
  search_interval_fn = search_interval_fn,
  increment = increment,
  confidence_levels = confidence_levels,
  cutoff_buffer = cutoff_buffer,
  uniroot_expand_factor = uniroot_expand_factor,
  name = "Weighted Sum (psi)"
)

# # ============================================================
# # Specify nuisance parameter
# # ============================================================
source("nuisance.R")

nuisance <- nuisance_spec(
  E_loglik = E_loglik,
  E_loglik_grad = E_loglik_grad,
  name = "Nuisance parameter spec"
)

# # ============================================================
# # Specify optimizer
# # ============================================================
localsolver <- "SLSQP"
control <- list(xtol_rel = 1e-8, maxeval = 1000)
localtol <- 1e-6
max_retries <- 10
drop_mult <- 2

optimizer <- optimizer_spec(
  localsolver = localsolver,
  control = control,
  localtol = localtol,
  max_retries = max_retries,
  drop_mult = drop_mult,
  name = "Optimizer spec"
)

# # ============================================================
# # Specify execution plan (serial or parallel)
# # ============================================================
seed <- 7835
num_workers <- 12
chunk_size <- 1
packages <- c("likelyr")
execution <- parallel_spec(
  num_workers = num_workers,
  chunk_size = chunk_size,
  packages = packages,
  seed = seed,
  name = "Parallel execution"
)

# # ============================================================
# # Specify model
# # ============================================================
model <- model_spec(name = "Poisson - Fixed Effects Regression") |>
  add(parameter) |>
  add(likelihood) |>
  add(estimand) |>
  add(nuisance) |>
  add(optimizer) |>
  add(execution)

# # ============================================================
# # Calibrate model to data and integrate
# # ============================================================
source("data.R")
data <- generate_data(config, parameter)

# # doFuture::registerDoFuture()
# # future::plan(future::multisession, workers = num_workers)

fit <- model |>
  calibrate(data)

# fit <- fit |>
#   integrate()

# future::plan(future::sequential)

fit <- fit |>
  profile()

# fit <- fit |>
#   diagnose() |>
#   infer() |>
#   compare()

fit <- fit |> infer()

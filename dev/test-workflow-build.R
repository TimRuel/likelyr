# Load package internals
devtools::document()
# devtools::install(force = TRUE, upgrade = "never")
devtools::load_all()

library(dplyr)
library(foreach)
library(likelyr)

# ============================================================
# Specify model parameter
# ============================================================
seed <- 7835
set.seed(seed)
J <- 6
theta_0 <- rgamma(J, shape = 3, rate = 1)
theta_lower <- 1e-12

parameter <- parameter_spec(
  name = "Model parameter spec",
  theta_0 = theta_0,
  theta_lower = theta_lower
)

# ============================================================
# Specify likelihood
# ============================================================
loglik <- function(theta, data) sum(dpois(data$Y, data$t * theta, log = TRUE))
theta_mle_fn <- function(data) data$Y / data$t

likelihood <- likelihood_spec(
  loglik = loglik,
  theta_mle_fn = theta_mle_fn,
  name = "Likelihood Spec"
)

# ============================================================
# Specify estimand
# ============================================================
psi_fn <- function(theta, data) sum(theta * data$weights)
psi_jac <- function(theta, data) data$weights
search_interval_fn <- function(data) {
  theta_mle <- theta_mle_fn(data)
  psi_mle <- psi_fn(theta_mle, data)
  psi_mle_se <- sqrt(sum(data$weights^2 * theta_mle / data$t))
  psi_mle + c(-1, 1) * 6 * psi_mle_se
}
increment <- 0.1
confidence_levels <- c(0.90, 0.95, 0.99)
cutoff_buffer <- 0.1
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

# ============================================================
# Specify nuisance parameter
# ============================================================
E_loglik <- function(theta, omega_hat, data) sum(data$t * (omega_hat * log(theta) - theta))
E_loglik_grad <- function(theta, omega_hat, data) data$t * (omega_hat / theta - 1)

nuisance <- nuisance_spec(
  E_loglik = E_loglik,
  E_loglik_grad = E_loglik_grad,
  name = "Nuisance parameter spec"
)

# ============================================================
# Specify optimizer
# ============================================================
localsolver <- "SLSQP"
control <- list(xtol_rel = 1e-8, maxeval  = 1000)
localtol <- 1e-6
max_retries <- 10

optimizer <- optimizer_spec(
  localsolver = localsolver,
  control = control,
  localtol = localtol,
  max_retries = max_retries,
  name = "Optimizer spec"
)

# ============================================================
# Specify execution plan (serial or parallel)
# ============================================================
R <- 10
execution <- serial_spec(
  R = R,
  seed = seed,
  name = "Serial execution"
)

# ============================================================
# Specify model
# ============================================================
model <- model_spec(name = "Poisson - Naive Rates") |>
  add(parameter) |>
  add(likelihood) |>
  add(estimand) |>
  add(nuisance) |>
  add(optimizer) |>
  add(execution)

# ============================================================
# Calibrate model to data and integrate
# ============================================================
process_labels <- LETTERS[1:J]
weights <- runif(J, 1, 3)
t <- do.call(runif, list(n = J, min = J * 1, max = J * 5))
mu_0 <- theta_0 * t
Y <- rpois(J, mu_0)
data <- tibble(process = factor(process_labels),
               t = t,
               Y = Y,
               weights = weights) |>
  group_by(process)

fit <- model |>
  calibrate(data) |>
  profile() |>
  integrate() |>
  diagnose() |>
  infer()
  # compare()





# plot(fit$results$IL$diagnostics)
# plot(fit$results$IL)
# plot(fit$results$PL)
# plot(fit$results$IL$inference)
# plot(fit$results$PL$inference)

# plan(callr, workers = wf$execution$num_workers)
# fit  <- fit_integrated(cal)
# plan(sequential)






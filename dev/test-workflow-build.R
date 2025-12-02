# Load package internals
devtools::document()
# devtools::install(force = TRUE, upgrade = "never")
devtools::load_all()

library(dplyr)
library(foreach)
library(likelyr)

# ============================================================
# Specify likelihood
# ============================================================
J <- 6
loglik <- function(theta, data) sum(dpois(data$Y, data$t * theta, log = TRUE))
theta_mle_fn <- function(data) data$Y / data$t
theta_lower <- 1e-12

likelihood <- likelihood_spec(
  loglik = loglik,
  theta_dim = J,
  theta_mle_fn = theta_mle_fn,
  theta_lower = theta_lower,
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
confidence_levels <- 0.8

estimand <- estimand_spec(
  psi_fn = psi_fn,
  psi_jac = psi_jac,
  search_interval_fn = search_interval_fn,
  increment = increment,
  confidence_levels = confidence_levels,
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
seed <- 7835

execution <- serial_spec(
  R = R,
  seed = seed,
  name = "Serial execution"
)

# ============================================================
# Specify model
# ============================================================
model <- model_spec(name = "Poisson - Naive Rates") |>
  add(likelihood) |>
  add(estimand) |>
  add(nuisance) |>
  add(optimizer) |>
  add(execution)

# ============================================================
# Calibrate model to data
# ============================================================
set.seed(seed)
process_labels <- LETTERS[1:J]
theta_0 <- rgamma(J, shape = 3, rate = 1)
weights <- runif(J, 1, 3)
t <- do.call(runif, list(n = J, min = J * 1, max = J * 5))
mu_0 <- theta_0 * t
Y <- rpois(J, mu_0)
data <- tibble(process = factor(process_labels),
               t = t,
               Y = Y,
               weights = weights) |>
  group_by(process)

cal <- calibrate(model, data)

# ===================================================================
# Integrate likelihood of calibrated model w.r.t. nuisance parameter
# ===================================================================
log_IL <- integrate(cal)

# -------------------------------------------------------------
# 5. Attach diagnostics (separate file)
# -------------------------------------------------------------
# diagnostics <- diagnose(log_IL)

# plan(callr, workers = wf$execution$num_workers)
# fit  <- fit_integrated(cal)
# plan(sequential)






# Load package internals
devtools::load_all()

library(dplyr)
library(foreach)

set.seed(1)

J <- 6
process_labels <- LETTERS[1:J]
theta_0 <- rgamma(J, shape = 3, rate = 1)
weights <- runif(J, 1, 3)
t <- do.call(runif, list(n = J, min = J * 1, max = J * 5))
mu_0 <- theta_0 * t
Y <- rpois(J, mu_0)

# Aggregate to group level
data <- tibble(process = factor(process_labels),
               t = t,
               Y = Y,
               weights = weights) |>
  group_by(process)

loglik <- function(theta, data) sum(dpois(data$Y, data$t * theta, log = TRUE))
E_loglik <- function(theta, omega_hat, data) sum(data$t * (omega_hat * log(theta) - theta))
E_loglik_grad <- function(theta, omega_hat, data) sum(data$t * (omega_hat / theta - 1))
theta_mle_fn <- function(data) data$Y / data$t

psi_fn <- function(theta, data) sum(theta * data$weights)
psi_mle <- psi_fn(theta_mle, data)
psi_mle_se <- sqrt(sum(weights^2 * theta_mle / t))
num_std_errors <- 6
search_interval <- psi_mle + c(-1, 1) * num_std_errors * psi_mle_se
increment <- 0.1
psi_jac <- function(theta, data) data$weights

xtol_rel <- 1e-8
maxeval <- 1000
control <- list(
  xtol_rel = xtol_rel,
  maxeval  = maxeval
)

# ============================================================
# CREATE SPECS
# ============================================================

model <- model_spec(
  loglik = loglik,
  E_loglik = E_loglik,
  param_dim = J,
  E_loglik_grad = E_loglik_grad,
  theta_mle_fn = theta_mle_fn,
  lower = rep(0, J),
  name = "Poisson - Naive Rates"
)

estimand <- estimand_spec(
  psi_fn = psi_fn,
  search_interval = search_interval,
  increment = increment,
  psi_jac = psi_jac,
  name = "Weighted Sum (psi)"
)

nuisance <- nuisance_spec(
  sampler = function(J) rgamma(J, 2, 1),
  init_strategy = "random",
  name = "omega_hat initial guess distribution"
)

optimizer <- optimizer_spec(
  control = control,
  name = "Default optimizer"
)

execution <- serial_spec(
  R = 2,
  seed = 7835,
  name = "Serial execution"
)

# ============================================================
# BUILD WORKFLOW
# ============================================================

wf <- workflow() |>
  add(model) |>
  add(estimand) |>
  add(nuisance) |>
  add(optimizer) |>
  add(execution)

print(wf)

# ============================================================
# CALIBRATE WORKFLOW
# ============================================================

cal <- calibrate(wf, data)
print(cal)

log_IL <- fit_integrated(cal)

# plan(callr, workers = wf$execution$num_workers)
# fit  <- fit_integrated(cal)
# plan(sequential)

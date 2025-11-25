# Load package internals
devtools::document()
# devtools::install(force = TRUE, upgrade = "never")
devtools::load_all()

library(dplyr)
library(foreach)
library(likelyr)

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
E_loglik_grad <- function(theta, omega_hat, data) data$t * (omega_hat / theta - 1)
theta_mle_fn <- function(data) data$Y / data$t

psi_fn <- function(theta, data) sum(theta * data$weights)
theta_mle <- theta_mle_fn(data)
psi_mle <- psi_fn(theta_mle, data)
psi_mle_se <- sqrt(sum(weights^2 * theta_mle / t))
num_std_errors <- 6
search_interval <- psi_mle + c(-1, 1) * num_std_errors * psi_mle_se
increment <- 0.1
psi_jac <- function(theta, data) data$weights
confidence_levels <- 0.8

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
  confidence_levels = confidence_levels,
  name = "Weighted Sum (psi)"
)

nuisance <- nuisance_spec(
  init_guess_sampler = function(J) rgamma(J, 2, 1),
  name = "omega_hat initial guess distribution"
)

optimizer <- optimizer_spec(
  control = control,
  name = "Default optimizer"
)

execution <- serial_spec(
  R = 10,
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

# ============================================================
# CALIBRATE WORKFLOW
# ============================================================

cal <- calibrate(wf, data)

log_IL <- generate(cal)

# -------------------------------------------------------------
# 5. Attach diagnostics (separate file)
# -------------------------------------------------------------
diagnostics <- compute_branch_diagnostics(
  branch_mat   = branch_mat,
  log_mean_vec = log_mean
)
attr(out, "diagnostics") <- diagnostics

# plan(callr, workers = wf$execution$num_workers)
# fit  <- fit_integrated(cal)
# plan(sequential)



# FOR THURSDAY
# Branch diagnostics
# Desired pipeline:

# cal_wf <- workflow() |>
#   add(model) |>
#   add(estimand) |>
#   add(nuisance) |>
#   add(optimizer) |>
#   add(execution) |>
#   calibrate(data)
#
# log_IL <- cal_wf |>
#   generate()
#
# log_IL <- log_IL |>
#   diagnose()


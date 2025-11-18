# Load package internals
devtools::load_all()

set.seed(1)

# ============================================================
# SIMULATE POISSON DATA (toy example)
# Observations come from Poisson( exp(θ1 + θ2 * x) )
# ============================================================

n <- 200
x <- rnorm(n)
theta_true <- c(0.25, -0.8)          # true parameter vector
mu <- exp(theta_true[1] + theta_true[2] * x)
y <- rpois(n, lambda = mu)

data <- list(x = x, y = y)

# ============================================================
# USER-LIKELIHOOD (DATA IS EMBEDDED IN THE CLOSURE!)
# ============================================================

loglik <- function(theta) {
  mu <- exp(theta[1] + theta[2] * data$x)
  sum(dpois(data$y, lambda = mu, log = TRUE))
}

# Dummy placeholder for integrated likelihood testing
# E_loglik must depend on theta + omega_hat
E_loglik <- function(theta, omega_hat) {
  loglik(theta)  # not correct IL, but placeholder for structure
}

# ============================================================
# WEIGHTED-SUM ESTIMAND ψ(θ) = wᵀ θ
# ============================================================

w <- c(0.3, 0.7)

psi <- function(theta) {
  sum(w * theta)
}

# ============================================================
# MLE (OPTIONAL) – We can compute it via glm() here
# ============================================================

fit <- glm(y ~ x, family = poisson)
theta_mle <- coef(fit)

# ============================================================
# CREATE SPECS
# ============================================================

model <- model_spec(
  param_dim = 2,
  loglik    = loglik,
  E_loglik  = E_loglik,
  theta_mle = theta_mle,
  name      = "Poisson Regression (2-parameter)"
)

estimand <- estimand_spec(
  psi  = psi,
  name = "Weighted Sum (psi)"
)

nuisance <- nuisance_spec(
  sampler = function(n) rnorm(n),
  num_draws_per_branch = 1,
  name = "Dummy nuisance"
)

optimizer <- optimizer_spec(
  method = "nloptr",
  theta_init = theta_mle,
  strict = TRUE,       # requires theta_mle
  name = "Default optimizer"
)

execution <- execution_spec(
  mode = "serial",
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

fit  <- fit_integrated(cal)

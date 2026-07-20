# A3: on a jagged curve with several threshold crossings per side,
# find_interval_endpoints() must return the OUTERMOST crossing (widest
# interval), not an inner one that a single bracketed uniroot might land on.

make_loglik <- function(psi, ll, range) {
  f <- stats::approxfun(psi, ll, rule = 2)
  attr(f, "psi range") <- range
  f
}

test_that("find_interval_endpoints picks the outermost root on a jagged curve", {
  # global max 0 at psi=5. Left side has a secondary bump that rises back
  # above the -crit threshold, creating THREE left crossings (~1.54, ~2.46,
  # ~3.43). The lower endpoint must be the outermost (~1.54).
  psi <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
  ll  <- c(-5, -3, -1, -3, -0.5, 0, -0.5, -2.5, -4)
  f <- make_loglik(psi, ll, range = c(0, 8))

  ep <- find_interval_endpoints(f, alpha = 0.05, grid_increment = 0.25)

  # outermost left crossing ~1.54, NOT the inner ones at ~2.46 / ~3.43
  expect_gt(ep$lower, 1.3)
  expect_lt(ep$lower, 1.8)
  # single right crossing ~6.71
  expect_gt(ep$upper, 6.5)
  expect_lt(ep$upper, 6.9)
})

test_that("find_interval_endpoints matches the single crossing on a clean curve", {
  psi <- seq(0, 10, by = 0.5)
  ll  <- -0.5 * (psi - 5)^2          # smooth concave, max at 5
  f <- make_loglik(psi, ll, range = c(0, 10))

  ep <- find_interval_endpoints(f, alpha = 0.05, grid_increment = 0.5)
  crit <- 0.5 * qchisq(0.95, df = 1)
  # -0.5 (psi-5)^2 = -crit  ->  |psi-5| = sqrt(2*crit)
  half <- sqrt(2 * crit)
  expect_equal(ep$lower, 5 - half, tolerance = 0.05)
  expect_equal(ep$upper, 5 + half, tolerance = 0.05)
})

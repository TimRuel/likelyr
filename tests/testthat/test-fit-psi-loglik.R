test_that("fit_psi_loglik drops non-finite rows and still fits (audit A4)", {
  psi <- seq(0, 1, length.out = 12)
  loglik <- -10 * (psi - 0.5)^2
  loglik[3] <- NA_real_
  loglik[7] <- Inf
  df <- tibble::tibble(psi = psi, loglik = loglik)

  expect_warning(
    f <- fit_psi_loglik(df),
    "non-finite"
  )
  expect_true(is.function(f))
  # evaluates finite at an interior point
  expect_true(is.finite(f(0.5)))
  # range attribute is carried under the "psi range" key
  expect_false(is.null(attr(f, "psi range")))
})

test_that("fit_psi_loglik errors when too few finite points remain", {
  df <- tibble::tibble(
    psi = c(0, 0.25, 0.5, 0.75, 1),
    loglik = c(0, NA, NA, NA, -1)   # only 2 finite
  )
  expect_error(suppressWarnings(fit_psi_loglik(df)), "fewer than 4")
})

test_that("fit_psi_loglik is unchanged when all rows are finite", {
  psi <- seq(0, 1, length.out = 12)
  df <- tibble::tibble(psi = psi, loglik = -10 * (psi - 0.5)^2)
  expect_silent(f <- fit_psi_loglik(df))
  expect_true(is.finite(f(0.5)))
})

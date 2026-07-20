test_that("compute_common_interval uses the profile extent when no modes", {
  df <- tibble::tibble(psi = seq(1, 2, by = 0.05), loglik = 0)
  ci <- compute_common_interval(df, psi_interval = NULL, increment = 0.05)
  expect_equal(ci$psi_lower, 1)
  expect_equal(ci$psi_upper, 2)
  expect_false(ci$snapped_to_lower)
  expect_false(ci$snapped_to_upper)
})

test_that("compute_common_interval unions in branch seed modes (audit P1)", {
  df <- tibble::tibble(psi = seq(1, 2, by = 0.05), loglik = 0)
  ci <- compute_common_interval(
    df, psi_interval = NULL, increment = 0.05,
    branch_modes = c(0.7, 1.5, 2.4)
  )
  expect_equal(ci$psi_lower, 0.7)
  expect_equal(ci$psi_upper, 2.4)
})

test_that("compute_common_interval ignores non-finite modes", {
  df <- tibble::tibble(psi = seq(1, 2, by = 0.05), loglik = 0)
  ci <- compute_common_interval(
    df, psi_interval = NULL, increment = 0.05,
    branch_modes = c(0.7, NA, Inf, 2.4)
  )
  expect_equal(ci$psi_lower, 0.7)
  expect_equal(ci$psi_upper, 2.4)
})

test_that("compute_common_interval expands by interval_buffer about the span centre", {
  df <- tibble::tibble(psi = seq(1, 2, by = 0.05), loglik = 0)
  ci <- compute_common_interval(df, psi_interval = NULL, increment = 0.05,
                                interval_buffer = 2)
  # centre 1.5, half-width 0.5 -> *2 -> [0.5, 2.5]
  expect_equal(ci$psi_lower, 0.5)
  expect_equal(ci$psi_upper, 2.5)
})

test_that("compute_common_interval snaps to a nearby finite domain boundary", {
  df <- tibble::tibble(psi = seq(0.92, 2, by = 0.02), loglik = 0)
  ci <- compute_common_interval(
    df, psi_interval = sets::interval(0.9, 5), increment = 0.05
  )
  expect_true(ci$snapped_to_lower)
  expect_equal(ci$psi_lower, 0.9)
})

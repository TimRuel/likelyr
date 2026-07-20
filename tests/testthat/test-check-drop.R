test_that("check_drop rejects drops above the absolute cap", {
  # drop exceeds cap -> FALSE regardless of history
  expect_false(check_drop(drop = 100, recent_drops = c(1, 1, 1),
                          drop_multiplier = 2, max_drop_cap = 10,
                          k_recent = 3))
})

test_that("check_drop accepts while history is shorter than k_recent", {
  expect_true(check_drop(drop = 5, recent_drops = c(1),
                         drop_multiplier = 2, max_drop_cap = 100,
                         k_recent = 3))
})

test_that("check_drop applies the relative multiplier once history is full", {
  # median(recent) = 1, multiplier 2 -> threshold 2
  expect_true(check_drop(drop = 1.5, recent_drops = c(1, 1, 1),
                         drop_multiplier = 2, max_drop_cap = 100,
                         k_recent = 3))
  expect_false(check_drop(drop = 3, recent_drops = c(1, 1, 1),
                          drop_multiplier = 2, max_drop_cap = 100,
                          k_recent = 3))
})

test_that("check_drop accepts when the recent median is non-positive", {
  expect_true(check_drop(drop = 5, recent_drops = c(0, 0, 0),
                         drop_multiplier = 2, max_drop_cap = 100,
                         k_recent = 3))
})

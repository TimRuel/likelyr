test_that(".upper_convex_hull keeps all points of an already-concave curve", {
  x <- 0:4
  y <- c(0, 3, 4, 3, 0)   # strictly concave (downward parabola-ish)
  expect_equal(.upper_convex_hull(x, y), 1:5)
})

test_that(".upper_convex_hull drops a convex kink below the upper envelope", {
  x <- 0:4
  # a dip at the middle point (index 3) sits below the line joining its
  # neighbours -> it is not on the least concave majorant.
  y <- c(0, 3, 1, 3, 0)
  hull <- .upper_convex_hull(x, y)
  expect_false(3L %in% hull)
  expect_true(all(c(1L, 5L) %in% hull))   # endpoints always retained
})

test_that(".upper_convex_hull handles degenerate short inputs", {
  expect_equal(.upper_convex_hull(numeric(0), numeric(0)), integer(0))
  expect_equal(.upper_convex_hull(1, 1), 1L)
})

# Tests for the internal helpers that replaced plyr::mapvalues() and
# scales::rescale().

test_that("helper_mapvalues() replaces matched values and leaves the rest alone", {
  expect_equal(helper_mapvalues(c(1, 2, 3, 2), from = c(1, 2), to = c(10, 20)), c(10, 20, 3, 20))
  expect_equal(helper_mapvalues(c("a", "b", "c"), from = "b", to = "B"), c("a", "B", "c"))

  # replacing with NA
  expect_equal(helper_mapvalues(c(1, 8, 9), from = c(8, 9), to = c(NA, NA)), c(1, NA, NA))

  # the result takes the common type of x and to, as plyr::mapvalues() did
  expect_equal(helper_mapvalues(c(1, 2), from = c(1, 2), to = c("yes", "no")), c("yes", "no"))

  # NA in x is left as NA
  expect_equal(helper_mapvalues(c(1, NA), from = 1, to = 2), c(2, NA))

  # empty from is a no-op
  expect_equal(helper_mapvalues(1:3, from = numeric(), to = numeric()), 1:3)
})

test_that("helper_mapvalues() relabels factor levels and keeps their order", {
  f <- factor(c("lo", "hi", "lo"), levels = c("lo", "hi"))
  out <- helper_mapvalues(f, from = c("lo", "hi"), to = c("low", "high"))
  expect_s3_class(out, "factor")
  expect_equal(levels(out), c("low", "high"))
  expect_equal(as.character(out), c("low", "high", "low"))
})

test_that("helper_mapvalues() reports unmatched from values only when asked", {
  expect_message(helper_mapvalues(1:3, from = c(1, 9), to = c(10, 90)), "not present")
  expect_silent(helper_mapvalues(1:3, from = c(1, 9), to = c(10, 90), warn_missing = FALSE))
  expect_error(helper_mapvalues(1:3, from = 1:2, to = 1), "same length")
})

test_that("helper_rescale() maps the observed range onto the target range", {
  expect_equal(helper_rescale(c(2, 4, 6)), c(0, 50, 100))
  expect_equal(helper_rescale(c(2, 4, 6), to = c(0, 1)), c(0, 0.5, 1))

  # NA and non-finite values are ignored for the range and passed through
  expect_equal(helper_rescale(c(0, NA, 10)), c(0, NA, 100))
  expect_equal(helper_rescale(c(0, Inf, 10)), c(0, Inf, 100))

  # a constant vector maps to the midpoint, as scales::rescale() does
  expect_equal(helper_rescale(c(3, 3, 3)), c(50, 50, 50))
})

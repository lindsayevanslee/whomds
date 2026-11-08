# Tests for the small exported helper functions.

test_that("helper_rowSums() returns NA for all-NA rows when allNA0 = TRUE", {
  x <- data.frame(v1 = c(NA, 1:4), v2 = c(NA, 2:5), v3 = c(NA, 1:2, NA, 3))

  expect_equal(helper_rowSums(x, na.rm = TRUE, allNA0 = TRUE), c(NA, 4, 7, 7, 12))
  expect_equal(helper_rowSums(x, na.rm = TRUE, allNA0 = FALSE), c(0, 4, 7, 7, 12))

  # with no extra arguments it matches base::rowSums()
  y <- data.frame(a = 1:3, b = 4:6)
  expect_equal(helper_rowSums(y), base::rowSums(y))
})

test_that("helper_varslist() flattens, deduplicates, and sorts", {
  expect_equal(
    helper_varslist(list(a = c("c3", "c1"), b = c("c2", "c1"))),
    c("c1", "c2", "c3")
  )
  expect_equal(helper_varslist(c("b", "a")), c("a", "b"))
})

test_that("helper_indicator() creates mapped _ind columns", {
  df <- tibble::tibble(x = c(1, 2, 1), y = c(2, 2, 1))

  res <- helper_indicator(df, vars_indicators = c("x", "y"),
                          mapvalues_from = c(1, 2), mapvalues_to = c("yes", "no"))
  expect_true(all(c("x_ind", "y_ind") %in% names(res)))
  expect_s3_class(res$x_ind, "factor")
  expect_equal(as.character(res$x_ind), c("yes", "no", "yes"))
  expect_equal(as.character(res$y_ind), c("no", "no", "yes"))
  expect_equal(res$x, df$x) # original columns untouched

  res_chr <- helper_indicator(df, "x", c(1, 2), c("yes", "no"), make_factor = FALSE)
  expect_type(res_chr$x_ind, "character")
})

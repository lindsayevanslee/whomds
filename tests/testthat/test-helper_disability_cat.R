test_that("helper_disability_cat() reproduces disability_cat in the example data by default", {
  cat_default <- helper_disability_cat(df_adults$disability_score)

  expect_s3_class(cat_default, c("ordered", "factor"))
  expect_equal(levels(cat_default), c("No", "Mild", "Moderate", "Severe"))
  expect_equal(as.character(cat_default), as.character(df_adults$disability_cat))

  m <- mean(df_adults$disability_score, na.rm = TRUE)
  s <- stats::sd(df_adults$disability_score, na.rm = TRUE)
  expect_equal(attr(cat_default, "cutoffs"), c(m - s, m, m + s))
})

test_that("helper_disability_cat() applies explicit cut-offs with lower bounds inclusive", {
  out <- helper_disability_cat(c(0, 10, 20, 39.9, 40, 90, NA), cutoffs = c(20, 40, 60))
  expect_equal(as.character(out), c("No", "No", "Mild", "Mild", "Moderate", "Severe", NA))
  expect_equal(attr(out, "cutoffs"), c(20, 40, 60))

  # a score of 0 is always the first level, even if the lowest cut-off is below 0
  out0 <- helper_disability_cat(c(0, 1), cutoffs = c(-5, 2, 3))
  expect_equal(as.character(out0), c("No", "Mild"))

  # custom number of levels and labels
  out3 <- helper_disability_cat(c(0, 10, 50, 90), cutoffs = c(20, 60), labels = c("Low", "Medium", "High"))
  expect_equal(as.character(out3), c("Low", "Low", "Medium", "High"))
  expect_equal(levels(out3), c("Low", "Medium", "High"))
})

test_that("helper_disability_cat() validates its inputs", {
  expect_error(helper_disability_cat(c("a", "b")), "must be numeric")
  expect_error(helper_disability_cat(1:10, cutoffs = c(2, 4)), "one fewer element")
  expect_error(helper_disability_cat(1:10, cutoffs = c(4, 2, 6)), "strictly increasing")
  expect_error(helper_disability_cat(1:10, labels = c("a", "b", "c")), "supply `cutoffs`")
  expect_error(helper_disability_cat(1:10, cutoffs = c(2, 4), labels = c("a", "a", "b")), "distinct")
  expect_error(helper_disability_cat(c(NA_real_, 5)), "at least two non-missing")
})

test_that("the cutoffs attribute survives dplyr verbs", {
  df <- df_adults %>%
    dplyr::mutate(cat = helper_disability_cat(disability_score)) %>%
    dplyr::filter(!is.na(cat))
  expect_length(attr(df$cat, "cutoffs"), 3)
})

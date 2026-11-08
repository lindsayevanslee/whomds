# Smoke tests for the figure functions: each returns a ggplot that builds
# without error and without using deprecated ggplot2 features.

df_fig <- df_adults %>%
  dplyr::filter(!is.na(sex), !is.na(disability_score), !is.na(disability_cat), !is.na(age))

test_that("fig_density() builds without deprecation warnings", {
  p1 <- expect_no_deprecation(fig_density(df_fig, score = "disability_score", cutoffs = c(19.1, 34.4, 49.6)))
  expect_s3_class(p1, "ggplot")
  expect_no_deprecation(ggplot2::ggplot_build(p1))

  p2 <- expect_no_deprecation(fig_density(df_fig, score = "disability_score", var_color = "sex", var_facet = "age_cat", size = 1))
  expect_s3_class(p2, "ggplot")
  expect_no_deprecation(ggplot2::ggplot_build(p2))

  expect_error(fig_density(df_fig, score = "disability_score", var_color = "age"), "must be a factor")
})

test_that("fig_dist() builds without deprecation warnings, as counts and as percentages", {
  p_n <- expect_no_deprecation(fig_dist(df_fig, score = "disability_score", score_cat = "disability_cat", cutoffs = c(19.1, 34.4, 49.6), y_max = 2000))
  expect_s3_class(p_n, "ggplot")
  b_n <- expect_no_deprecation(ggplot2::ggplot_build(p_n))
  expect_equal(b_n$plot$labels$y, "Frequency")

  p_pct <- expect_no_deprecation(fig_dist(df_fig, score = "disability_score", score_cat = "disability_cat", cutoffs = c(19.1, 34.4, 49.6), pcent = TRUE))
  b_pct <- expect_no_deprecation(ggplot2::ggplot_build(p_pct))
  expect_equal(b_pct$plot$labels$y, "Percent")
  # with pcent = TRUE the bar heights are proportions summing to 1
  expect_equal(sum(b_pct$data[[1]]$y), 1)
})

test_that("fig_poppyramid() builds without deprecation warnings", {
  p <- expect_no_deprecation(fig_poppyramid(df_fig, var_age = "age", var_sex = "sex"))
  expect_s3_class(p, "ggplot")
  expect_no_deprecation(ggplot2::ggplot_build(p))
})

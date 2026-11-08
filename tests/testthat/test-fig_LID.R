# fig_LID() draws the local item dependence graph. Correlation matrices can
# contain NA when two items were never observed on the same respondents (items
# split by age group or by split_strategy); igraph >= 2.0 refuses NAs in an
# adjacency matrix, which used to break print_results = TRUE.

make_cor <- function() {
  m <- diag(4)
  dimnames(m) <- list(paste0("i", 1:4), paste0("i", 1:4))
  m["i1", "i2"] <- m["i2", "i1"] <- 0.5   # dependent pair
  m["i3", "i4"] <- m["i4", "i3"] <- NA    # never observed together
  m["i1", "i3"] <- m["i3", "i1"] <- 0.1   # below cut-off
  m
}

test_that("fig_LID() tolerates NA correlations and returns a ggplot", {
  out_dir <- withr::local_tempdir()

  p <- fig_LID(make_cor(), LIDcutoff = 0.2, path_output = out_dir)
  expect_s3_class(p, "ggplot")

  # the csv of correlations above the cut-off is written, with NA shown as 0
  csv <- utils::read.csv(file.path(out_dir, "LID_above_0.2.csv"), row.names = 1)
  expect_equal(csv["i1", "i2"], 0.5)
  expect_equal(csv["i3", "i4"], 0)
  expect_equal(csv["i1", "i3"], 0)
})

test_that("fig_LID() handles a matrix with no dependence above the cut-off", {
  out_dir <- withr::local_tempdir()
  m <- make_cor()
  m["i1", "i2"] <- m["i2", "i1"] <- 0.1

  p <- fig_LID(m, LIDcutoff = 0.2, path_output = out_dir, extra_file_label = "none")
  expect_s3_class(p, "ggplot")
  expect_true(file.exists(file.path(out_dir, "LID_above_0.2_none.csv")))
})

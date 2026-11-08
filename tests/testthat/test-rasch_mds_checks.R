# Input validation in rasch_mds(). These checks run before any model fitting,
# so the tests are fast.

test_that("rasch_mds() rejects resp_opts that do not start at 1", {
  expect_error(
    rasch_mds(df_adults, vars_metric = paste0("EF", 1:12), vars_id = "HHID",
              resp_opts = 2:5, max_NA = 2, print_results = FALSE),
    "resp_opts must start with 1"
  )
})

test_that("rasch_mds() rejects max_NA that is not smaller than the number of items", {
  expect_error(
    rasch_mds(df_adults, vars_metric = paste0("EF", 1:12), vars_id = "HHID",
              resp_opts = 1:5, max_NA = 12, print_results = FALSE),
    "max_NA must be less than length of vars_metric"
  )
})

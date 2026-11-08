# Regression tests for the person-ability columns returned by rasch_model().
#
# Background: eRm 1.0-7 added a "Std.Error" column to person.parameter()$theta.table.
# rasch_model() used to rename the columns of theta.table positionally, which
# mislabeled them and left the last column with an NA name, so the left_join()
# in rasch_rescale() failed with
#   "Can't use NA as column index in a tibble for assignment."

vars_metric <- paste0("EF", 1:12)

# Small subset so the test stays reasonably fast; items are recoded to start at 0
# as rasch_mds() would do before calling rasch_model()
df_small <- df_adults[1:300, ] %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(vars_metric), ~ . - 1),
    HHID = as.character(HHID)
  )

test_that("rasch_model() returns df_score with valid, correctly labeled person columns", {
  model_result <- suppressWarnings(rasch_model(df_small, vars_metric = vars_metric, vars_id = "HHID"))
  df_score <- model_result$df_score

  # no missing or empty column names
  expect_false(anyNA(names(df_score)))
  expect_true(all(nzchar(names(df_score))))

  # exactly the expected columns, in the expected order
  expect_identical(names(df_score), c("HHID", vars_metric, "person_pars", "person_pars_se", "NAgroup", "Interpolated"))
  expect_equal(nrow(df_score), nrow(df_small))

  # columns carry the right content, not shifted neighbours from theta.table
  expect_type(df_score$person_pars, "double")
  expect_false(anyNA(df_score$person_pars))
  # standard errors are positive wherever eRm could estimate one (#14)
  expect_type(df_score$person_pars_se, "double")
  expect_true(all(df_score$person_pars_se > 0, na.rm = TRUE))
  expect_true(all(!is.na(df_score$person_pars_se[!df_score$Interpolated])))
  expect_type(df_score$NAgroup, "integer")
  expect_type(df_score$Interpolated, "logical")
  expect_equal(
    df_score$Interpolated,
    model_result$person_parameters$theta.table$Interpolated,
    ignore_attr = TRUE
  )
})

test_that("rasch_DIF() runs without deprecation warnings", {
  model_result <- suppressWarnings(rasch_model(df_small, vars_metric = vars_metric, vars_id = "HHID"))
  # rasch_DIF() matches residuals to rows of df through the "person" column, as rasch_mds() does
  dif <- expect_no_deprecation(suppressMessages(
    rasch_DIF(df = df_small, vars_metric = vars_metric, vars_DIF = "sex",
              residuals_PCM = model_result$residuals_PCM)
  ))
  expect_type(dif$DIF_results, "character")
})

test_that("rasch_rescale() joins df_score without error and adds rescaled", {
  model_result <- suppressWarnings(rasch_model(df_small, vars_metric = vars_metric, vars_id = "HHID"))

  df_final <- suppressMessages(rasch_rescale(df_small, model_result$df_score, vars_id = "HHID"))

  expect_true(all(c("person_pars", "person_pars_se", "rescaled") %in% names(df_final)))
  expect_equal(nrow(df_final), nrow(df_small))
  expect_equal(range(df_final$rescaled), c(0, 100))
})

test_that("rasch_mds() runs end to end on the example data", {
  skip_on_cran()

  model_result <- expect_no_deprecation(suppressMessages(
    rasch_mds(
      df = df_adults,
      vars_metric = vars_metric,
      vars_id = "HHID",
      vars_DIF = c("sex", "age_cat"),
      resp_opts = 1:5,
      max_NA = 2,
      print_results = FALSE
    )
  ), ignore = "polychoric correlation")   # sparse response categories in EF12, expected

  expect_true(all(c("person_pars", "person_pars_se", "rescaled") %in% names(model_result$df)))
  expect_false(anyNA(names(model_result$df)))
  expect_equal(range(model_result$df$rescaled), c(0, 100))
})

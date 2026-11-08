# Smoke test for the children pipeline. It uses TAM rather than eRm, so it is
# exposed to a different set of dependency changes than rasch_mds(). The call
# mirrors the example in the c4_rasch_children vignette.

test_that("rasch_mds_children() runs end to end on the example data", {
  res <- expect_no_deprecation(suppressMessages(
    rasch_mds_children(
      df = df_children,
      vars_id = "HHID",
      vars_group = "age_cat",
      vars_metric_common = paste0("child", 1:10),
      vars_metric_grouped = list(
        "Age2to4"   = paste0("child", c(12, 15, 16, 19, 20, 24, 25)),
        "Age5to9"   = paste0("child", c(11, 13, 14, 17, 18, 20, 21, 22, 23, 24, 25, 27)),
        "Age10to17" = paste0("child", c(11, 13, 14, 17, 18, 20, 21, 22, 23, 25, 26, 27))
      ),
      TAM_model = "PCM2",
      resp_opts = 1:5,
      has_at_least_one = 4:5,
      max_NA = 10,
      print_results = FALSE
    )
  ))

  expect_named(res, c("df", "vars_metric"))
  expect_s3_class(res$df, "data.frame")
  expect_false(anyNA(names(res$df)))

  expect_true(all(c("person_pars", "person_pars_se", "rescaled") %in% names(res$df)))
  expect_false(anyNA(res$df$rescaled))
  # WLE standard errors are present and positive for every child (#14)
  expect_false(anyNA(res$df$person_pars_se))
  expect_true(all(res$df$person_pars_se > 0))
  expect_equal(range(res$df$rescaled), c(0, 100))
  expect_true(all(res$df$age_cat %in% c("Age2to4", "Age5to9", "Age10to17")))

  # common items are kept as they are
  expect_true(all(paste0("child", 1:10) %in% names(res$df)))
  # items shared by several age groups are split by age group ...
  expect_true(all(c("child11_Age5to9", "child11_Age10to17") %in% names(res$df)))
  expect_setequal(res$vars_metric$Age5to9, paste0("child", c(11, 13, 14, 17, 18, 20, 21, 22, 23, 24, 25, 27), "_Age5to9"))
  # ... while items belonging to a single age group are not
  expect_true("child12" %in% res$vars_metric$Age2to4)
  expect_false("child12_Age2to4" %in% names(res$df))
})

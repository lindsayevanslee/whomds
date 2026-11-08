# Value checks for the table functions. The existing test-dimensions.R file
# only checks table shapes; these tests check that the numbers are right.

# table_weightedpct() -------------------------------------------------------

test_that("table_weightedpct() percentages sum to 100 within each group and item", {
  withr::local_options(survey.lonely.psu = "adjust")

  tab <- table_weightedpct(
    df_adults, vars_ids = c("PSU", "HHID"), vars_strata = "strata", vars_weights = "weight",
    formula_vars = paste0("EF", 1:3), formula_vars_levels = 1:5, by_vars = "sex"
  )

  sums <- tab %>%
    dplyr::group_by(sex, item) %>%
    dplyr::summarize(total = sum(prop), .groups = "drop")
  expect_equal(nrow(sums), 2 * 3)
  expect_equal(sums$total, rep(100, nrow(sums)))

  expect_false("prop_se" %in% names(tab))
  expect_true(all(tab$prop >= 0 & tab$prop <= 100))
})

test_that("table_weightedpct() returns standard errors and weighted totals when asked", {
  withr::local_options(survey.lonely.psu = "adjust")

  tab_se <- table_weightedpct(
    df_adults, vars_ids = c("PSU", "HHID"), vars_strata = "strata", vars_weights = "weight",
    formula_vars = "EF1", formula_vars_levels = 1:5, by_vars = "sex", include_SE = TRUE
  )
  expect_true("prop_se" %in% names(tab_se))
  expect_true(all(tab_se$prop_se >= 0))

  tab_n <- table_weightedpct(
    df_adults, vars_ids = c("PSU", "HHID"), vars_strata = "strata", vars_weights = "weight",
    formula_vars = "EF1", formula_vars_levels = 1:5, pct = FALSE
  )
  expect_true("total" %in% names(tab_n))
  # weighted totals over all response options equal the summed weights of respondents to the item
  expect_equal(sum(tab_n$total), sum(df_adults$weight[!is.na(df_adults$EF1)]))
})

test_that("table_weightedpct() adds Total rows of 100 and pivots by spread_key", {
  withr::local_options(survey.lonely.psu = "adjust")

  tab_tot <- table_weightedpct(
    df_adults, vars_ids = c("PSU", "HHID"), vars_strata = "strata", vars_weights = "weight",
    formula_vars = paste0("EF", 1:2), formula_vars_levels = 1:5, by_vars = "sex", add_totals = TRUE
  )
  totals <- dplyr::filter(tab_tot, resp == "Total")
  expect_equal(nrow(totals), 2 * 2)
  expect_equal(totals$prop, rep(100, nrow(totals)))

  tab_wide <- table_weightedpct(
    df_adults, vars_ids = c("PSU", "HHID"), vars_strata = "strata", vars_weights = "weight",
    formula_vars = paste0("EF", 1:2), formula_vars_levels = 1:5, by_vars = "sex", spread_key = "resp"
  )
  expect_true(all(as.character(1:5) %in% names(tab_wide)))
  expect_equal(nrow(tab_wide), 2 * 2)
})

test_that("table_weightedpct() rejects NAs in by_vars", {
  withr::local_options(survey.lonely.psu = "adjust")
  df_na <- df_adults
  df_na$sex[1] <- NA

  expect_error(
    table_weightedpct(
      df_na, vars_ids = c("PSU", "HHID"), vars_strata = "strata", vars_weights = "weight",
      formula_vars = "EF1", formula_vars_levels = 1:5, by_vars = "sex"
    ),
    "Remove NAs from by_vars"
  )
})

test_that("table_weightedpct() warns when the lonely-PSU option is not set", {
  withr::local_options(survey.lonely.psu = "fail")
  expect_warning(
    table_weightedpct(
      df_adults, vars_ids = c("PSU", "HHID"), vars_strata = "strata", vars_weights = "weight",
      formula_vars = "EF1", formula_vars_levels = 1:5
    ),
    "lonely PSUs"
  )
})

# table_unweightedpctn() ----------------------------------------------------

test_that("table_unweightedpctn() counts and percentages are consistent with the data", {
  tab <- table_unweightedpctn(df_adults, c("sex", "edu_cat"))

  expect_equal(unique(as.character(tab$item)), c("sex", "edu_cat"))

  n_by_item <- tab %>% dplyr::group_by(item) %>% dplyr::summarize(n = sum(n), pct = sum(pct))
  expect_equal(n_by_item$n[n_by_item$item == "sex"], sum(!is.na(df_adults$sex)))
  expect_equal(n_by_item$n[n_by_item$item == "edu_cat"], sum(!is.na(df_adults$edu_cat)))
  expect_equal(n_by_item$pct, rep(100, 2), tolerance = 0.005) # percentages are rounded to 1 decimal

  n_female <- tab$n[tab$item == "sex" & tab$demo == "Female"]
  expect_equal(n_female, sum(df_adults$sex == "Female", na.rm = TRUE))
})

test_that("table_unweightedpctn() grouped percentages sum to 100 along the requested margin", {
  # default: within each level of group_by_var, percentages over demo sum to 100
  tab <- table_unweightedpctn(df_adults, "sex", group_by_var = "edu_cat")
  sums <- tab %>% dplyr::group_by(edu_cat) %>% dplyr::summarize(pct = sum(pct))
  expect_equal(sums$pct, rep(100, nrow(sums)), tolerance = 0.005)

  # group_by_var_sums_to_100: within each demo level, percentages over group_by_var sum to 100
  tab2 <- table_unweightedpctn(df_adults, "sex", group_by_var = "edu_cat", group_by_var_sums_to_100 = TRUE)
  sums2 <- tab2 %>% dplyr::group_by(demo) %>% dplyr::summarize(pct = sum(pct))
  expect_equal(sums2$pct, rep(100, nrow(sums2)), tolerance = 0.005)

  # the grouped table has one row per (demo, group) combination
  expect_equal(nrow(tab), length(levels(df_adults$sex)) * length(levels(df_adults$edu_cat)))
})

test_that("table_unweightedpctn() spreads group_by_var into pct and n columns", {
  tab <- table_unweightedpctn(df_adults, "sex", group_by_var = "edu_cat", spread_by_group_by_var = TRUE)

  expected_cols <- c(paste0(levels(df_adults$edu_cat), "_pct"), paste0(levels(df_adults$edu_cat), "_n"))
  expect_true(all(expected_cols %in% names(tab)))
  expect_equal(nrow(tab), length(levels(df_adults$sex)))
  expect_true(all(vapply(tab[expected_cols], is.numeric, logical(1))))
})

# table_basicstats() --------------------------------------------------------

test_that("table_basicstats() computes household-size statistics per group and in total", {
  # three households: sizes 2, 3 and 1
  df_hh <- tibble::tibble(
    HHID = c(1, 1, 2, 2, 2, 3),
    grp  = c("A", "B", "A", "A", "B", "B")
  )

  tab <- suppressMessages(table_basicstats(df_hh, hh_id = "HHID", group_by_var = "grp"))

  # a factor grouping variable must give the same result as a character one
  df_hh_fct <- dplyr::mutate(df_hh, grp = factor(grp))
  tab_fct <- suppressMessages(table_basicstats(df_hh_fct, hh_id = "HHID", group_by_var = "grp"))
  expect_equal(tab, tab_fct)

  expect_equal(names(tab), c("grp", "mean_sd", "median", "range"))
  expect_equal(tab$grp, c("A", "B", "Total"))

  # A per household: 1, 2, 0 (household 3 has no A, filled as 0)
  expect_equal(tab$mean_sd[tab$grp == "A"], "1 (1)")
  expect_equal(tab$median[tab$grp == "A"], 1)
  expect_equal(tab$range[tab$grp == "A"], "0 - 2")

  # B per household: 1, 1, 1
  expect_equal(tab$mean_sd[tab$grp == "B"], "1 (0)")
  expect_equal(tab$range[tab$grp == "B"], "1 - 1")

  # Total household sizes: 2, 3, 1
  expect_equal(tab$mean_sd[tab$grp == "Total"], "2 (1)")
  expect_equal(tab$median[tab$grp == "Total"], 2)
  expect_equal(tab$range[tab$grp == "Total"], "1 - 3")
})

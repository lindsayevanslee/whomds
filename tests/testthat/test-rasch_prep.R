# Unit tests for the data-preparation steps that rasch_mds() runs before
# fitting the model: recoding, splitting, testlets, dropping items, and raw
# scores. All of these are pure data transformations, so the tests use a small
# synthetic data set (items already shifted to start at 0, as in rasch_mds()).

df_prep <- tibble::tibble(
  id  = as.character(1:6),
  sex = c("M", "F", "M", "F", "M", "F"),
  c1  = c(0, 1, 2, 3, 4, 2),
  c2  = c(1, 1, 2, 2, 3, 4),
  c3  = c(0, 0, 1, 1, 2, 2),
  c4  = c(4, 3, 2, 1, 0, 1)
)
vars_prep <- c("c1", "c2", "c3", "c4")
max_values_prep <- tibble::tibble(var = vars_prep, max_val = 4)
resp_opts_prep <- 1:5

# rasch_recode() ------------------------------------------------------------

test_that("rasch_recode() collapses categories and updates max_values", {
  res <- rasch_recode(
    df = df_prep, vars_metric = vars_prep,
    recode_strategy = list("c1,c2" = c(0, 1, 2, 3, 3), "c3" = c(0, 1, 1, 2, 2)),
    max_values = max_values_prep
  )

  expect_equal(res$df$c1, c(0, 1, 2, 3, 3, 2))
  expect_equal(res$df$c2, c(1, 1, 2, 2, 3, 3))
  expect_equal(res$df$c3, c(0, 0, 1, 1, 1, 1))
  expect_equal(res$df$c4, df_prep$c4) # untouched

  expect_equal(
    dplyr::pull(dplyr::arrange(res$max_values, var), max_val),
    c(3, 3, 2, 4)
  )
})

test_that("rasch_recode() rejects invalid strategies", {
  expect_error(
    rasch_recode(df_prep, vars_prep, list("c9" = c(0, 1, 2, 3, 3)), max_values_prep),
    "not included in the variable list"
  )
  expect_error(
    rasch_recode(df_prep, vars_prep, list("c1" = c(0, 1, 2)), max_values_prep),
    "same # of characters"
  )
  max_values_mixed <- dplyr::mutate(max_values_prep, max_val = ifelse(var == "c2", 3, max_val))
  expect_error(
    rasch_recode(df_prep, vars_prep, list("c1,c2" = c(0, 1, 2, 3, 3)), max_values_mixed),
    "different possible response options"
  )
})

# rasch_split() -------------------------------------------------------------

test_that("rasch_split() creates one column per category and updates metadata", {
  res <- rasch_split(
    df = df_prep, vars_metric = vars_prep,
    split_strategy = list("sex" = "c1"),
    max_values = max_values_prep
  )

  expect_true(all(c("c1_M", "c1_F") %in% names(res$df)))
  expect_equal(res$df$c1_M, ifelse(df_prep$sex == "M", df_prep$c1, NA))
  expect_equal(res$df$c1_F, ifelse(df_prep$sex == "F", df_prep$c1, NA))

  expect_setequal(res$vars_metric, c("c2", "c3", "c4", "c1_M", "c1_F"))
  expect_false("c1" %in% res$max_values$var)
  expect_equal(
    dplyr::filter(res$max_values, var %in% c("c1_M", "c1_F"))$max_val,
    c(4, 4)
  )
})

test_that("rasch_split() handles a list-valued vars_metric", {
  vars_list <- list(common = c("c1", "c2"), older = c("c3", "c4"))
  res <- rasch_split(df_prep, vars_list, list("sex" = "c1"), max_values_prep)

  expect_setequal(res$vars_metric$common, c("c2", "c1_M", "c1_F"))
  expect_equal(res$vars_metric$older, c("c3", "c4"))
})

test_that("rasch_split() rejects unknown variables", {
  expect_error(
    rasch_split(df_prep, vars_prep, list("region" = "c1"), max_values_prep),
    "not included in the variable list"
  )
  expect_error(
    rasch_split(df_prep, vars_prep, list("sex" = "c9"), max_values_prep),
    "not included in the variable list"
  )
})

# rasch_testlet() -----------------------------------------------------------

test_that("rasch_testlet() sums items into a named testlet", {
  res <- rasch_testlet(
    df = df_prep, vars_metric = vars_prep,
    testlet_strategy = list(t12 = c("c1", "c2")),
    max_values = max_values_prep, resp_opts = resp_opts_prep
  )

  expect_equal(res$df$t12, df_prep$c1 + df_prep$c2)
  expect_setequal(res$vars_metric, c("c3", "c4", "t12"))
  expect_false(any(c("c1", "c2") %in% res$max_values$var))
  expect_equal(dplyr::filter(res$max_values, var == "t12")$max_val, 8)
})

test_that("rasch_testlet() names unnamed testlets from their items", {
  res <- rasch_testlet(df_prep, vars_prep, list(c("c3", "c4")), max_values_prep, resp_opts_prep)

  expect_true("c3_c4" %in% names(res$df))
  expect_equal(names(res$testlet_strategy), "c3_c4")
  expect_setequal(res$vars_metric, c("c1", "c2", "c3_c4"))
})

test_that("rasch_testlet() rejects unknown variables", {
  expect_error(
    rasch_testlet(df_prep, vars_prep, list(c("c1", "c9")), max_values_prep, resp_opts_prep),
    "not included in the variable list"
  )
})

# rasch_drop() --------------------------------------------------------------

test_that("rasch_drop() removes items from vars_metric and max_values", {
  res <- rasch_drop(vars_prep, drop_vars = "c2", max_values = max_values_prep)
  expect_equal(res$vars_metric, c("c1", "c3", "c4"))
  expect_equal(res$max_values$var, c("c1", "c3", "c4"))

  res_list <- rasch_drop(list(a = c("c1", "c2"), b = c("c3", "c4")), c("c2", "c3"), max_values_prep)
  expect_equal(res_list$vars_metric, list(a = "c1", b = "c4"))
  expect_equal(res_list$max_values$var, c("c1", "c4"))
})

test_that("rasch_drop() rejects unknown variables", {
  expect_error(rasch_drop(vars_prep, "c9", max_values_prep), "not included in the variable list")
})

# rasch_rawscore() ----------------------------------------------------------

test_that("rasch_rawscore() adds artificial MIN and MAX rows only when needed", {
  # Nobody in df_prep scores 0 or the maximum of 16, so both rows are added
  res <- suppressMessages(
    rasch_rawscore(df_prep, vars_prep, vars_id = "id", max_values = max_values_prep)
  )

  expect_equal(nrow(res), nrow(df_prep) + 2)
  expect_equal(res$RawScore[1:6], c(5, 5, 7, 7, 9, 9))

  row_max <- dplyr::filter(res, id == "MAX")
  expect_equal(nrow(row_max), 1)
  expect_equal(unlist(row_max[, vars_prep], use.names = FALSE), rep(4, 4))
  expect_equal(row_max$RawScore, 16)

  row_min <- dplyr::filter(res, id == "MIN")
  expect_equal(nrow(row_min), 1)
  expect_equal(unlist(row_min[, vars_prep], use.names = FALSE), rep(0, 4))
  expect_equal(row_min$RawScore, 0)

  # When both extremes are already attained, no rows are added
  df_extremes <- dplyr::bind_rows(
    df_prep,
    tibble::tibble(id = "7", sex = "M", c1 = 0, c2 = 0, c3 = 0, c4 = 0),
    tibble::tibble(id = "8", sex = "F", c1 = 4, c2 = 4, c3 = 4, c4 = 4)
  )
  res2 <- suppressMessages(
    rasch_rawscore(df_extremes, vars_prep, vars_id = "id", max_values = max_values_prep)
  )
  expect_equal(nrow(res2), nrow(df_extremes))
  expect_false(any(res2$id %in% c("MIN", "MAX")))
})

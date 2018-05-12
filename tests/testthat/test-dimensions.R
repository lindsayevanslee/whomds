context("Dimensions of tables")

test_that("Dimensions as number of levels and 2", {
  tab <- table_unweightedpctn(mdstest, c("sex", "edu_cat"))
  
  expect_equal(nrow(tab),
               length(c(
                 levels(mdstest$sex), levels(mdstest$edu_cat)
               )))
  expect_equal(ncol(tab), 2)
})


test_that("Dimensions as multiple of formula_vars, levels, and by_vars", {
  
  formula_vars <- paste0("fa",1:10)
  formula_vars_levels <-1:5
  by_vars <- "sex"
  
  tab <- table_weightedpct(mdstest, 
                           vars_ids = c("VARUNIT_N","enc_id"),
                           vars_strata = "VARSTRAT_N",
                           vars_weights = "Factor_Persona",
                           formula_vars = formula_vars,
                           formula_vars_levels = formula_vars_levels,
                           by_vars = by_vars)
  
  expect_equal(nrow(tab),
               length(formula_vars) * length(formula_vars_levels) * length(levels(dplyr::pull(mdstest,by_vars)))
  )
  expect_equal(ncol(tab), 4)
  
  
  
}
)

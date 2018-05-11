context("Dimensions of tables")

test_that("Dimensions as number of levels and 2", {
  tab <- table_unweightedpctn(mdstest, c("sex", "edu_cat"))
  
  expect_equal(nrow(tab),
               length(c(
                 levels(mdstest$sex), levels(mdstest$edu_cat)
               )))
  expect_equal(ncol(tab), 2)
})

#' whomds: A package for calculating results from WHO Model Disability Survey
#'
#' The whomds package provides two categories of important functions:
#' table functions and graph functions
#'
#' @section Table functions:
#' The table functions output different fit for purpose tables for reporting
#' results from the WHO Model Disability Survey (MDS)
#' 
#'
#' @section Graph functions:
#' The graph functions output different graphs for reporting
#' results from the WHO Model Disability Survey (MDS)
#'
#' @docType package
#' @name whomds
NULL

#quiets R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "age_cat",
      "demo",
      "item",
      "median",
      "nperHH",
      "pct",
      "pct_n",
      "prop",
      "prop_se",
      "resp",
      "sd",
      "sex"
    )
  )
}
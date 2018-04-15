#' whomds: A package for calculating results from WHO Model Disability Survey
#'
#' The whomds package provides three categories of important functions:
#' table functions, figure functions, and Rasch Analysis functions
#'
#' @section Table functions:
#' The table functions output different fit for purpose tables for reporting
#' results from the WHO Model Disability Survey (MDS). They begin with \code{table_*()}
#'  
#' @section Figure functions:
#' The graph functions output different graphs for reporting
#' results from the WHO Model Disability Survey (MDS). They begin with \code{fig_*()}
#'
#' @section Rasch Analysis functions:
#' These functions are used to complete an iteration of Rasch Analysis for WHO Model Disability Survey (MDS). They begin with \code{rasch_*}
#'
#'
#' @references WHO Model Disability Survey: \url{http://www.who.int/disabilities/data/mds/en/}
#'
#' @docType package
#' @name whomds
NULL

#quiets R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c( 
      ".",
      "age_cat",
      "demo",
      "item",
      "layout.kamada.kawai",
      "max_val",
      "median",
      "nperHH",
      "pct",
      "pct_n",
      "person",
      "person_pars",
      "prop",
      "prop_se",
      "RawScore",
      "resp",
      "sd",
      "sex",
      "var",
      "vars_id"
    )
  )
}
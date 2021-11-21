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
#' The figures functions output different figures for reporting
#' results from the WHO Model Disability Survey (MDS). They begin with \code{fig_*()}
#'
#' @section Rasch Analysis functions:
#' These functions are used to complete an iteration of Rasch Analysis for WHO Model Disability Survey (MDS). They begin with \code{rasch_*}
#'
#'
#' @references WHO Model Disability Survey: \url{https://www.who.int/health-topics/disability}
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
      "anchored_xsi",
      "cor_anchored", 
      "cor_multigroup", 
      "cor_start", 
      "data",
      "demo",
      "df_split",
      "df_split_selected",
      "disordered",
      "fit",
      "index_uniqueitems",
      "item",
      "layout.kamada.kawai",
      "max_val",
      "median",
      "Metric",
      "mod_anchored",
      "mod_multigroup", 
      "mod_start", 
      "model_results",
      "multigroup_xsi", 
      "n_threshold",
      "name",
      "nodes",
      "nperHH",
      "original",
      "original_var",
      "pct",
      "pct_n",
      "person",
      "person_pars",
      "prop",
      "prop_se",
      "Q",
      "RawScore",
      "recoded",
      "resp",
      "sd",
      "SE Threshold",
      "sex",
      "split_category",
      "start_xsi",
      "Threshold",
      "total_se",
      "var",
      "var1",
      "var2",
      "Var1",
      "variable",
      "variable_to_split",
      "vars_id",
      "vert_clr",
      "WLE_anchored"
    )
  )
}
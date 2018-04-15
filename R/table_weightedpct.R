
#' Calculate table of percentages of response distribution for survey items, survey weighted, disaggregated
#'
#' @param df a tibble of survey responses, one row per respondent
#' @param vars_ids a character vector of cluster ids, passed to a \code{survey::svydesign} object
#' @param vars_strata a character vector of strata ids, passed to a \code{survey::svydesign} object
#' @param vars_weights a character vector of survey weight ids, passed to a \code{survey::svydesign} object
#' @param formula_vars a character vector of variables to calculate the percentages of each level for
#' @param formula_vars_levels a vector of the levels of the the \code{formula_vars}
#' @param by_vars a character vector of variables to disaggregate results by
#' @param spread_key a string with variable name to pass to \code{key} argument of \code{dplyr::spread()}. Default is NULL.
#' @param spread_value a string with variable name to pass to \code{value} argument of \code{dplyr::spread()}. Default is "prop" (the columm of percentages created within the function)
#' @param arrange_vars a character vector with variables to pass to \code{dplyr::arrange()}. Default is NULL.
#' @param willfilter a logical variable that tells the function whether or not to filter or transmute the data. Leave as default NULL to not filter or transmute. Set as TRUE to filter and FALSE to transmute. See Details.
#' @param ... captures expressions to pass to \code{dplyr::filter()} or \code{dplyr::transmute()}, depending on the value of argument \code{willfilter}. See Details.
#'
#' @return a tibble of weighted response percentages 
#' 
#' @details 
#' If \code{willfilter} is NULL, the table is not filtered or transmuted. If \code{willfilter} is TRUE, the table is filtered before it is spread or arranged. If \code{willfilter} is FALSE, the table is transmuted after the spread and/or arrange. "..." captures the non-standard evaluation expressions (NSE) to pass to \code{dplyr::filter} or \code{dplyr::transmute()}.
#' 
#' @note Right now, if \code{willfilter} is non-NULL, then every argument needs to be explictly named (i.e., cannot just use default values) in order for NSE expressions to be properly used. This is a bug that will be fixed.
#' 
#' 
#' @family table functions
#' 
#' @export
#' 
#' @import srvyr
#' 
#' @seealso See \code{vignette("programming", package = "dplyr")} for more about non-standard evaluation (NSE)
#'
#' @examples
#' table_weightedpct(mdstest, 
#'     vars_ids = c("VARUNIT_N","enc_id"),
#'     vars_strata = "VARSTRAT_N",
#'     vars_weights = "Factor_Persona",
#'     formula_vars = paste0("fa",1:10),
#'     formula_vars_levels = 1:5,
#'     by_vars = "sex")
table_weightedpct <- function(df, vars_ids, vars_strata, vars_weights, 
                       formula_vars, formula_vars_levels = 0:1, by_vars, 
                       spread_key = NULL, spread_value = "prop",
                       arrange_vars = NULL, 
                       willfilter = NULL, ...
                       ) {

  #convert data to long format using variables from formula_vars
  df <- df %>%
    tidyr::gather_(
      key = "item",
      value = "resp",
      gather_cols = formula_vars,
      factor_key = TRUE,
      na.rm = TRUE
    ) %>%
    mutate(resp = ordered(resp, levels = formula_vars_levels),
           item = ordered(item))


  #warning if lonely psu option is not set correctly
  if (getOption("survey.lonely.psu")!="adjust") warning('You may have issues with lonely PSUs if you have not set: options(survey.lonely.psu = "adjust")')
  
  
  #create survey design object
  des <-
    df %>%
    as_survey_design_(
      ids = vars_ids,
      strata = vars_strata,
      weights = vars_weights,
      nest = TRUE
    )
  
  #store ... expressions for filter() or transmute()
  if (!is.null(willfilter)) {
    exprs <- quos(...)
    if (length(exprs)==0) stop("willfilter is not NULL but you didn't include any expressions to pass to filter() or transmute()")
  }
  
  #initialize results table
  prevtab <- des %>%
    group_by_at(c(by_vars, "item", "resp")) %>%
    summarize(prop = survey_mean(na.rm=TRUE)) %>%
    dplyr::select(-prop_se) %>%
    mutate(prop = prop*100)
  
  
  #filter, if willfilter==TRUE
  if (!is.null(willfilter) & isTRUE(willfilter)) prevtab <- prevtab %>% filter(!!!exprs)
  
  
  #spread 
  if (!is.null(spread_key)) {
    prevtab <- prevtab %>%
      tidyr::spread_(key = spread_key, value = spread_value) 
  }
  
  #arrange
  if (!is.null(arrange_vars)) {
    prevtab <- prevtab %>%
      arrange_at(arrange_vars)
  }
  
  
  #transmute, if willfilter==FALSE (collapse response options) -  (if spread_key is performance_cat, then can't use transmute here to collapse response options)
  if (!is.null(willfilter) & !isTRUE(willfilter)) prevtab <- prevtab %>% transmute(!!!exprs)
  
  
  return(prevtab)
  
}

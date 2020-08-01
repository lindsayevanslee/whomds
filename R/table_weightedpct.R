#' Calculate table of percentages or N of response distribution for survey items, survey weighted, disaggregated
#'
#' @param vars_ids a character vector of cluster ids, passed to a \code{survey::svydesign} object
#' @param vars_strata a character vector of strata ids, passed to a \code{survey::svydesign} object
#' @param vars_weights a character vector of survey weight ids, passed to a \code{survey::svydesign} object
#' @param formula_vars a character vector of variables to calculate the percentages of each level for
#' @param ... captures expressions to pass to \code{dplyr::filter()} or \code{dplyr::transmute()}, depending on the value of argument \code{willfilter}. See Details.
#' @param formula_vars_levels a vector of the levels of the the \code{formula_vars}
#' @param by_vars a character vector of variables to disaggregate results by. Default is \code{NULL} for no disaggregation. The columns listed must not include NAs.
#' @param pct a logical variable indicating whether or not to calculate weighted percentages. Default is \code{TRUE} for weighted percentages. Set to \code{FALSE} for weighted N.
#' @param willfilter a logical variable that tells the function whether or not to filter or transmute the data. Leave as default \code{NULL} to not filter or transmute. Set as \code{TRUE} to filter and \code{FALSE} to transmute. See Details.
#' @param spread_key a string with variable name to pass to \code{key} argument of \code{tidyr::spread()}. Default is \code{NULL}.
#' @param spread_value a string with variable name to pass to \code{value} argument of \code{tidyr::spread()}. Default is "prop" (the columm of percentages created within the function)
#' @param arrange_vars a character vector with variables to pass to \code{dplyr::arrange()}. Default is NULL.
#' @param include_SE a logical variable indicating whether to include the standard errors in the table. Default is FALSE. Currently does not work when adding totals, spreading or transmutting.
#' @inheritParams rasch_mds
#' @inheritParams table_unweightedpctn
#' 
#' @return a tibble of weighted response percentages or N's
#' 
#' @details 
#' If \code{willfilter} is NULL, the table is not filtered or transmuted. If \code{willfilter} is TRUE, the table is filtered before it is spread or arranged. If \code{willfilter} is FALSE, the table is transmuted after the spread and/or arrange. "..." captures the non-standard evaluation expressions (NSE) to pass to \code{dplyr::filter} or \code{dplyr::transmute()}.
#' 
#' The function performs the following actions with the table after results are calculated in the following order (if applicable): filter, add totals, spread, arrange, transmute
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
#' table_weightedpct(df_adults, 
#'     vars_ids = c("HHID", "PSU"),
#'     vars_strata = "strata",
#'     vars_weights = "weight",
#'     formula_vars = paste0("EF",1:10),
#'     formula_vars_levels = 1:5,
#'     by_vars = "sex")
table_weightedpct <- function(df, vars_ids, vars_strata, vars_weights, 
                       formula_vars, ..., 
                       formula_vars_levels = 0:1, by_vars = NULL,
                       pct = TRUE,
                       willfilter = NULL, 
                       add_totals = FALSE,
                       spread_key = NULL, spread_value = "prop",
                       arrange_vars = NULL,
                       include_SE = FALSE
                       ) {

  ##TESTING: if include_SE = TRUE, don't allow for adding totals, spreading, filtering, transmuting
  if (include_SE) {
    if(add_totals) stop("For testing of include_SE, add_totals must be FALSE")
    if(isFALSE(willfilter)) stop("For testing of include_SE, willfilter must be NULL or TRUE (i.e., must not transmute in order to avoid SE's from being erroneously summed)")
    if(!is.null(spread_key)) stop("For testing of include_SE, spread_key must be NULL")
  }
  
  #check for NAs in by_vars
  if (!is.null(by_vars)) {
    any_NAs <- df %>% 
      select(!!by_vars) %>% 
      filter_all(is.na) %>% 
      nrow() %>% 
      `>`(., 0)
  } else any_NAs <- FALSE
  
  if(any_NAs) stop("Remove NAs from by_vars columns first.")
  
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #convert data to long format using variables from formula_vars
  df <- df %>%
    tidyr::gather(
      key = "item",
      value = "resp",
      !!!rlang::syms(formula_vars),
      factor_key = TRUE,
      na.rm = TRUE
    ) %>%
    # mutate(resp = ordered(resp, levels = formula_vars_levels),
    mutate(resp = factor(resp, levels = formula_vars_levels),
           item = ordered(item))


  #warning if lonely psu option is not set correctly
  if (getOption("survey.lonely.psu")!="adjust") warning('You may have issues with lonely PSUs if you have not set: options(survey.lonely.psu = "adjust")')
  
  
  #create survey design object
  if (is.null(vars_ids)) expr_ids <- "NULL" else expr_ids <- paste0("c(", paste0(vars_ids, collapse = ","), ")")
  
  if (is.null(vars_strata)) expr_strata <- "NULL" else expr_strata <- paste0("c(", paste0(vars_strata, collapse = ","), ")")
  
  if (is.null(vars_weights)) expr_weights <- "NULL" else expr_weights <- paste0("c(", paste0(vars_weights, collapse = ","), ")")
  
  des <-
    df %>%
    as_survey_design(
      ids = !!rlang::parse_expr(expr_ids),
      strata = !!rlang::parse_expr(expr_strata),
      weights = !!rlang::parse_expr(expr_weights),
      nest = TRUE
    )
  
  #store ... expressions for filter() or transmute()
  if (!is.null(willfilter)) {
    exprs <- quos(...)
    if (length(exprs)==0) stop("willfilter is not NULL but you didn't include any expressions to pass to filter() or transmute()")
  }
  
  
  
  
  
  #initialize results table
  
  #if wanting weighted percentage
  if (pct) {
    
    prevtab <- des %>%
      group_by_at(c(by_vars, "item", "resp")) %>%
      summarize(prop = survey_mean(na.rm=TRUE)) %>% 
      mutate(prop = prop*100)
    
    if (include_SE) {
       prevtab <- prevtab %>% 
        mutate(prop_se = prop_se*100)
       
    } else {
      prevtab <- prevtab %>% 
        dplyr::select(-prop_se)
    }
    
  } else { #if wanting weighted N
    
    prevtab <- des %>%
      group_by_at(c(by_vars, "item", "resp")) %>%
      summarize(total = survey_total(na.rm=TRUE))
    
    if (!include_SE) {
      prevtab <- prevtab %>% 
        dplyr::select(-total_se)
    }
    
    
  }
  
  
  #filter, if willfilter==TRUE
  if (!is.null(willfilter) & isTRUE(willfilter)) prevtab <- prevtab %>% filter(!!!exprs)
  
  #add totals, if applicable
  if (add_totals) {
    prevtab <- prevtab %>% 
      group_by_at(c(by_vars, "item")) %>% 
      nest() %>% 
      mutate(data = purrr::map(data, function(df) {
        df %>% 
          add_row(resp := "Total", prop = sum(df$prop, na.rm = TRUE))
      })) %>% 
      unnest()
    
  }
  
  
  #spread 
  if (!is.null(spread_key)) {
    prevtab <- prevtab %>%
      tidyr::spread(key = !!rlang::sym(spread_key), value = !!rlang::sym(spread_value))
  }
  
  #arrange
  if (!is.null(arrange_vars)) {
    prevtab <- prevtab %>%
      arrange_at(arrange_vars)
  }
  
  
  #transmute, if willfilter==FALSE (collapse response options) -  (if spread_key is disability_cat, then can't use transmute here to collapse response options)
  if (!is.null(willfilter) & !isTRUE(willfilter)) prevtab <- prevtab %>% transmute(!!!exprs)
  
  
  return(prevtab)
  
}

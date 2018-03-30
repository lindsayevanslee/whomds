##########################################################################################
#                           FUNCTION TO OBTAIN WEIGHTED RESPONSE PERCENTAGES                  #
# Description                                                                            #
# . Function: PrevFunNew                                                          #
# . Arguments:                                                                           #
#    . df - the data frame with all the variables of interest
#    . vars_ids - variable names of the survey cluster ids
#    . vars_strata - variable names of the survey strata
#    . vars_weights - variable names of the weights
#    . formula_vars - vector of the column names of variables you would like to print results for
#    . formula_vars_levels - numeric vector of the factor levels of the variables in formula_vars. By default, the function assumes the variables have two levels: 0 and 1
#    . by_vars - the variables to disaggregate by
#    . spread_key - the variable to spread the table horizontally by. Keep as the default NULL to not spread the table horizontally.
#    . spread_value - the variable to fill the table with after a horizontal spread. By default this argument is "prop", which is a value created interally by the function, and generally does not need to be changed
#    . arrange_vars - the list of variables to arrange the table by
#    . willfilter - a variable that tells the function whether or not to filter the data by a particular value. 
#      . For example, if your formula_vars have response options of 0 and 1 but you only want to show the values for 1, then you would say willfilter = TRUE. Then at the end of your argument list you write an expression for the filter. In this case, you would say resp==1. 
#      . If you set willfilter = FALSE, then the function will assume you want to "transmute" the data, in other words collapse response options. For example, if your formula_vars have 5 response options, but you only want to show results for the sum of options "Agree" and "StronglyAgree", you could set willfilter=FALSE, and then directly after write the expression for the transmution, giving it a new column name--in this case the expression would be NewColName=Agree+AgreeStrongly.
#      . If you leave willfilter as its default of NULL, then the function will not filter or transmute data.
#    . ... - additional expressions to pass to the function to either filter or transmute the table by, depending on the value of willfilter
# . Output: table with weighted percent of responses, disaggregated by values given                        #
##########################################################################################

#LOAD dplyr THEN srvyr

PrevFunNew <- function(df, vars_ids, vars_strata, vars_weights, 
                       formula_vars, formula_vars_levels = 0:1, by_vars, 
                       spread_key = NULL, spread_value = "prop",
                       arrange_vars = NULL, 
                       willfilter = NULL, ...
                       ) {
  
  #convert data to long format using variables from formula_vars
  df <- df %>%
    gather_(
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

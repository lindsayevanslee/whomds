#' Compute unweighted percent and N for multiple variables, disaggregated
#'
#' @param df a tibble of individual data where each row is an individual
#' @param vars_demo a character vector of names of variables to calculate percent and N for
#' @param group_by_var a string (length 1) with the name of the variable from \code{df} to disaggregate by
#' @param spread_by_group_by_var logical determining whether to pass \code{group_by_var} to \code{dplyr::spread()} to give a wide-format tab. Default is FALSE.
#'
#' @return A tibble with percent and N for each level of each variable in \code{vars_demo}
#' @export
#'
#' @examples
table_unweightedpctn <- function(df, vars_demo, group_by_var=NULL, spread_by_group_by_var = FALSE) {
  
  #create sym of group_by_var if applicable
  if (!is.null(group_by_var)) {
    sym_group_by_var <- rlang::sym(group_by_var)
    
    #initialize final table
    final_tab <- tibble(demo=character(),
                        !!sym_group_by_var := character(),
                        n = numeric(),
                        pct = numeric())
  } else {
    final_tab <- tibble(demo=character(),
                        n = numeric(),
                        pct = numeric())
  }
  
  
  #for each vars_demo...
  for (i in seq_along(vars_demo)){
    
    #create sym of vars_demo at this iteration
    sym_this_vars_demo <- rlang::sym(vars_demo[i])
    
    #remove NAs of this vars_demo
    tab <- df %>% 
      filter(!is.na(!!sym_this_vars_demo)) 
    
    #if applicable, remove NAs of group_by_var
    if (!is.null(group_by_var)) {
      tab <- tab %>%
        filter(!is.na(!!sym_group_by_var))
    }
    
    #create summary table
    tab <- tab %>% 
      group_by_at(c(vars_demo[i], group_by_var)) %>%
      summarize(n=n()) %>% 
      mutate(pct=round(n/sum(n)*100,1)) %>% 
      rename(demo = !!sym_this_vars_demo)
    
    final_tab <- bind_rows(final_tab, tab)
    
  }
  
  if (!is.null(group_by_var)) {
    final_tab <- final_tab %>% 
      mutate(demo = ordered(demo, levels = unique(demo)), 
             !!sym_group_by_var := ordered(!!sym_group_by_var, levels=unique(!!sym_group_by_var))) %>% 
      transmute(demo,
                !!sym_group_by_var,
                pct_n = paste0(pct, " (", n, ")")) 
    
    if (spread_by_group_by_var) final_tab <- final_tab %>% 
        spread(!!sym_group_by_var, pct_n)
  } else {
    final_tab <- final_tab %>% 
      mutate(demo = ordered(demo, levels = unique(demo))) %>% 
      transmute(demo,
                pct_n = paste0(pct, " (", n, ")"))
  }
  

  
  return(final_tab)
}



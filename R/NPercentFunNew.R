#only for disaggregating by one variable for now (group_by_var)
NPercentFunNew <- function(df, vars_demo, group_by_var=NULL, spread_by_group_by_var = FALSE) {
  
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



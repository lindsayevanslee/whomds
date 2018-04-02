rasch_testlet <- function(df, vars_metric, testlet_strategy, max_values, resp_opts) {
  
  n_testlets <- length(testlet_strategy)
  
  #for each testlet
  for (i in 1:n_testlets) {
    
    #capture variables for new testlet
    new_testlet_vars <- testlet_strategy[[i]]
    new_testlet_name <- names(testlet_strategy)[i]
    
    if (!all(new_testlet_vars %in% vars_metric)) stop("You input a string that is not included in the variable list.")
    
    #create testlet
    if (!is.null(new_testlet_name) & !identical(new_testlet_name,"")) {
      new_testlet <- new_testlet_name
    } else {
      new_testlet <- paste(new_testlet_vars,collapse="_")
      names(testlet_strategy)[i] <- new_testlet
    }
    
    df <- df %>% 
      mutate(!!sym(new_testlet) := rowSums(df[,new_testlet_vars],na.rm=TRUE))
    
    #edit list of variables
    vars_metric <- vars_metric[-which(vars_metric %in% new_testlet_vars)]
    vars_metric <- c(vars_metric,new_testlet)
    
    #edit data.frame of maximum possible values
    max_values <- max_values %>% filter(!(var %in% new_testlet_vars))
    max_values <- max_values %>% bind_rows(c(var = new_testlet,
                                             max_val = (max(resp_opts)-1)*length(new_testlet_vars)))
  }
  
  
  testlet_result <- list(df = df,
                         vars_metric = vars_metric,
                         testlet_strategy = testlet_strategy,
                         max_values = max_values)
  
  return(testlet_result)
  
  
  
}
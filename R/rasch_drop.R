rasch_drop <- function(df, vars_metric, drop_vars, max_values) {
  

  if (!all(drop_vars %in% vars_metric)) stop("You input a string that is not included in the variable list.")

  #edit list of variables
  vars_metric <- vars_metric[-which(vars_metric %in% drop_vars)]
  
  #edit data.frame of maximum possible values
  max_values <- max_values %>% 
    filter(!(var %in% drop_vars))
 
  drop_result <- list(df = df,
                      vars_metric = vars_metric,
                      max_values = max_values)
  
  return(drop_result)
  
  
}
rasch_split <- function(df, vars_metric, split_strategy, max_values) {
  
  
  #How many different kinds of recode?
  n_split <- length(split_strategy)
  
  
  for (i in 1:n_split) {
    
    #capture new variable to split by
    new_split_by <- names(split_strategy)[i]
    
    if (!(new_split_by %in% names(df))) stop("You input a string that is not included in the variable list.")
    
    #categories of split variable
    split_cats <- df %>% pull(new_split_by) %>% unique()
    
    
    #capture variables to split
    new_vars_to_split <- split_strategy[[i]]
    
    if (!all(new_vars_to_split %in% vars_metric)) stop("You input a string that is not included in the variable list.")
    
    
    #create split
    for (ct in split_cats) {
      
      for (var in new_vars_to_split) {
        
        df <- df %>% 
          mutate(!!sym(paste(var,ct, sep="_")) := case_when(
            !!sym(new_split_by)==ct ~ !!sym(var)
          ))
        
      }
      
    }
    
    
    #edit list of variables
    new_split_vars <- paste(rep(new_vars_to_split,each=length(split_cats)),
                            split_cats,sep="_")
    
    vars_metric <- vars_metric[-which(vars_metric %in% new_vars_to_split)]
    vars_metric <- c(vars_metric, new_split_vars)
    
    #edit max_values
    new_max_values <- max_values %>% 
      slice(rep(which(var %in% new_vars_to_split),
                each = length(split_cats))) %>% 
      mutate(var = paste(var, split_cats, sep="_"))
    
    max_values <- max_values %>% 
      filter(!(var %in% new_vars_to_split)) %>% 
      bind_rows(new_max_values)
    
  }
  
  #save results
  split_result <- list(df = df,
                       vars_metric = vars_metric,
                       max_values = max_values)
  
  return(split_result)
  
  
}
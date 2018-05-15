#' Split survey items by categories for a Rasch Model
#'
#' @param df a data frame of individual survey data, where each row is an individual 
#' @param vars_metric a character vector of items to use in the Rasch Analysis
#' @param split_strategy a named list giving the strategy to take for spliting variables by categories, passed to \code{rasch_split()}. One element of the list per variable to split by. Each element of the list must be a character vector of column names to split. The names of the list are the variables to split each group of variables by.
#' @param max_values a tibble with two columns, \code{var} equivalent to \code{vars_metric} and \code{max_val} with their corresponding maximum possible values
#'
#' @details If significant differential item functioning (DIF) is observed, it may be desirable to split variables based on the charactersitic for which DIF is observed. For example, if men and women have significantly different patterns of responses to items, then it may be desirable to split items by sex. This function performs that variable splitting.
#'
#' @return a named list with the new \code{df}, new \code{vars_metric} and new \code{max_values} after splitting the desired variables
#' @export
#' 
#' @import dplyr
#' @import rlang
rasch_split <- function(df, vars_metric, split_strategy, max_values) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
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
                       max_values = max_values,
                       split_strategy = split_strategy)
  
  return(split_result)
  
  
}
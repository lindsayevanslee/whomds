#' Split survey items by categories for a Rasch Model
#'
#'
#' @inheritParams rasch_mds
#' @inheritParams rasch_testlet
#' 
#' @details If significant differential item functioning (DIF) is observed, it may be desirable to split variables based on the characteristic for which DIF is observed. For example, if men and women have significantly different patterns of responses to items, then it may be desirable to split items by sex. This function performs that variable splitting.
#'
#' @return a named list with:
#' \item{df}{new \code{df} after splitting the desired variables}
#' \item{vars_metric}{new \code{vars_metric} after splitting the desired variables}
#' \item{max_values}{new \code{max_values} after splitting the desired variables}
#' @export
#' 
#' @family rasch functions
#' @family children analysis functions
#' 
#' @import dplyr
#' @import rlang
rasch_split <- function(df, vars_metric, split_strategy, max_values) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #How many different kinds of splitting?
  n_split <- length(split_strategy)
  
  
  for (i in 1:n_split) {
    
    #capture new variable to split by
    new_split_by <- names(split_strategy)[i]
    
    if (!(new_split_by %in% names(df))) stop("You input a string to split by that is not included in the variable list.")
    
    #categories of split variable
    split_cats <- df %>% pull(new_split_by) %>% unique()
    
    
    #capture variables to split
    new_vars_to_split <- split_strategy[[i]]
    
    if (!all(new_vars_to_split %in% helper_varslist(vars_metric))) stop("You input a string that is not included in the variable list.")
    
    
    #create split
    for (ct in split_cats) {
      
      for (var in new_vars_to_split) {
        
        df <- df %>% 
          mutate(!!rlang::sym(paste(var,ct, sep="_")) := case_when(
            !!rlang::sym(new_split_by)==ct ~ !!rlang::sym(var)
          ))
        
      }
      
    }
    
    
    #edit list of variables
    if (is.list(vars_metric)) {
      vars_metric <- purrr::map(vars_metric, function(vset) {
        test <- vset %in% new_vars_to_split
        
        if (any(test)) {
          
          new_split_vars <- paste(rep(vset[which(test)], 
                                      each = length(split_cats)),
                                  split_cats, sep = "_")
          
          new_vset <- vset[-which(test)]
          new_vset <- c(new_vset, new_split_vars)
        } else new_vset <- vset
        
        return(new_vset)
        
      })} else {
        new_split_vars <- paste(rep(new_vars_to_split,each=length(split_cats)),
                                split_cats,sep="_")
        vars_metric <- vars_metric[-which(vars_metric %in% new_vars_to_split)]
        vars_metric <- c(vars_metric, new_split_vars)
      }
    

    
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
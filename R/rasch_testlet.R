#' Create testlets of survey items for a Rasch Model
#'
#' @param df a data frame of individual survey data, where each row is an individual 
#' @param vars_metric a character vector of items to use in the Rasch Analysis
#' @param testlet_strategy a list giving the strategy to take for creating testlets, passed to \code{rasch_testlet()}. One element of the list per testlet to create. Each element of the list must be a character vector of column names to use for the testlet. Optionally, name the element of the list to give the name of the new testlet. Otherwise, the new testlet will be the original column names separated by "_".
#' @param max_values a tibble with two columns, \code{var} equivalent to \code{vars_metric} and \code{max_val} with their corresponding maximum possible values
#' @param resp_opts a numeric vector of possible response options for \code{vars_metric}.
#'
#' @details If high local item dependence is observed (i.e., residual correlation) is observed between items, it may be desirable to combine them into a testlet. This code creates the testlets as desired.
#'
#' @return a named list with the new \code{df}, new \code{vars_metric}, new \code{testlet_strategy} and new \code{max_values} after creating desired testlets
#' @export
#' 
#' @import dplyr
rasch_testlet <- function(df, vars_metric, testlet_strategy, max_values, resp_opts) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
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
    max_values <- max_values %>% bind_rows(tibble(var = new_testlet,
                                                  max_val = (max(resp_opts)-1)*length(new_testlet_vars)))
  }
  
  
  testlet_result <- list(df = df,
                         vars_metric = vars_metric,
                         testlet_strategy = testlet_strategy,
                         max_values = max_values)
  
  return(testlet_result)
  
  
  
}
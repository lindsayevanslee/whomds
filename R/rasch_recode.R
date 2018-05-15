#' Recode survey items for use in Rasch Analysis
#'
#' @param df a tibble of individual survey data, where each row is an individual 
#' @param vars_metric a character vector of items to use in the Rasch Analysis
#' @param recode_strategy a named list giving the strategy to take for recoding variables, passed to \code{rasch_recode()}. One element of the list per recode strategy. Each element of the list is a numeric vector giving the new values to map the variables to. The names of the list are the groups of column names to use for each recoding strategy, separated only by ",".
#' @param max_values a tibble with two columns, \code{var} equivalent to \code{vars_metric} and \code{max_val} with their corresponding maximum possible values
#'
#' @return a named list with the new \code{df} and new \code{max_values} after recoding the desired variables
#' @export
#' 
#' @import dplyr
rasch_recode <- function(df, vars_metric, recode_strategy, max_values) {
  
  #How many different kinds of recode?
  n_recode <- length(recode_strategy)
  
  
  #What is the recode outcome? 
  for (i in 1:n_recode) {
    
    #capture desired recode outcome and variables to recode
    new_outcome <- recode_strategy[[i]]
    
    new_recoded <- names(recode_strategy)[i]
    new_recoded <- unlist(strsplit(new_recoded,","))
    if (!all(new_recoded %in% vars_metric)) stop("You input a string that is not included in the variable list.")
    
    #capture range of maximum values for each recoded variable and test if max values are equal
    resp_opts_range <- max_values %>% 
      filter(var %in% new_recoded) %>% 
      pull("max_val") %>% 
      as.numeric() %>% 
      range()
    if (!isTRUE(all.equal(resp_opts_range[1],resp_opts_range[2])))  stop("You input variables with different possible response options.")
    if (length(new_outcome)!=unique(resp_opts_range)+1) stop("Outcome string must have same # of characters as possible response options.")

    #edit data.frame of maximum possible values
    max_values <- max_values %>% 
      mutate(max_val = ifelse(var %in% new_recoded, utils::tail(new_outcome,1), max_val))
    
  
    #perform the recode
    df <- df %>% 
      mutate_at(vars(new_recoded), 
                funs(plyr::mapvalues, .args = list(from = 0:unique(resp_opts_range), to = new_outcome, warn_missing = FALSE)))

  }
  
  recode_result <- list(df = df,
                        max_values = max_values)
  
  return(recode_result)  
  
}
#' Create testlets of survey items for a Rasch Model
#'
#' @param max_values a tibble with two columns, \code{var} equivalent to \code{vars_metric} and \code{max_val} with their corresponding maximum possible values
#' @inheritParams rasch_mds
#'
#' @details If high local item dependence is observed (i.e., residual correlation) is observed between items, it may be desirable to combine them into a testlet. This code creates the testlets as desired.
#'
#' @return a named list with:
#' \item{df}{new \code{df} after creating desired testlets}
#' \item{vars_metric}{new \code{vars_metric} after creating desired testlets}
#' \item{testlet_strategy}{new \code{testlet_strategy} after creating desired testlets}
#' \item{max_values}{new \code{max_values} after creating desired testlets}
#' 
#' @export
#' 
#' @family rasch functions
#' @family children analysis functions
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
    
    if (!all(new_testlet_vars %in% helper_varslist(vars_metric))) stop("You input a string that is not included in the variable list.")
    
    #create testlet
    if (!is.null(new_testlet_name) & !identical(new_testlet_name,"") & !identical(new_testlet_name,NA_character_)) {
      new_testlet <- new_testlet_name
    } else {
      new_testlet <- paste(new_testlet_vars,collapse="_")
      names(testlet_strategy)[i] <- new_testlet
    }
    
    df <- df %>% 
      mutate(!!rlang::sym(new_testlet) := rowSums(df[,new_testlet_vars],na.rm=TRUE))
    
    #edit list of variables
    if (is.list(vars_metric)) {
      vars_metric <- purrr::map(vars_metric, function(vset) {
        
        if (all(new_testlet_vars %in% vset)) {
          new_vset <-  vset[-which(vset %in% new_testlet_vars)]
          new_vset <- c(new_vset, new_testlet)
        } else {
          new_vset <- vset
        }
        
        return(new_vset)
        
      })
    } else {
      vars_metric <- vars_metric[-which(vars_metric %in% new_testlet_vars)]
      vars_metric <- c(vars_metric,new_testlet)
    }
    
    
    
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
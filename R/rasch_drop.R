#' Drop items from a Rasch Analysis
#'
#' @inheritParams rasch_mds
#' @inheritParams rasch_testlet
#'
#' @return a named list with:
#' \item{vars_metric}{new \code{vars_metric} after dropping the desired variables}
#' \item{max_values}{new \code{max_values} after dropping the desired variables}
#' 
#' @details Dropping variables might be desirable if one finds that particular items are causing a lot of problems for the fit of a Rasch Model.
#' 
#' @family rasch functions
#' @family children analysis functions
#' 
#' @export
rasch_drop <- function(vars_metric, drop_vars, max_values) {
  

  if (!all(drop_vars %in% helper_varslist(vars_metric))) stop("You input a string that is not included in the variable list.")

  #edit list of variables
  if (is.list(vars_metric)) {
    vars_metric <- purrr::map(vars_metric, function(vset) {
      
      if (any(drop_vars %in% vset)) {
        new_vset <-  vset[-which(vset %in% drop_vars)]
      } else {
        new_vset <- vset
      }
      
      return(new_vset)
      
    })
  } else {
    vars_metric <- vars_metric[-which(vars_metric %in% drop_vars)]
  }
  
  #edit data.frame of maximum possible values
  max_values <- max_values %>% 
    filter(!(var %in% drop_vars))
 
  drop_result <- list(vars_metric = vars_metric,
                      max_values = max_values)
  
  return(drop_result)
  
  
}
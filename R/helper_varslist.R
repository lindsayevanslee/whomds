#' Convert list to vector with unique elements
#'
#' @param vars_list a list of character vectors
#'
#' @return a character vector of all unique items from \code{vars_list}
#' 
#' @family helper functions
#'
helper_varslist <- function(vars_list) {
  vars_vec <- vars_list %>% 
    unlist() %>% 
    unique() %>% 
    sort()
  
  return(vars_vec)
}
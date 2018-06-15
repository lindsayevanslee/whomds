#' Convert list to vector with unique elements
#'
#' @param vars_list a list of character vectors
#'
#' @return a character vector of all unique items from \code{vars_list}
#' 
#' @family helper functions
#'
#' @examples my_list <- list(letters[1:3], letters[3:6], letters[4:10])
#' helper_varslist(my_list)
helper_varslist <- function(vars_list) {
  vars_vec <- vars_list %>% 
    unlist() %>% 
    unique() %>% 
    sort()
  
  return(vars_vec)
}
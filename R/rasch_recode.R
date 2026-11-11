#' Recode survey items for use in Rasch Analysis
#'
#' @inheritParams rasch_mds
#' @inheritParams rasch_testlet
#' 
#' @return a named list with:
#' \item{df}{new \code{df} after recoding the desired variables}
#' \item{max_values}{new \code{max_values} after recoding the desired variables}
#' 
#' @family rasch functions
#' @family children analysis functions
#' 
#' @example inst/examples/prep_adults.R
#' @examples
#'
#' # Collapse the response options of EF1 and EF2 so that the two highest
#' # options (3 and 4 after the shift) become one, and those of EF3 from five
#' # options to three
#' recoded <- rasch_recode(df, items,
#'   recode_strategy = list("EF1,EF2" = c(0, 1, 2, 3, 3), "EF3" = c(0, 1, 1, 2, 2)),
#'   max_values = max_values)
#'
#' # compare the original and recoded responses to EF1
#' table(original = df$EF1, recoded = recoded$df$EF1)
#' # the highest possible values of the recoded items are updated
#' recoded$max_values
#' @export
#' 
#' @import dplyr
rasch_recode <- function(df, vars_metric, recode_strategy, max_values) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #How many different kinds of recode?
  n_recode <- length(recode_strategy)
  
  
  #What is the recode outcome? 
  for (i in 1:n_recode) {
    
    #capture desired recode outcome and variables to recode
    new_outcome <- recode_strategy[[i]]
    
    new_recoded <- names(recode_strategy)[i]
    new_recoded <- unlist(strsplit(new_recoded,","))
    if (!all(new_recoded %in% helper_varslist(vars_metric))) stop("You input a string that is not included in the variable list.")
    
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
      mutate_at(vars(all_of(new_recoded)), 
                list(~ helper_mapvalues(., from = 0:unique(resp_opts_range), 
                                       to = new_outcome, 
                                       warn_missing = FALSE)))

  }
  
  recode_result <- list(df = df,
                        max_values = max_values)
  
  return(recode_result)  
  
}

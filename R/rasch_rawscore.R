#' Add the raw scores to the data and artificial individuals attaining the minimum and/or maximum
#'
#' @inheritParams rasch_mds
#' @inheritParams rasch_testlet
#'
#' @return a tibble with a new column \code{RawScore} with the raw sum score of \code{vars_metric} for each individual, and artificial rows with individuals that attain the minimum and/or maximum if either is not attained in \code{df}. The artificial maximum row has value "MAX" in the \code{vars_id} column, and likewise the artificial minimum row has the value "MIN" in this column.
#'
#' @family rasch functions
#' 
#' @export
#'
rasch_rawscore <- function(df, vars_metric, vars_id, max_values) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  df <- df %>% 
    mutate(RawScore = rowSums(df %>% select(vars_metric), na.rm=TRUE))
  
  max_value <- max_values %>% pull(max_val) %>% sum
  
  #if max value not attained, add row with artificial maximum
  if (!any(pull(df, RawScore)==max_value, na.rm=TRUE)) {
    
    df_max <- t(max_values) %>% 
      as_tibble() %>% 
      rename_all(list(~ pull(max_values, var))) %>% 
      slice(2) %>% 
      mutate_all(list(~ as.numeric(.))) %>% 
      mutate(!!rlang::sym(vars_id) := "MAX",
             RawScore = max_value)
    
    df <- df %>% full_join(df_max)
  }
  
  #if min value not attained, add row with artificial minimum
  if (!any(pull(df, RawScore)==0, na.rm=TRUE)){
    
    
    df_min <- matrix(0,1,length(vars_metric), 
                     dimnames = list(NA,vars_metric)) %>% 
      as_tibble() %>% 
      mutate(!!rlang::sym(vars_id) := "MIN",
             RawScore = 0)
    
    df <- df %>% full_join(df_min)
    
  }
  
  
  return(df)
  
  
}
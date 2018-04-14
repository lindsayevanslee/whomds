rasch_rawscore <- function(df, vars_metric, vars_id, max_values) {
  
  df <- df %>% 
    mutate(RawScore = rowSums(df %>% select(vars_metric), na.rm=TRUE))
  
  max_value <- max_values %>% pull(max_val) %>% sum
  
  #if max value not attained, add row with artificial maximum
  if (!any(pull(df, RawScore)==max_value, na.rm=TRUE)) {
    
    df_max <- t(max_values) %>% 
      as_data_frame() %>% 
      rename_all(funs(pull(max_values,var))) %>% 
      slice(2) %>% 
      mutate_all(funs(as.numeric)) %>% 
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
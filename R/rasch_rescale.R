rasch_rescale <- function(df, df_score, vars_id) {
  
  df_final <- df %>% 
    left_join(df_score) %>% 
    mutate(rescaled = scales::rescale(person_pars, c(0,100))) %>% 
    filter_at(vars(vars_id), any_vars(. != "MAX")) %>% 
    filter_at(vars(vars_id), any_vars(. !="MIN"))
  
  return(df_final)
  
  
  
}
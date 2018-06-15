#' Rescale score from Rasch Analysis for children to range from 0 to 100
#' 
#' @inheritParams rasch_mds
#' @inheritParams rasch_mds_children
#' @inheritParams rasch_model_children
#'
#' @return a tibble with the data \code{df} or unnested \code{df_nest} and new columns "Metric" and "MetricRescaled" with the original and rescaled person abilities, ranging from 0 to 100, and filter out any rows with an artificial minimum or maximum
#'
#' @family rasch functions
#' @family children analysis functions
#'
#' @export
#'
#'
rasch_rescale_children <- function(df, df_nest, vars_age_group, vars_id) {
  
  
  #Anchored
  if ("WLE_anchored" %in% names(df_nest)) {
    df_final <- df_nest %>% 
      mutate(df_split = map2(df_split, WLE_anchored, function(df_age, WLE_age) {
        df_age <- df_age %>% 
          add_column(Metric = WLE_age$theta)
        return(df_age)
      })) %>% 
      select(c(vars_age_group, "df_split")) %>% 
      unnest()
  }
  #Multigroup
  else {
    # browser()
    df_final <- df %>% 
      add_column(Metric = df_nest$WLE_multigroup[[1]]$theta)
    
  }
  
   df_final <- df_final  %>% 
    mutate(MetricRescaled = scales::rescale(pull(., Metric), c(0, 100))) %>% 
    filter_at(vars(vars_id), any_vars(. != "MAX")) %>% 
    filter_at(vars(vars_id), any_vars(. !="MIN"))
  
  return(df_final)
  
}

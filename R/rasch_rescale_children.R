#' Rescale score from Rasch Analysis for children to range from 0 to 100
#'
#' @param df_nest a data frame of individual survey data, where each row is an individual 
#' @param vars_age_group
#' @param vars_id a string with column name uniquely identifying individuals
#'
#' @return a tibble with the full join between \code{df} and \code{df_score} and new column "rescaled" with the rescaled person abilities, ranging from 0 to 100, and filter out any rows with an artificial minimum or maximum
#'
#' @family rasch functions
#'
#' @export
#'
#'
rasch_rescale_children <- function(df_nest, vars_age_group, vars_id) {

  df_final <- df_nest %>% 
    mutate(df_split = map2(df_split, WLE_anchored, function(df_age, WLE_age) {
      df_age <- df_age %>% 
        add_column(Metric = WLE_age$theta)
      return(df_age)
    })) %>% 
    select(c(vars_age_group, df_split)) %>% 
    unnest() %>% 
    mutate(MetricRescaled = scales::rescale(pull(., Metric), c(0, 100))) %>% 
    filter_at(vars(vars_id), any_vars(. != "MAX")) %>% 
    filter_at(vars(vars_id), any_vars(. !="MIN"))
  
  return(df_final)
  
  
  
}
#' Rescale score from Rasch Analysis to range from 0 to 100
#'
#' @param df_score a tibble resulting from \code{rasch_model()} with the person abilities from the Rasch Model
#' @inheritParams rasch_mds
#'
#' @return a tibble with the left join between \code{df} and \code{df_score} and new column "rescaled" with the rescaled person abilities, ranging from 0 to 100, and filter out any rows with an artificial minimum or maximum
#'
#' @family rasch functions
#'
#' @export
#'
#'
rasch_rescale <- function(df, df_score, vars_id) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  df_final <- df %>% 
    left_join(df_score) %>% 
    mutate(rescaled = scales::rescale(person_pars, c(0,100))) %>% 
    filter_at(vars(vars_id), any_vars(. != "MAX")) %>% 
    filter_at(vars(vars_id), any_vars(. !="MIN"))
  
  return(df_final)
  
  
  
}
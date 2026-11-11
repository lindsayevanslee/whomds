#' Rescale score from Rasch Analysis to range from 0 to 100
#'
#' @param df_score a tibble resulting from \code{rasch_model()} with the person abilities from the Rasch Model
#' @inheritParams rasch_mds
#'
#' @return a tibble with the left join between \code{df} and \code{df_score} (which carries \code{person_pars} and \code{person_pars_se}) and new column \code{rescaled} with the rescaled person abilities, ranging from 0 to 100, with any rows with an artificial minimum or maximum filtered out
#'
#' @family rasch functions
#'
#' @example inst/examples/prep_adults.R
#' @example inst/examples/prep_adults_model.R
#' @examples
#'
#' model <- rasch_model(df, items, vars_id = "HHID")
#'
#' df_final <- rasch_rescale(df, model$df_score, vars_id = "HHID")
#'
#' # the person abilities are on a logit scale; "rescaled" puts them on the
#' # 0 to 100 disability score used in WHO reports
#' head(df_final[c("HHID", "person_pars", "person_pars_se", "rescaled")])
#' range(df_final$rescaled)
#' @export
#'
#'
rasch_rescale <- function(df, df_score, vars_id) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  df_final <- df %>% 
    left_join(df_score) %>% 
    mutate(rescaled = helper_rescale(person_pars, c(0, 100))) %>% 
    filter_at(vars(all_of(vars_id)), any_vars(. != "MAX")) %>% 
    filter_at(vars(all_of(vars_id)), any_vars(. != "MIN"))
  
  return(df_final)
  
  
  
}
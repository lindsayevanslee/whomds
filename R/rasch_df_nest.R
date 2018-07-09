#' Nest data by age group
#'
#' @inheritParams rasch_mds_children
#'
#' @return nested tibble with two columns: the age group given by \code{vars_group} and \code{df_split} with the split data
#' @export
#' 
#' @family rasch functions
#' @family children analysis functions
#'
rasch_df_nest <- function(df, vars_group, vars_id) {
  
  #remove people with NA for age group 
  df_nest <- df %>% 
    filter(!is.na(!!rlang::sym(vars_group)))
  
  #split data by age group and add back the max and min rows row
  df_nest <- df_nest %>% 
    group_by_at(vars_group) %>% 
    tidyr::nest(.key = "df_split") %>% 
    mutate(df_split = purrr::map(df_split, function(df_age) {
      df_age <- bind_rows(df_age,
                          df %>% filter(!!rlang::sym(vars_id) %in% c("MAX","MIN")))
    }))
  
  return(df_nest)
  
}
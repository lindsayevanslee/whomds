#' Nest data by age group
#'
#' @inheritParams rasch_mds_children
#'
#' @return nested tibble with two columns: the age group given by \code{vars_group} and \code{df_split} with the split data
#' @example inst/examples/prep_children.R
#' @examples
#'
#' split <- rasch_split_age(df, "age_cat", vars_metric, "HHID", max_values)
#'
#' # one row per age group, with that group's data in the column df_split
#' df_nest <- rasch_df_nest(split$df, vars_group = "age_cat", vars_id = "HHID")
#' df_nest
#' head(df_nest$df_split[[1]][1:6])
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
    tidyr::nest() %>% 
    rename(df_split = data) %>% 
    mutate(df_split = purrr::map(df_split, function(df_age) {
      df_age <- bind_rows(df_age,
                          df %>% filter(!!rlang::sym(vars_id) %in% c("MAX","MIN")))
    }))
  
  return(df_nest)
  
}
#' Rescale score from Rasch Analysis for children to range from 0 to 100
#' 
#' @inheritParams rasch_mds
#' @inheritParams rasch_mds_children
#' @inheritParams rasch_model_children
#'
#' @return a tibble with the data \code{df} or unnested \code{df_nest} and new columns "person_pars" and "rescaled" with the original and rescaled person abilities, ranging from 0 to 100, and filter out any rows with an artificial minimum or maximum
#'
#' @family rasch functions
#' @family children analysis functions
#'
#' @export
#'
rasch_rescale_children <- function(df, df_nest, vars_group, vars_id) {
  
  
  #Anchored
  if ("WLE_anchored" %in% names(df_nest)) {
    df_final <- df_nest %>% 
      dplyr::mutate(df_split = purrr::map2(df_split, WLE_anchored, function(df_age, WLE_age) {
        df_age <- df_age %>% 
          tibble::add_column(person_pars = WLE_age$theta) %>% 
          dplyr::select(-!!rlang::sym(vars_group)) 
        return(df_age)
      })) %>% 
      dplyr::select(all_of(c(vars_group, "df_split"))) %>% 
      tidyr::unnest(cols = c(df_split))
  }
  #Multigroup
  else {
    df_final <- df %>% 
      tibble::add_column(person_pars = df_nest$WLE_multigroup[[1]]$theta)
    
  }
  
   df_final <- df_final  %>% 
    dplyr::mutate(rescaled = scales::rescale(person_pars, c(0, 100))) %>% 
    dplyr::filter_at(dplyr::vars(dplyr::all_of(vars_id)), dplyr::any_vars(. != "MAX")) %>% 
    dplyr::filter_at(dplyr::vars(dplyr::all_of(vars_id)), dplyr::any_vars(. !="MIN"))
  
  return(df_final)
  
}

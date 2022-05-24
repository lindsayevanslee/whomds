#' Run the multigroup and anchored Rasch Model 
#'
#' @param df_nest a nested tibble that contains the column \code{df_split} with the data split by the categories in the column \code{vars_group}
#' @inheritParams rasch_mds
#' @inheritParams rasch_mds_children
#'
#' @family rasch functions
#' @family children analysis functions
#'
#' @return a nested tibble with new columns with the Rasch Models calculated with the \code{TAM} package
#' @export
#'
rasch_model_children <- function(df, df_nest, vars_metric, vars_group, TAM_model) {
  
  #choose tam model function based on TAM_model
  if (TAM_model == "PCM2") {
    tam.f <- TAM::tam.mml
  } else { 
    tam.f <- TAM::tam.mml.2pl
  }
  
  #Calculate start models and store in list column called mod_start
  df_nest <- df_nest %>%
    dplyr::mutate(df_split_selected = purrr::map2(df_split, !!rlang::sym(vars_group), 
                                           ~ dplyr::select(..1, c(vars_metric[["common"]], vars_metric[[as.character(..2)]])))
    )
  
  #test if some variables only have one chosen response value for an age group
  if (df_nest$df_split_selected %>% 
    purrr::map(~ purrr::map(., table)) %>% 
    purrr::flatten() %>% 
    purrr::map(length) %>% 
    purrr::flatten_dbl() %>% 
    `==`(1) %>% 
    any()) warning("Some items have only one chosen response option for an age group, which can cause problems with TAM. Check the response frequencies.")
  
  
  df_nest <- df_nest %>%
    dplyr::mutate(
      mod_start = purrr::map(df_split_selected, 
                             .f = function(df_obj) {tam.f(resp = df_obj, irtmodel = TAM_model, verbose = FALSE)})
    )
  
  
  #Calculate multigroup model with only common items
  mod_multigroup <- df %>% 
    dplyr::select(vars_metric[["common"]]) %>% 
    tam.f(resp = .,
          irtmodel = TAM_model,
          group = pull(df, vars_group),
          beta.fixed = FALSE,
          verbose = FALSE)
  df_nest <- df_nest %>% 
    dplyr::mutate(mod_multigroup = list(mod_multigroup))
  
  #Calculate anchored model
  if (length(vars_metric) > 1) {
    df_nest <- df_nest %>%
      dplyr::mutate(start_xsi = purrr::map(mod_start, "xsi.fixed.estimated"),
             multigroup_xsi = purrr::map(mod_multigroup, "xsi.fixed.estimated"),
             index_uniqueitems = purrr::map2(start_xsi, !!rlang::sym(vars_group),
                                             ~ which(grepl(pattern = paste(vars_metric[[as.character(..2)]], collapse="|"), rownames(..1)))),
             anchored_xsi = purrr::pmap(list(multigroup_xsi, start_xsi, index_uniqueitems),
                                        ~ rbind(..1, ..2[..3,])),
             mod_anchored = purrr::map2(df_split_selected, anchored_xsi,
                                        ~ tam.f(resp = ..1,
                                                irtmodel = TAM_model,
                                                xsi.fixed = ..2,
                                                verbose = FALSE))

      )
    
  }
  
  
  return(df_nest)
  
} 


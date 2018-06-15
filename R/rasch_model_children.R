rasch_model_children <- function(df, df_nest, vars_metric, vars_age_group, TAM_model) {
  
  #choose tam model function based on TAM_model
  if (TAM_model == "PCM2") {
    tam.f <- tam.mml
  } else { 
    tam.f <- tam.mml.2pl
  }
  
  #Calculate start models and store in list column called mod_start
  df_nest <- df_nest %>%
    mutate(df_split_selected = map2(df_split, !!rlang::sym(vars_age_group), 
                                    ~ select(..1, c(vars_metric[["common"]], vars_metric[[as.character(..2)]]))),
           mod_start = map(df_split_selected, 
                           ~ tam.f(resp = ., irtmodel = TAM_model, verbose = FALSE))
    )
  
  
  #Calculate multigroup model with only common items
  mod_multigroup <- df %>% 
    select(vars_metric[["common"]]) %>% 
    tam.f(resp = .,
          irtmodel = TAM_model,
          group = pull(df, vars_age_group),
          beta.fixed = FALSE,
          verbose = FALSE)
  df_nest <- df_nest %>% 
    mutate(mod_multigroup = list(mod_multigroup))
  
  #Calculate anchored model
  df_nest <- df_nest %>% 
    mutate(start_xsi = map(mod_start, "xsi.fixed.estimated"),
           multigroup_xsi = map(mod_multigroup, "xsi.fixed.estimated"),
           index_uniqueitems = map2(start_xsi, !!rlang::sym(vars_age_group), 
                                    ~ which(grepl(pattern = paste(vars_metric[[as.character(..2)]], collapse="|"), rownames(..1)))),
           anchored_xsi = pmap(list(multigroup_xsi, start_xsi, index_uniqueitems), 
                               ~ rbind(..1, ..2[..3,])),
           mod_anchored = map2(df_split_selected, anchored_xsi,
                               ~ tam.f(resp = ..1, 
                                       irtmodel = TAM_model, 
                                       xsi.fixed = ..2,
                                       verbose = FALSE))
           
    )
  
  return(df_nest)
  
} 


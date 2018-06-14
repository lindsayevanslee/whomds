rasch_model_children <- function(df, df_nest, vars_metric, vars_age_group, TAM_model) {
  
  #choose tam model function based on TAM_model
  if (TAM_model == "PCM2") {
    tam.f <- tam.mml
  } else { 
    tam.f <- tam.mml.2pl
  }
  
  #Calculate start models and store in list column called mod_start
  df_nest <- df_nest %>%
    mutate(mod_start = tam.f(df_split %>% select(c(
      vars_metric[["common"]],
      vars_metric[[!!rlang::sym(vars_age_group)]]
    )),
    irtmodel = TAM_model,
    verbose = FALSE))
  
  #Calculate multigroup model with only common items
  mod_multigroup <-
    tam.f(resp = df %>% select(vars_metric[["common"]]),
          irtmodel = TAM_model,
          group = pull(df, vars_age_group),
          beta.fixed = FALSE,
          verbose = FALSE)
  df_nest <- df_nest %>% 
    mutate(mod_multigroup = mod_multigroup)
  
  #Calculate anchored model
  df_nest <- df_nest %>% 
    mutate(start_xsi = pull(mod_start, "xsi.fixed.estimated"),
           multigroup_xsi = pull(mod_multigroup, "xsi.fixed.estimated"),
           index_uniqueitems = which(grepl(pattern = paste(vars_metric[[!!rlang::sym(vars_age_group)]], collapse="|"),
                                           rownames(start_xsi))),
           anchored_xsi = rbind(multigroup_xsi,
                                start_xsi[index_uniqueitems,]),
           mod_anchored = tam.f(resp = df_split %>% select(c(
             vars_metric[["common"]],
             vars_metric[[!!rlang::sym(vars_age_group)]]
           )),
           irtmodel = TAM_model,
           xsi.fixed = anchored_xsi,
           verbose = FALSE))

  
  return(df_nest)
  
} 


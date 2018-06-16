library(tidyverse)

load("/Users/lindsaylee/Dropbox/WHO/MDS/09 national and regional surveys/Chile 2014/04 Syntax/DescriptiveAnalysis/chile.RData")

demos <- c("sex","edad","age_cat", "work_cat","edu_cat","marital_status")
scores <- c("capacity_cat","performance_cat","CapacityScore","PerformanceScorePredicted")
vars2keep <- c("enc_id",demos,scores,vars_ids,vars_strata,vars_weights,vars_capacity,vars_conditions1,vars_environ,vars_health,vars_performance)

chile_adults <- chile[,vars2keep] %>% as_tibble()

any(table(chile_adults$enc_id)>1)


chile_children <- data_total %>% 
  as_tibble() %>% 
  dplyr::filter(kishinfantil == 1) %>% 
  mutate(age_cat = cut(
    edad,
    breaks = c(2, 5, 10, 17),
    labels = c("Age2to4", "Age5to9", "Age10to17"),
    right = FALSE,
    include.lowest = TRUE,
    ordered_result = TRUE
  ),
  sex = factor(sexo, levels = 1:2, labels = c("Male", "Female"))) %>% 
  select(c("enc_id", "sex", "edad", "age_cat", "VARUNIT_N", "VARSTRAT_N", "Factor_Persona"), 
         matches("e\\d"), matches("n\\d"))
  



rm(list=setdiff(ls(), c("chile_adults", "chile_children")))

devtools::use_data(chile_adults, overwrite = TRUE)
devtools::use_data(chile_children, overwrite = TRUE)


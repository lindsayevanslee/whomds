library(tidyverse)
library(haven)

#costa rica
costa_rica <- haven::read_sav("/Users/lindsaylee/Dropbox/WHO/MDS/09 national and regional surveys/Costa Rica 2018/01 Data/ENADIS 2018 USUARIO.sav")

# usethis::use_data(costa_rica, overwrite = TRUE)


#chile
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
         matches("e\\d"), matches("n[1-9]"), matches("n[1-5][0-9]"), matches("n[6][0-3]"), -contains("especifique")) %>% 
  map(function(x) {
    attr(x, "value.labels") <- NULL
    return(x)
  }) %>% 
  as_tibble()


rm(list=setdiff(ls(), c("chile_adults", "chile_children")))

# usethis::use_data(chile_adults, overwrite = TRUE)
# usethis::use_data(chile_children, overwrite = TRUE)


#anonymized
set.seed(1992)
df_adults <- 
  #randomly generate demographic and survey variables
  tibble(HHID = 1:2500,
         strata = sample(c("A", "B", "C"), size = 2500, replace = TRUE),
         PSU = sample(1:20, size = 2500, replace = TRUE),
         weight = sample(seq(800, 1200, by = 50), size = 2500, replace = TRUE),
         sex = factor(sample(c("Male", "Female"), size = 2500, replace = TRUE)), 
         age = sample(18:100, size = 2500, prob = rbeta(83, 2, 5), replace = TRUE),
         age_cat = cut(
           age,
           breaks = c(18, 25, 40, 65, 100),
           labels = c("18-24", "25-39", "40-64", "64-100"),
           right = FALSE,
           include.lowest = TRUE,
           ordered_result = TRUE
         ),
         # work_cat = factor(sample(c("Y", "N"), size = 2500, prob = c(0.6, 0.4), replace = TRUE)),
         edu_cat = ordered(sample(c("None", "Elementary", "Secondary", "University"), 
                                  size = 2500, prob = c(0.2, 0.4, 0.25, 0.15), replace = TRUE),
                           levels = c("None", "Elementary", "Secondary", "University"))) %>%
  #combine with raw data from Chile, randomly selected, and work_cat variable
  bind_cols(
    chile_adults %>% 
      sample_n(2500) %>% 
      select(d1:d47, c2:c25, fa1:fa12
             , work_cat
             # , CapacityScore, capacity_cat, PerformanceScorePredicted, performance_cat
      ) %>% 
      rename_at(.vars = vars(d1:d47), .funs = list(~ str_replace_all(., "d", "F"))) %>% 
      rename_at(.vars = vars(c2:c25), .funs = list(~ str_replace_all(., "c", "C"))) %>% 
      rename_at(.vars = vars(fa1:fa12), .funs = list(~ str_replace_all(., "fa", "EF"))) 
  ) %>% 
  #combine with disability score calculated from example in guidebook
  left_join(
    read_csv("/Users/lindsaylee/Dropbox/WHO/MDS/32 Data Analyses Training at WHO/Guide book/ExampleRasch_EF/Testlet1_Recode4/Data_final.csv") %>% 
      select(HHID, rescaled) %>% 
      rename(disability_score = rescaled)
  ) %>% 
  #calculate disability_cat based on score
  mutate(disability_cat = case_when(
    disability_score == 0 ~ "No",
    disability_score < mean(disability_score, na.rm = TRUE) - sd(disability_score, na.rm = TRUE) ~ "No",
    disability_score < mean(disability_score, na.rm = TRUE) & disability_score >= mean(disability_score, na.rm = TRUE) - sd(disability_score, na.rm = TRUE) ~ "Mild",
    disability_score < mean(disability_score, na.rm = TRUE) + sd(disability_score, na.rm = TRUE) & disability_score >= mean(disability_score, na.rm = TRUE) ~ "Moderate",
    disability_score >= mean(disability_score, na.rm = TRUE) + sd(disability_score, na.rm = TRUE) ~ "Severe",
    TRUE ~ NA_character_
  )
  ) %>% 
  #fix remaining factors
  mutate(disability_cat = ordered(disability_cat, levels = c("No", "Mild", "Moderate", "Severe"))
         , work_cat = factor(work_cat, levels = 0:1, labels = c("N", "Y"))
         ) %>% 
  #put in the desired order
  select(HHID:edu_cat, work_cat, disability_score, disability_cat, everything())


df_children <- df_adults %>% 
  select(HHID:weight) %>% 
  mutate(sex = factor(sample(c("Male", "Female"), size = 2500, replace = TRUE))) %>% 
  bind_cols(
    chile_children %>% 
      sample_n(2500) %>% 
      select(edad, age_cat, n1:n35) %>% 
      rename_at(.vars = vars(n1:n35), .funs = list(~ str_replace_all(., "n", "child"))) %>% 
      rename(age = edad) %>% 
      mutate(age = case_when(
        age_cat == "Age2to4" ~ sample(2:4, 1),
        age_cat == "Age5to9" ~ sample(5:9, 1),
        age_cat == "Age10to17" ~ sample(10:17, 1)
      ),
      age_cat = ordered(age_cat, levels = c("Age2to4", "Age5to9", "Age10to17"))
      )
    
  ) %>% print()


usethis::use_data(df_adults, overwrite = TRUE)
usethis::use_data(df_children, overwrite = TRUE)


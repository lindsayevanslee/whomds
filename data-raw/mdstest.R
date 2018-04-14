library(tidyverse)
load("/Users/lindsaylee/Dropbox/WHO/MDS/09 national and regional surveys/Chile 2014/04 Syntax/DescriptiveAnalysis/chile.RData")

demos <- c("sex","edad","age_cat", "work_cat","edu_cat","marital_status")
scores <- c("capacity_cat","performance_cat","CapacityScore","PerformanceScorePredicted")
vars2keep <- c("enc_id",demos,scores,vars_ids,vars_strata,vars_weights,vars_capacity,vars_conditions1,vars_environ,vars_health,vars_performance)

mdstest <- chile[,vars2keep] %>% as_tibble()

any(table(mdstest$enc_id)>1)

rm(list=setdiff(ls(), c("mdstest")))

devtools::use_data(mdstest, overwrite = TRUE)

library(tidyverse)
load("/Users/lindsaylee/Dropbox/WHO/MDS/09 national and regional surveys/Chile 2014/04 Syntax/DescriptiveAnalysis/chile.RData")

demos <- c("sex","edad","age_cat", "work_cat","edu_cat","marital_status")
scores <- c("capacity_cat","performance_cat","CapacityScore","PerformanceScorePredicted")
vars2keep <- c(vars_indid,demos,scores,vars_ids,vars_strata,vars_weights,vars_capacity,vars_conditions1,vars_environ,vars_health,vars_performance)

mdstest <- chile[,vars2keep] %>% as_tibble()

rm(list=setdiff(ls(), c("mdstest")))

save.image("data/mdstest.RData")

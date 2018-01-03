# .libPaths("//WIMS.who.int/HQ/GVA11/Home/leel/My Documents/R/R-3.3.3/library")
load("/Users/lindsaylee/Dropbox/WHO/MDS/01b MDS Brief Version/08 Example report/04 Syntax/DescriptiveAnalysis/briefex.RData")

demos <- c("sex","age","age_cat", "work_cat","edu_cat","marital_status","ethnicity")
scores <- c("capacity_cat","performance_cat","CapacityScore","PerformanceScorePredicted")
vars2keep <- c(vars_id,demos,scores,vars_strata,vars_weights,vars_assistance,vars_assistance_nouseneed, vars_assistance_per,vars_assistance_use,
               vars_assistance_useneed,vars_attitude,vars_capacity,vars_conditions,vars_environ,vars_health,
               vars_performance,vars_support)


mdstest <- brief[,vars2keep]

rm(list=setdiff(ls(), c("mdstest")))

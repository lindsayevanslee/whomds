# # examples:
# testlet_strategy <- list(mobility = c("c1","c2"), c("c3","c4","c5"))
# recode_strategy <- list("c1,c2" = c(0,1,2,3,3),
#                         "c3,c4,c5" = c(0,1,1,2,2))
# drop_vars <- c("c1","c2")
# split_strategy <- list("sex" = c("c1","c2","c3"))


rasch_mds <- function(df, 
                      vars_metric,
                      vars_id,
                      vars_sex,
                      vars_age,
                      resp_opts = 1:5,
                      max_NA = 2,
                      testlet_strategy = NULL, 
                      recode_strategy = NULL, 
                      drop_vars = NULL, 
                      split_strategy = NULL,
                      comment = NULL,
                      metric_name = NULL,
                      path_output = NULL,
                      sink_errors = FALSE
                      ) {
  
  
  # PREPARE DATA ------------
  
  #recode non-resp_opts to NA
  to_NA <- df %>% 
    select(vars_metric) %>% 
    unlist() %>% 
    unique() %>% 
    setdiff(c(resp_opts, NA))
  
  df <- df %>%
    mutate_at(vars(vars_metric),
              dplyr::funs(plyr::mapvalues, .args = list(
                from = to_NA, to = rep(NA, length(to_NA)), warn_missing = FALSE
              )))
  
  #remove people with too many NAs
  rm_rows <- df %>% 
    select(vars_metric) %>% 
    is.na() %>% 
    rowSums()
  rm_rows <- rm_rows > max_NA
  
  df <- df %>% 
    filter(!rm_rows)
  
  #convert values to start at 0
  df <- df %>%
    mutate_at(vars(vars_metric),
              dplyr::funs(. - 1))
  
  
  #store initial data frame of maximum possible values for each variable
  max_values <- tibble(var = vars_metric,
                       max_val = max(resp_opts)-1)
  
  
  
  # PERFORM TESTLETS--------
  if (!is.null(testlet_strategy)) {
    testlet_result <- rasch_testlet(df, vars_metric, testlet_strategy, max_values, resp_opts)
    
    df <- testlet_result[["df"]]
    vars_metric <- testlet_result[["vars_metric"]]
    testlet_strategy <- testlet_result[["testlet_strategy"]]
    max_values <- testlet_result[["max_values"]]
    
    }
  
  # PERFORM RECODING --------
  if (!is.null(recode_strategy)) {
    recode_result <- rasch_recode(df, vars_metric, recode_strategy, max_values)
    
    df <- recode_result[["df"]]
    max_values <- recode_result[["max_values"]]
    
  }
  
  # DROP VARIABLES ---------
  if (!is.null(drop_vars)) {
    drop_result <- rasch_drop(df, vars_metric, drop_vars, max_values)
    
    df <- drop_result[["df"]]
    vars_metric <- drop_result[["vars_metric"]]
    max_values <- drop_result[["max_values"]]
    
  }
  
  # PERFORM FACTOR ANALYSIS ------------
  rasch_factor(df, vars_metric)
  
  
  # PERFORM SPLIT -------
  if (!is.null(split_strategy)) {
    split_result <- rasch_split(df, vars_metric, split_strategy, max_values)
    
    df <- split_result[["df"]]
    vars_metric <- split_result[["vars_metric"]]
    max_values <- split_result[["max_values"]]
    
  }
  
  
  # PERFORM RASCH ANALYSIS -----
  
  
  
  
  # PERFORM DIF ANALYSIS ---------
  
  
  
  # RESCALE SCORE --------
  
  
}
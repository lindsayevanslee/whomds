#' Top-level function to perform Rasch Analysis on WHO Model Disability Survey data for children
#'
#' @param df a data frame of individual survey data, where each row is an individual 
#' @param vars_metric_common a character vector the common items among all individuals
#' @param vars_metric_grouped a named list of character vectors with the items to use in the Rasch Analysis per group. The list should have names corresponding to the different groups, and contain character vectors of the corrsponding items for each group.
#' @param vars_id a string with column name uniquely identifying individuals
#' @param vars_metric a named list of character vectors with the items to use in the Rasch Analysis. One element of the list should be "common" and contain a character vector of the common items among all individuals. Other elements of the list should have names corresponding to the different groups, and contain character vectors of the corrsponding items for each group.
#' @param vars_DIF a string with the column names to use for analyzing differential item functioning (DIF). Default is NULL, to skip analysis of DIF.
#' @param resp_opts a numeric vector of possible response options for \code{vars_metric}. Must begin with 1. Default is 1:5
#' @param max_NA a numeric value for the maximum number of NAs allowed per individual among \code{vars_metric}. Default is 2.
#' @param print_results a logical value indicating whether or not to print various files displaying results from the Rasch Model. Default is TRUE, to print the files.
#' @param path_parent a string with the path to the folder where results from multiple models will be outputted. Default is NULL
#' @param model_name a string with a name for the model, which is used to create a new folder for model output. Default is NULL.
#' @param testlet_strategy a list giving the strategy to take for creating testlets, passed to \code{rasch_testlet()}. One element of the list per testlet to create. Each element of the list must be a character vector of column names to use for the testlet. Optionally, name the element of the list to give the name of the new testlet. Otherwise, the new testlet will be the original column names separated by "_". Default is NULL, to not create testlets.
#' @param recode_strategy a named list giving the strategy to take for recoding variables, passed to \code{rasch_recode()}. One element of the list per recode strategy. Each element of the list is a numeric vector giving the new values to map the variables to. The names of the list are the groups of column names to use for each recoding strategy, separated only by ",". Default is NULL, to not recode items.
#' @param drop_vars a character vector of column names to drop from the Rasch Analysis. Default is NULL, to not drop items.
#' @param split_strategy a named list giving the strategy to take for spliting variables by categories, passed to \code{rasch_split()}. One element of the list per variable to split by. Each element of the list must be a character vector of column names to split. The names of the list are the variables to split each group of variables by. Default is NULL, to not split items.
#' @param comment a string giving a comment describing the analysis, printed to a txt file. Default is NULL, to not print a comment.
#'
#' @details This function combines all of the separate analyses of model fit necessary to assess the quality of the Rasch Model. It is designed to require minimal intervention from the user. Users wishing to have more control over the analysis can use the other Rasch functions in this package separately.
#' 
#' @return a tibble with new columns representing the original person abilities (\code{person_pars}) and the rescaled person abilities (\code{rescaled}). 
#' 
#' If \code{print_results} is TRUE, prints files to the working directory with the results of the Rasch Model. 
#' 
#' @family rasch functions
#' 
#' @export
#' 
#' @import dplyr
rasch_mds_children <- function(df, 
                               vars_id,
                               vars_age_group,
                               vars_metric_common = NULL,
                               vars_metric_grouped = NULL,
                               vars_metric = NULL,
                               TAM_model = "PCM2",
                               vars_DIF = NULL,
                               resp_opts = 1:5,
                               has_at_least_one = 4:5,
                               max_NA = 10,
                               print_results = TRUE,
                               path_parent = NULL,
                               model_name = NULL,
                               testlet_strategy = NULL, 
                               recode_strategy = NULL, 
                               drop_vars = NULL, 
                               split_strategy = NULL,
                               comment = NULL
) {
  
  # # examples:
  # testlet_strategy <- list(mobility = c("c1","c2"), c("c3","c4","c5"))
  # recode_strategy <- list("c1,c2" = c(0,1,2,3,3),
  #                         "c3,c4,c5" = c(0,1,1,2,2))
  # drop_vars <- c("c1","c2")
  # split_strategy <- list("sex" = c("c1","c2","c3"))
  
  #TO IMPLEMENT
  #DONE take input of common items and age-specific items
  #DONE argument of models to try for TAM
  #DONE how to do age split
  #DONE adapt testlet, drop, recode, split to account for list of variables across groups -> should testlets be allowed between common items and specific items?
  #DONE split df by age group
  #DONE 6_RunModels through 10_ModelQuality should be basically fine
  #DONE 13_Thresholds, dependency graph, levels should ve mostly ok
  #adapt so that either anchored or multigroup model can be used
  #factor analysis? DIF?
  #test testlet, recode, drop, split
  
  #check for correct entry of vars_metric/_common/_grouped
  if (is.null(vars_metric) & (is.null(vars_metric_common) | is.null(vars_metric_grouped))) {
    stop("You either must enter EITHER vars_metric OR both vars_metric_common and vars_metric_grouped")
  } else if (!is.null(vars_metric) & (!is.null(vars_metric_common) | !is.null(vars_metric_grouped))) {
    stop("You entered vars_metric and at least one of vars_metric_common and vars_metric_grouped. You either must enter EITHER vars_metric OR both vars_metric_common and vars_metric_grouped.")
  }
  
  #combine items into one list
  if (is.null(vars_metric)) {
    vars_metric <- c(list(common = vars_metric_common),
                     vars_metric_grouped)
  }
  
  #perform some checks
  if (resp_opts[1]!=1) stop("resp_opts must start with 1")
  if (max_NA >= length(helper_varslist(vars_metric))) stop("max_NA must be less than length of vars_metric")
  
  # SAVE OUTPUT PATH ---------------------
  
  if (print_results) {
    if (!is.null(path_parent) & !is.null(model_name)) {
      path_output <- paste0(path_parent, model_name)
      dir.create(path_output, showWarnings = FALSE)
    } else{
      warning("You said you wanted to print results but did not provide an explicit path. Results will be printed to working directory.")
      path_output <- NULL
    }
  } else message("Results will not be printed (print_results is FALSE).")
  
  
  # PREPARE DATA ------------
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #recode non-resp_opts to NA, make vars_id character
  to_NA <- df %>% 
    select(helper_varslist(vars_metric)) %>% 
    unlist() %>% 
    unique() %>% 
    setdiff(c(resp_opts, NA))
  
  df <- df %>%
    mutate_at(vars(helper_varslist(vars_metric)),
              dplyr::funs(plyr::mapvalues, .args = list(
                from = to_NA, to = rep(NA, length(to_NA)), warn_missing = FALSE
              ))) %>% 
    mutate_at(vars(vars_id), funs(as.character))
  
  #remove people with too many NAs
  rm_rows <- df %>% 
    select(helper_varslist(vars_metric)) %>% 
    is.na() %>% 
    rowSums()
  rm_rows <- rm_rows > max_NA
  
  df <- df %>% 
    filter(!rm_rows)
  
  #convert values to start at 0
  df <- df %>%
    mutate_at(vars(helper_varslist(vars_metric)),
              dplyr::funs(. - 1))
  
  
  #store initial data frame of maximum possible values for each variable
  max_values <- tibble(var = helper_varslist(vars_metric),
                       max_val = max(resp_opts)-1)
  
  # save comment
  if (!is.null(comment)) utils::write.table(comment, file = paste0(path_output, "/Comment.txt"), row.names = FALSE, col.names = FALSE)
  
  
  # keep only those with at least one of vars_metric in has_at_least_one ---------
  if (!is.null(has_at_least_one)) {
    df <- df %>% 
      filter_at(vars(helper_varslist(vars_metric)), 
                any_vars(. %in% (has_at_least_one - 1)))
  }
  
  
  # SPLIT BY AGE/MAKE VARS DISCRETE BY AGE --------
  if (length(vars_metric) > 1) {
    split_age_result <- rasch_split_age(df = df,
                                        vars_age_group = vars_age_group, 
                                        vars_metric = vars_metric)
    df <- split_age_result[["df"]]
    vars_metric <- split_age_result[["vars_metric"]]
    
  }
  
  
  # PERFORM TESTLETS--------
  if (!is.null(testlet_strategy)) {
    testlet_result <- rasch_testlet(df = df, 
                                    vars_metric = vars_metric, 
                                    testlet_strategy = testlet_strategy, 
                                    max_values = max_values, 
                                    resp_opts = resp_opts)
    
    df <- testlet_result[["df"]]
    vars_metric <- testlet_result[["vars_metric"]]
    testlet_strategy <- testlet_result[["testlet_strategy"]]
    max_values <- testlet_result[["max_values"]]
    
    cat("Testlet creation completed. \n")
    
  }
  
  # PERFORM RECODING --------
  if (!is.null(recode_strategy)) {
    recode_result <- rasch_recode(df = df, 
                                  vars_metric = vars_metric, 
                                  recode_strategy = recode_strategy, 
                                  max_values = max_values)
    
    df <- recode_result[["df"]]
    max_values <- recode_result[["max_values"]]
    
    cat("Recoding variables completed. \n")
    
  }
  
  # DROP VARIABLES ---------
  if (!is.null(drop_vars)) {
    drop_result <- rasch_drop(vars_metric = vars_metric, 
                              drop_vars = drop_vars, 
                              max_values = max_values)
    
    vars_metric <- drop_result[["vars_metric"]]
    max_values <- drop_result[["max_values"]]
    
    cat("Dropping variables completed. \n")
    
  }
  
  # PERFORM SPLIT -------
  if (!is.null(split_strategy)) {
    split_result <- rasch_split(df = df, 
                                vars_metric = vars_metric, 
                                split_strategy = split_strategy, 
                                max_values = max_values)
    
    df <- split_result[["df"]]
    vars_metric <- split_result[["vars_metric"]]
    max_values <- split_result[["max_values"]]
    
    cat("Splitting variables completed. \n")
  }
  
  # SPLIT DATA BY AGE -----
  df_nest <- rasch_df_nest(df = df,
                           vars_age_group = vars_age_group,
                           vars_id = vars_id)
  
  

  # CALCULATE MODELS --------------
  df_nest <- rasch_model_children(df = df, 
                                  df_nest = df_nest,
                                  vars_metric = vars_metric,
                                  vars_age_group = vars_age_group,
                                  TAM_model = TAM_model)
  
  # CALCULATE MODEL QUALITY -----------
  df_nest <- rasch_quality_children(df_nest = df_nest,
                                    vars_metric = vars_metric)
  
 
  # PRINT RESULTS ------------
  if (print_results) {
    rasch_quality_children_print(df_nest = df_nest,
                                 vars_metric = vars_metric,
                                 vars_age_group = vars_age_group,
                                 TAM_model = TAM_model,
                                 path_output = path_output)
    
  }
  
  # 
  # 
  # 
  # 
  # # ADD RAW SCORE ---------
  # df <- rasch_rawscore(df = df,
  #                      vars_metric = vars_metric,
  #                      vars_id = vars_id,
  #                      max_values = max_values)
  # 
  # 
  # 
  # # PERFORM DIF ANALYSIS ---------
  # if (!is.null(vars_DIF)) {
  #   DIF_result <- rasch_DIF(df = df, 
  #                           vars_metric = vars_metric, 
  #                           vars_DIF = vars_DIF, 
  #                           residuals_PCM = residuals_PCM, 
  #                           split_strategy = split_strategy, 
  #                           print_results = print_results, 
  #                           path_output = path_output)
  #   
  #   cat("DIF analysis completed. \n")
  # }

  
  # RESCALE SCORE --------
  df_final <- rasch_rescale_children(df_nest = df_nest,
                                     vars_age_group = vars_age_group,
                                     vars_id = vars_id)
  
  # PRINT DATA ---------
  if (print_results) df_final %>% readr::write_csv(path = paste0(path_output, "/Data_final.csv"))
  
  
  # RETURN DATA WITH SCORE ----------
  return(list(df = df_final,
              vars_metric = vars_metric))
  
}
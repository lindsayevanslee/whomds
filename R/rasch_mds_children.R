#' Top-level function to perform Rasch Analysis on WHO Model Disability Survey data for children
#'
#' @param vars_group a string with the column name identifying grouping variable
#' @param vars_metric_common a character vector the common items among all individuals
#' @param vars_metric_grouped a named list of character vectors with the items to use in the Rasch Analysis per group. The list should have names corresponding to the different groups, and contain character vectors of the corresponding items for each group.
#' @param TAM_model a string with the type of IRT model to use, passed to \code{irtmodel} argument of \code{TAM::tam()}. Default is \code{"PCM2"}
#' @param vars_DIF Currently does nothing. In the future, a string with the column names to use for analyzing differential item functioning (DIF). Default is NULL, to skip analysis of DIF.
#' @param has_at_least_one a numeric vector with the response options that a respondent must have at least one of in order to be included in the metric calculation. See details for more information.
#' @inheritParams rasch_mds
#' 
#' @details This function combines all of the separate analyses of model fit necessary to assess the quality of the Rasch Model. It is designed to require minimal intervention from the user. Users wishing to have more control over the analysis can use the other Rasch functions in this package separately.
#' 
#' Often Rasch Analysis of children data is more difficult because of the extreme skewness of the responses. For this reason, it is often advisable to build a scale only with the respondents on the more severe end of the disability continuum. By specifying \code{has_at_least_one}, the function will remove all children from the sample who do endorse an answer of any of \code{has_at_least_one} in at least one \code{vars_metric}. The scores created can be reunited with the excluded children post-hoc.
#' 
#' @return a tibble with new columns representing the original person abilities (\code{person_pars}) and the rescaled person abilities (\code{rescaled}). 
#' 
#' If \code{print_results} is TRUE, prints files to the working directory with the results of the Rasch Model. 
#' 
#' @family rasch functions
#' @family children analysis functions
#' 
#' @export
#' 
#' @import dplyr
rasch_mds_children <- function(df, 
                               vars_id,
                               vars_group,
                               vars_metric_common,
                               vars_metric_grouped = NULL,
                               TAM_model = "PCM2",
                               vars_DIF = NULL,
                               resp_opts = 1:5,
                               has_at_least_one = 4:5,
                               max_NA = 10,
                               print_results = FALSE,
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
  #DONE adapt so that either anchored or multigroup model can be used
  #factor analysis? DIF?
  
  
  
  #combine items into one list
  vars_metric <- c(list(common = vars_metric_common),
                   vars_metric_grouped)
  
  #perform some checks
  if (resp_opts[1]!=1) stop("resp_opts must start with 1")
  if (!is.null(vars_metric_grouped) & is.null(vars_metric_common)) stop("vars_metric_grouped cannot be specified without vars_metric_common")
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
    select(all_of(helper_varslist(vars_metric))) %>% 
    unlist() %>% 
    unique() %>% 
    setdiff(c(resp_opts, NA))
  
  df <- df %>%
    mutate_at(vars(all_of(helper_varslist(vars_metric))),
              list(~ plyr::mapvalues(., 
                                     from = to_NA, 
                                     to = rep(NA, length(to_NA)), 
                                     warn_missing = FALSE)
                   )
              ) %>% 
    mutate_at(vars(all_of(vars_id)), list(~ as.character(.)))
  
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
    mutate_at(vars(all_of(helper_varslist(vars_metric))),
              list(~ as.numeric(as.character(.)) - 1))
  
  
  #store initial data frame of maximum possible values for each variable
  max_values <- tibble(var = helper_varslist(vars_metric),
                       max_val = max(resp_opts)-1)
  
  # save comment
  if (!is.null(comment)) utils::write.table(comment, file = paste0(path_output, "/Comment.txt"), row.names = FALSE, col.names = FALSE)
  
  
  # keep only those with at least one of vars_metric in has_at_least_one ---------
  if (!is.null(has_at_least_one)) {
    df <- df %>% 
      filter_at(vars(all_of(helper_varslist(vars_metric))), 
                any_vars(. %in% (has_at_least_one - 1)))
  }
  
  
  # SPLIT BY AGE/MAKE VARS DISCRETE BY AGE --------
  if (length(vars_metric) > 1) {
    split_age_result <- rasch_split_age(df = df,
                                        vars_group = vars_group, 
                                        vars_metric = vars_metric,
                                        vars_id = vars_id,
                                        max_values = max_values)
    df <- split_age_result[["df"]]
    vars_metric <- split_age_result[["vars_metric"]]
    max_values <- split_age_result[["max_values"]]
    
    message("Splitting by age to create discrete age-specific variables completed.")
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
    
    message("Testlet creation completed.")
  }
  
  # PERFORM RECODING --------
  if (!is.null(recode_strategy)) {
    recode_result <- rasch_recode(df = df, 
                                  vars_metric = vars_metric, 
                                  recode_strategy = recode_strategy, 
                                  max_values = max_values)
    
    df <- recode_result[["df"]]
    max_values <- recode_result[["max_values"]]
    
    message("Recoding variables completed.")
  }
  
  # DROP VARIABLES ---------
  if (!is.null(drop_vars)) {
    drop_result <- rasch_drop(vars_metric = vars_metric, 
                              drop_vars = drop_vars, 
                              max_values = max_values)
    
    vars_metric <- drop_result[["vars_metric"]]
    max_values <- drop_result[["max_values"]]
    
    message("Dropping variables completed.")
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
    
    message("Splitting variables completed.")
  }
  
  # PRINT RESPONSE FREQUENCIES -------
  if (print_results) {
    df %>% 
      select(helper_varslist(vars_metric)) %>% 
      purrr::map(~ table(resp = ., useNA = "always")) %>% 
      purrr::map(~ as_tibble(.)) %>% 
      bind_rows(.id = "Q") %>% 
      tidyr::pivot_wider(names_from = Q, values_from = n) %>% 
      readr::write_csv(paste0(path_output, "/response_freq.csv"))
    
  }
  
  # SPLIT DATA BY AGE -----
  df_nest <- rasch_df_nest(df = df,
                           vars_group = vars_group,
                           vars_id = vars_id)
  
  
  
  # CALCULATE MODELS --------------
  df_nest <- rasch_model_children(df = df, 
                                  df_nest = df_nest,
                                  vars_metric = vars_metric,
                                  vars_group = vars_group,
                                  TAM_model = TAM_model)
  message("Models completed.")
  
  # CALCULATE MODEL QUALITY -----------
  df_nest <- rasch_quality_children(df_nest = df_nest,
                                    vars_metric = vars_metric)
  message("Model quality calculated.")
  
  
  # PRINT RESULTS ------------
  if (print_results) {
    rasch_quality_children_print(df_nest = df_nest,
                                 vars_metric = vars_metric,
                                 vars_group = vars_group,
                                 TAM_model = TAM_model,
                                 path_output = path_output)
    message("Model quality printed.")
  }
  
  
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
  #   message("DIF analysis completed.")
  # }
  
  
  # RESCALE SCORE --------
  df_final <- rasch_rescale_children(df = df,
                                     df_nest = df_nest,
                                     vars_group = vars_group,
                                     vars_id = vars_id)
  
  # PRINT DATA ---------
  if (print_results) df_final %>% readr::write_csv(file = paste0(path_output, "/Data_final.csv"))
  
  
  # RETURN DATA WITH SCORE ----------
  return(list(df = df_final,
              vars_metric = vars_metric))
  
}
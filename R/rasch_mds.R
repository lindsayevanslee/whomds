#' Top-level function to perform Rasch Analysis on WHO Model Disability Survey data
#'
#' @param df a data frame of individual survey data, where each row is an individual 
#' @param vars_metric a character vector of items to use in the Rasch Analysis
#' @param vars_id a string with column name uniquely identifying individuals
#' @param vars_DIF a string with the column names to use for analyzing differential item functioning (DIF). Default is NULL, to skip analysis of DIF.
#' @param resp_opts a numeric vector of possible response options for \code{vars_metric}. Must begin with 1. Default is 1:5
#' @param max_NA a numeric value for the maximum number of NAs allowed per individual among \code{vars_metric}. Default is 2.
#' @param print_results a logical value indicating whether or not to print various files displaying results from the Rasch Model. Default is FALSE, to not print the files.
#' @param path_parent a string with the path to the folder where results from multiple models will be outputted. Default is NULL
#' @param model_name a string with a name for the model, which is used to create a new folder for model output. Default is NULL.
#' @param testlet_strategy a list giving the strategy to take for creating testlets, passed to \code{rasch_testlet()}. One element of the list per testlet to create. Each element of the list must be a character vector of column names to use for the testlet. Optionally, name the element of the list to give the name of the new testlet. Otherwise, the new testlet will be the original column names separated by "_". Default is NULL, to not create testlets.
#' @param recode_strategy a named list giving the strategy to take for recoding variables, passed to \code{rasch_recode()}. One element of the list per recode strategy. Each element of the list is a numeric vector giving the new values to map the variables to. The names of the list are the groups of column names to use for each recoding strategy, separated only by ",". Default is NULL, to not recode items.
#' @param drop_vars a character vector of column names to drop from the Rasch Analysis. Default is NULL, to not drop items.
#' @param split_strategy a named list giving the strategy to take for splitting variables by categories, passed to \code{rasch_split()}. One element of the list per variable to split by. Each element of the list must be a character vector of column names to split. The names of the list are the variables to split each group of variables by. Default is NULL, to not split items.
#' @param comment a string giving a comment describing the analysis, printed to a txt file. Default is NULL, to not print a comment.
#'
#' @details This function combines all of the separate analyses of model fit necessary to assess the quality of the Rasch Model. It is designed to require minimal intervention from the user. Users wishing to have more control over the analysis can use the other Rasch functions in this package separately.
#' 
#' @return a named list with:
#' \item{df}{a tibble with new columns representing the original person abilities (\code{person_pars}) and the rescaled person abilities (\code{rescaled})} 
#' \item{vars_metric}{a character vector with the variables used in the metric after all adjustments}
#' \item{df_results}{a tibble of one row with key results of the model}
#' 
#' If \code{print_results} is TRUE, prints files to the working directory with the results of the Rasch Model. 
#' 
#' @family rasch functions
#' 
#' @export
#' 
#' @import dplyr
rasch_mds <- function(df, 
                      vars_metric,
                      vars_id,
                      vars_DIF = NULL,
                      resp_opts = 1:5,
                      max_NA = 2,
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
  
  
  #perform some checks
  if (resp_opts[1]!=1) stop("resp_opts must start with 1")
  if (max_NA >= length(vars_metric)) stop("max_NA must be less than length of vars_metric")
  
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
    select(all_of(vars_metric)) %>% 
    unlist() %>% 
    unique() %>% 
    setdiff(c(resp_opts, NA))
  
  df <- df %>%
    mutate_at(vars(all_of(vars_metric)),
              list(~ plyr::mapvalues(., from = to_NA, to = rep(NA, length(to_NA)), warn_missing = FALSE
              ))) %>% 
    mutate_at(vars(all_of(vars_id)), list(as.character))
  
  #remove people with too many NAs
  rm_rows <- df %>% 
    select(all_of(vars_metric)) %>% 
    is.na() %>% 
    rowSums()
  rm_rows <- rm_rows > max_NA
  
  df <- df %>% 
    filter(!rm_rows)
  
  #convert values to start at 0 (explictly convert first to numeric, in case df has factor columns)
  df <- df %>%
    mutate_at(vars(all_of(vars_metric)),
              list(~ as.numeric(as.character(.)) - 1))
  
  
  #store initial data frame of maximum possible values for each variable
  max_values <- tibble(var = vars_metric,
                       max_val = max(resp_opts)-1)
  
  # save comment
  if (!is.null(comment)) utils::write.table(comment, file = paste0(path_output, "/Comment.txt"), row.names = FALSE, col.names = FALSE)
  
  
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
    
    if (print_results) {
      testlet_strategy %>% 
        tibble::enframe(name = "testlet", value = "original_var") %>% 
        tidyr::unnest(cols = original_var) %>% 
        readr::write_csv(file = paste0(path_output, "/testlet_strategy.csv"))
      
    }
    
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
    
    if (print_results) {
      
      recode_strategy %>% 
        tibble::enframe(name = "variable", value = "recoded") %>% 
        tidyr::separate(col = variable,
                        #programmatically determine number of columns to separate into
                        into = pull(., "variable") %>% 
                          stringr::str_split(",") %>% 
                          purrr::map_dbl(length) %>% 
                          max() %>% 
                          `:`(1,.) %>% 
                          paste0("var",.),
                        sep = ",") %>% 
        tidyr::pivot_longer(cols = starts_with("var"),
                            values_to = "variable") %>% 
        dplyr::filter(!is.na(variable)) %>%
        dplyr::rowwise() %>% 
        dplyr::mutate(original = list(0:(length(recoded) - 1))) %>% 
        tidyr::unnest(cols = c(original, recoded)) %>% 
        dplyr::select(variable, original, recoded) %>% 
        readr::write_csv(file = paste0(path_output, "/recode_strategy.csv"))     
      
    }
    
    message("Recoding variables completed.")
    
  }
  
  # DROP VARIABLES ---------
  if (!is.null(drop_vars)) {
    drop_result <- rasch_drop(vars_metric = vars_metric, 
                              drop_vars = drop_vars, 
                              max_values = max_values)
    
    vars_metric <- drop_result[["vars_metric"]]
    max_values <- drop_result[["max_values"]]
    
    if (print_results) {
      
      tibble::tibble(dropped_var = drop_vars) %>% 
        readr::write_csv(file = paste0(path_output, "/drop_vars.csv"))
      
      
    }
    
    message("Dropping variables completed.")
    
  }
  
  # PRINT RESPONSE FREQUENCIES -------
  if (print_results) {
    df %>% 
      dplyr::select(all_of(vars_metric)) %>% 
      purrr::map(~ table(resp = ., useNA = "always")) %>% 
      purrr::map(~ as_tibble(.)) %>% 
      bind_rows(.id = "Q") %>% 
      tidyr::pivot_wider(names_from = Q, values_from = n) %>% 
      dplyr::mutate(resp = as.numeric(resp)) %>% 
      dplyr::arrange(resp) %>% 
      readr::write_csv(paste0(path_output, "/response_freq.csv"))
    
  }
  
  # PERFORM FACTOR ANALYSIS ------------
  factor_result <- rasch_factor(df = df, 
                                vars_metric = vars_metric, 
                                print_results = print_results, 
                                path_output = path_output)
  
  
  message("Factor analysis completed.")
  
  # PERFORM SPLIT -------
  if (!is.null(split_strategy)) {
    split_result <- rasch_split(df = df, 
                                vars_metric = vars_metric, 
                                split_strategy = split_strategy, 
                                max_values = max_values)
    
    df <- split_result[["df"]]
    vars_metric <- split_result[["vars_metric"]]
    max_values <- split_result[["max_values"]]
    
    if (print_results) {
      
      split_strategy %>% 
        tibble::enframe(name = "split_category", value = "variable_to_split") %>% 
        tidyr::unnest(cols = c(split_category, variable_to_split)) %>% 
        readr::write_csv(file = paste0(path_output, "/split_strategy.csv"))
      
    }
    
    message("Splitting variables completed.")
  }
  
  # ADD RAW SCORE ---------
  df <- rasch_rawscore(df = df,
                       vars_metric = vars_metric,
                       vars_id = vars_id,
                       max_values = max_values)
  
  # PERFORM RASCH ANALYSIS -----
  model_result <- rasch_model(df = df,
                              vars_metric = vars_metric,
                              vars_id = vars_id,
                              print_results = print_results,
                              path_output = path_output)
  
  residuals_PCM <- model_result[["residuals_PCM"]]
  df_score <- model_result[["df_score"]]
  
  message("Rasch Model completed.")
  
  
  
  # PERFORM DIF ANALYSIS ---------
  if (!is.null(vars_DIF)) {
    DIF_result <- rasch_DIF(df = df, 
                            vars_metric = vars_metric, 
                            vars_DIF = vars_DIF, 
                            residuals_PCM = residuals_PCM, 
                            split_strategy = split_strategy, 
                            print_results = print_results, 
                            path_output = path_output)
    
    message("DIF analysis completed.")
  } else DIF_result <- list(DIF_results = "did not calculate")
  
  
  
  # RESCALE SCORE --------
  df_final <- rasch_rescale(df = df,
                            df_score = df_score,
                            vars_id = vars_id)
  
  # PRINT DATA ---------
  if (print_results) df_final %>% readr::write_csv(file = paste0(path_output, "/Data_final.csv"))
  
  
  # PREPARE RESULTS DF ROW ----------
  df_results <- tibble(model_name = model_name,
                       vars_metric = paste(vars_metric, collapse = ", "),
                       comment = comment,
                       LID = model_result$LID_results,
                       unidimensionality = factor_result$n_group_factors,
                       disordering = model_result$disordered_results,
                       DIF = DIF_result$DIF_results,
                       item_fit = model_result$fit_results,
                       model_fit = model_result$PSI)
  
  
  # RETURN DATA WITH SCORE ----------
  return(list(df = df_final,
              vars_metric = vars_metric,
              df_results = df_results))
  
}
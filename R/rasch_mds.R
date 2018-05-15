#' Top-level function to perform Rasch Analysis on WHO Model Disability Survey data
#'
#' @param df a data frame of individual survey data, where each row is an individual 
#' @param vars_metric a character vector of items to use in the Rasch Analysis
#' @param vars_id a string with column name uniquely identifying individuals
#' @param vars_DIF a string with the column names to use for analyzing differential item functioning (DIF). Default is NULL, to skip analysis of DIF.
#' @param resp_opts a numeric vector of possible response options for \code{vars_metric}. Must begin with 1. Default is 1:5
#' @param max_NA a numeric value for the maximum number of NAs allowed per individual among \code{vars_metric}. Default is 2.
#' @param testlet_strategy a list giving the strategy to take for creating testlets, passed to \code{rasch_testlet()}. One element of the list per testlet to create. Each element of the list must be a character vector of column names to use for the testlet. Optionally, name the element of the list to give the name of the new testlet. Otherwise, the new testlet will be the original column names separated by "_". Default is NULL, to not create testlets.
#' @param recode_strategy a named list giving the strategy to take for recoding variables, passed to \code{rasch_recode()}. One element of the list per recode strategy. Each element of the list is a numeric vector giving the new values to map the variables to. The names of the list are the groups of column names to use for each recoding strategy, separated only by ",". Default is NULL, to not recode items.
#' @param drop_vars a character vector of column names to drop from the Rasch Analysis. Default is NULL, to not drop items.
#' @param split_strategy a named list giving the strategy to take for spliting variables by categories, passed to \code{rasch_split()}. One element of the list per variable to split by. Each element of the list must be a character vector of column names to split. The names of the list are the variables to split each group of variables by. Default is NULL, to not split items.
#' @param comment a string giving a comment describing the analysis, printed to a txt file. Default is NULL, to not print a comment.
#' @param print_results a logical value indicating whether or not to print various files displaying results from the Rasch Model. Default is TRUE, to print the files.
#' @param model_name a string with a name for the model, which is used to create a new folder for model output. Default is NULL.
#' @param path_parent a string with the path to the folder where results from multiple models will be outputted. Default is NULL
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
rasch_mds <- function(df, 
                      vars_metric,
                      vars_id,
                      vars_DIF = NULL,
                      resp_opts = 1:5,
                      max_NA = 2,
                      testlet_strategy = NULL, 
                      recode_strategy = NULL, 
                      drop_vars = NULL, 
                      split_strategy = NULL,
                      comment = NULL,
                      print_results = TRUE ,
                      model_name = NULL,
                      path_parent = NULL
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
  if (!is_tibble(df)) df <- df %>% as_tibble()
  
  #recode non-resp_opts to NA, make vars_id character
  to_NA <- df %>% 
    select(vars_metric) %>% 
    unlist() %>% 
    unique() %>% 
    setdiff(c(resp_opts, NA))
  
  df <- df %>%
    mutate_at(vars(vars_metric),
              dplyr::funs(plyr::mapvalues, .args = list(
                from = to_NA, to = rep(NA, length(to_NA)), warn_missing = FALSE
              ))) %>% 
    mutate_at(vars(vars_id), funs(as.character))
  
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
  
  # PERFORM FACTOR ANALYSIS ------------
  factor_result <- rasch_factor(df = df, 
                                vars_metric = vars_metric, 
                                print_results = print_results, 
                                path_output = path_output)
  
  
  cat("Factor analysis completed. \n")
  
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
  
  cat("Rasch Model completed. \n")
  
  
  
  # PERFORM DIF ANALYSIS ---------
  if (!is.null(vars_DIF)) {
    DIF_result <- rasch_DIF(df = df, 
                            vars_metric = vars_metric, 
                            vars_DIF = vars_DIF, 
                            residuals_PCM = residuals_PCM, 
                            split_strategy = split_strategy, 
                            print_results = print_results, 
                            path_output = path_output)
    
    cat("DIF analysis completed. \n")
  }
  
  
  
  # RESCALE SCORE --------
  df_final <- rasch_rescale(df = df,
                            df_score = df_score,
                            vars_id = vars_id)
  
  
  # RETURN DATA WITH SCORE ----------
  return(df_final)
  
}
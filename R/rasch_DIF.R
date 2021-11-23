#' Perform analysis of Differential Item Functioning (DIF) for Rasch Model
#'
#' @param residuals_PCM a matrix giving the residuals of the person parameters from the Rasch Model. Row names are the numbers of the people and the columns are for each variable.
#' @param breaks a numeric value giving the number if class intervals. Default is 6.
#' @inheritParams rasch_mds
#' @inheritParams rasch_factor
#'
#' @return a list with results from the DIF analysis: 
#' \item{df_DIF_class}{the person residuals from the Rasch Model, the assigned class intervals, and the variables used for DIF analysis}
#' \item{tab_aov_DIF}{the results of the ANOVA used to analyze DIF}
#' \item{DIF_results}{string of various that exhibit some form of DIF}
#' 
#' @details Differential Item Functioning (DIF) refers to the circumstance when different groups in a sample respond to items in different ways. For instance, DIF would be observed if men and women had different patterns of responses to a set of survey questions. DIF can cause poor fit for the Rasch Model, and therefore should be analyzed. This function uses ANOVA to find DIF by the variables supplied and by a generated class interval.
#' 
#' @family rasch functions
#' 
#' @export
#' @import dplyr
#' 
#' @note Currently the calculation of the class intervals is quite slow. Reducing the number of breaks can improve speed.
rasch_DIF <- function(df, vars_metric, vars_DIF, residuals_PCM, split_strategy = NULL, print_results = FALSE, path_output = NULL, breaks = 6) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #save data frame for DIF
  df_DIF <- df %>% 
    select(c(vars_metric,vars_DIF))
  
  df_metric <- df_DIF %>% 
    select(vars_metric)
  
  
  ### "DIF analysis bases on the residuals
  residuals_PCM <- residuals_PCM %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("person") %>% 
    rename_at(vars(vars_metric), list(~ paste0(.,"_Res")))
  
  vars_metric_res <- colnames(residuals_PCM)[-1] #list of names of variable residuals
  
  rows <- residuals_PCM %>% 
    pull(person) %>% 
    stringr::str_extract_all("[:digit:]+") %>% 
    unlist() %>% 
    as.numeric()
  
  ###create class intervals
  
  #Get the residuals....
  residuals_PCM_cols <- residuals_PCM %>%
    select(-person)
  
  df_metric_rows <- df_metric %>% 
    slice(rows)
  
  df_metric_rows_sums <- rowSums(df_metric_rows,na.rm=TRUE)
  
  rows_sums_quantile <- stats::quantile(df_metric_rows_sums, probs = seq(0, 1, 1/breaks), na.rm=TRUE)
  
  lst <- purrr::map(list()[1:(breaks-1)], ~ c("<","<="))
  
  Gri <- expand.grid(lst) %>% 
    as_tibble() %>% 
    mutate_all(as.character)
  Grid <- Gri %>% 
    mutate(Var6 = "<=")
  
  Mat <- rows_sums_quantile[-1] %>% 
    tibble() %>% 
    t() %>% 
    as_tibble() %>% 
    slice(rep(1,nrow(Grid)))
  
  Intervals <- purrr::map2_dfc(Grid, Mat, ~paste0(..1, ..2))
  
  Dmat <- df_metric_rows_sums %>% 
    tibble() %>% 
    t() %>% 
    as_tibble() %>% 
    slice(rep(1,nrow(Grid)))
  
  
  ###here save the group intervals
  Detect <- function(x,y){
    Dbound <- outer(x, y, paste)
    Where <- apply(Dbound, c(1, 2), function(elem) eval(parse(text = elem)))
    apply(Where, 1, function(r) min(which(r), na.rm = TRUE))
  }
  
  what <- list()
  for(i in 1:nrow(Dmat)){ #very slow
    what[[i]] <- Detect(Dmat[i,],Intervals[i,])
  }
  
  names(what) <- paste0("v",1:length(what))
  
  sol <- bind_cols(what) %>% t() %>% as_tibble()
  k <- apply(sol,1,table)
  
  
  if(class(k)=="matrix"){
    Grouping <- which.min(apply(k,2,var))
  } else {
    take_in <- which(lapply(k, length)==breaks)
    K <- k[take_in]
    Grouping <- take_in[which.min(unlist(lapply(K,var)))]
  }
  
  class_intervals <- sol %>% 
    slice(Grouping) %>% 
    t()
  
  df_DIF_class <- bind_cols(class_intervals = class_intervals, 
                            select(residuals_PCM,-person), 
                            slice(df_DIF, rows))
  
  #perform ANOVA for each vars_metric_res and each vars_DIF
  list_aov_DIF <- purrr::map(.x = vars_metric_res,
                        .f = ~ purrr::map2(.x, .y = vars_DIF,
                                           .f = function(res, dif) {
                                             
                                             # if there was a split AND  this vars_DIF was used to split vars AND if this vars_metric was split by this vars_DIF
                                             if(!is.null(split_strategy) & 
                                                (dif %in% names(split_strategy)) & 
                                                (unlist(strsplit(res,"_"))[1] %in% split_strategy[[dif]])){
                                               
                                               result <- matrix(NA, nrow = 4, ncol = 5)
                                             } 
                                             #if there was not a split OR this vars_DIF not used to split vars OR  this variable not split by this vars_DIF
                                             else {
                                               
                                               #Anova of item residual and dif group
                                               result <- try(matrix(unlist(summary(
                                                 stats::aov(
                                                   as.numeric(pull(df_DIF_class, res)) ~ # residual
                                                     as.factor(pull(df_DIF_class, dif)) + #demographic
                                                     as.factor(pull(df_DIF_class, "class_intervals")) +  #class interval
                                                     as.factor(pull(df_DIF_class, dif)):as.factor(pull(df_DIF_class, "class_intervals")) #interaction between demo and class_intervals
                                                 )
                                               )), ncol = 5), silent = TRUE)
                                             }
                                             
                                             
                                             if (class(result) != "try-error") {
                                               colnames(result) <-
                                                 c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
                                               rownames(result) <-
                                                 c(
                                                   dif,
                                                   "class interval",
                                                   paste0(dif, ":class interval"),
                                                   "residuals"
                                                 )
                                               
                                               result <- result %>% 
                                                 as_tibble(rownames = "anova") %>% 
                                                 tibble::add_column(DIF = dif, .before = 1)
                                               
                                             }
                                             
                                             return(result)
                                             
                                           }) %>% bind_rows()
                        
  )
  
  names(list_aov_DIF) <- vars_metric_res
  
  
  #store Bonferonni corrected p-value
  Bonferonni <- 0.05 / (length(vars_DIF) *
                          3 *
                          (length(vars_DIF) + length(vars_metric)))
  
  #tab_aov_DIF stacks data frames for each variable vertically and flags possible DIF
  tab_aov_DIF <- bind_rows(list_aov_DIF, .id = "variable") %>% 
    mutate(!!rlang::sym(paste0("significant_at_",Bonferonni)) := case_when(
      `Pr(>F)` < Bonferonni ~ "X",
      TRUE ~ "-"
    ))
  
  DIF_results <- tab_aov_DIF %>% 
    select(variable, contains("significant")) %>% 
    filter_at(vars(contains("significant")),
              all_vars(. == "X")) %>% 
    pull(variable) %>% 
    paste(collapse = ", ")
  
  if (print_results) {
    
    utils::write.csv(df_DIF_class, file = paste0(path_output,"/Anova_Residuals.csv"), row.names = FALSE)
    
    utils::write.csv(tab_aov_DIF, file = paste0(path_output, "/DIF_rumm.csv"), row.names = FALSE)
    
  }
  
  
  DIF_result <- list(df_DIF_class = df_DIF_class,
                     tab_aov_DIF = tab_aov_DIF,
                     DIF_results = DIF_results)
  
  return(DIF_result)
  
  
  
}
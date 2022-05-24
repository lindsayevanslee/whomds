#' Run the Rasch Model and print diagnostic results
#'
#' @param path_output a string with the path to the output folder. Default is NULL.
#' @param LIDcutoff either a numeric value between 0 and 1 indicating the cut-off for significant local item dependence, or the string "christensen" to use the cut-off suggested by Christensen et al. 2017 (see reference). If "christensen" cut-off fails, defaults to 0.2.
#' @inheritParams rasch_mds
#'
#' @details The Rasch Model is calculated using the function \code{eRm::PCM()}. 
#' 
#' @return a list with results from the Rasch Model:
#' \item{model}{the results from the Rasch Model}
#' \item{df_score}{a tibble with the items used in the analysis and the person abilities}
#' \item{thresholds}{the item thresholds (i.e., crossings)}
#' \item{person_parameters}{person abilities}
#' \item{PSI}{the person-separation index}
#' \item{item_fit}{infit and outfit statistics per item}
#' \item{residuals_PCM}{the standardized person residuals}
#' \item{LID}{matrix with the item residual correlations}
#' \item{targeting}{a matrix with information on the targeting of the model}
#' \item{fit_results}{a string with results of the item fit}
#' \item{LID_results}{a string with results of the local item dependency}
#' \item{disordered_results}{a string listing items with disordered thresholds}
#' 
#' @family rasch functions
#' 
#' @references Christensen, K. B., Makransky, G., & Horton, M. (2017). Critical Values for Yen’s Q 3 : Identification of Local Dependence in the Rasch Model Using Residual Correlations.  Applied Psychological Measurement, 41(3), 178-194. \doi{10.1177/0146621616677520}
#' 
#' @export
#' 
#' @import eRm
rasch_model <- function(df, vars_metric, vars_id, print_results = FALSE, path_output = NULL, LIDcutoff = 0.2) {

  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #1. PCM analysis
  model <- PCM(df[,vars_metric])                                      
  
  #-------------------------------------------------------------------------------  
  #2. Item Difficulty thresholds
  Thr_PCM <- thresholds(model)
  
  Thresholds_Table_Recoded <- cbind(Thr_PCM$threshpar, Thr_PCM$se.thresh)
  colnames(Thresholds_Table_Recoded) <- c("Threshold", "SE Threshold")
  
  disordered_results <- Thresholds_Table_Recoded %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    as_tibble() %>% 
    mutate(var = stringr::str_replace_all(var, "thresh beta ", "")) %>% 
    tidyr::separate(col = var, into = c("var", "n_threshold"), sep = "\\.") %>% 
    select(-`SE Threshold`) %>% 
    arrange(var, n_threshold) %>% 
    group_by(var) %>% 
    summarize(disordered = is.unsorted(Threshold)) %>% 
    filter(disordered) %>% 
    pull(var) %>% 
    paste(collapse = ", ")

  #-------------------------------------------------------------------------------  
  #4. person parameters
  person_parameters <- person.parameter(model)
  
  
  #------------------------------------------------------------------------------- 
  #5. Person separation index
  PSIreport <- as.numeric(SepRel(person_parameters)[1])
  
  #-------------------------------------------------------------------------------  
  #6. item.fit
  Itemfit <- eRm::itemfit(person_parameters)
  table_Itemfit <- as.data.frame(cbind(Itemfit$i.fit, Itemfit$i.df, Itemfit$i.outfitMSQ,   Itemfit$i.infitMSQ,  Itemfit$i.outfitZ,  Itemfit$i.infitZ  ) )
  names(table_Itemfit) <- c("i.fit", "i.df", "i.outfitMSQ",   "i.infitMSQ",  "i.outfitZ",  "i.infitZ" )
  
  
  fit_results <- table_Itemfit %>% 
    tibble::rownames_to_column("var") %>% 
    as_tibble() %>% 
    select(var, contains("MSQ")) %>% 
    mutate_at(vars(contains("MSQ")),
              list(~ case_when(
                . > 1.1 ~ "Overfit",
                . < 0.9 ~ "Underfit",
                TRUE ~ "OK"
              ))) %>% 
    mutate(fit = case_when(
      i.outfitMSQ == "Underfit" & i.infitMSQ == "Overfit" ~ "Underfit & Overfit",
      i.infitMSQ == "Underfit" & i.outfitMSQ == "Overfit" ~ "Underfit & Overfit",
      i.outfitMSQ == "Underfit" | i.infitMSQ == "Underfit" ~ "Underfit",
      i.outfitMSQ == "Overfit" | i.infitMSQ == "Overfit" ~ "Overfit",
      TRUE ~ "OK"
    )) %>% 
    select(var, fit) %>% 
    group_by(fit) %>% 
    summarize(vars = paste(var, collapse = ",")) %>% 
    tidyr::unite(col = "fit_results", fit, vars, sep = ": ") %>%
    pull(fit_results) %>% 
    paste(collapse = "; ")
  
  
  ##additional cut-off for the fit based on Smith (see litterature)
  Sample_Size <- nrow(df)
  Cut_Infit <- 1+(2/sqrt(Sample_Size))
  Cut_Outfit <- 1+(6/sqrt(Sample_Size))
  
  Additional_Row <- as.data.frame(rbind(table_Itemfit,rep(NA, ncol(table_Itemfit)), rep(NA,ncol(table_Itemfit))))
  Additional_Row <- apply(Additional_Row,2,as.numeric)
  
  Additional_Row[nrow(Additional_Row)-1,3] <- Cut_Outfit
  Additional_Row[nrow(Additional_Row)-1,4] <- Cut_Infit
  Additional_Row[nrow(Additional_Row),1] <- PSIreport
  rownames(Additional_Row) <- c(vars_metric,"Smith's Critical Cut-Offs", "PSI") 
  
  #------------------------------------------------------------------------------- 
  #7. Standardized Residuals
  Residuals_PCM_Recoded <- stats::residuals(person_parameters)

  #------------------------------------------------------------------------------- 
  #8. Person Abilities
  person_par <-  person_parameters$theta.table 
  names(person_par) <- c("person_pars", "NAgroup", "Interpolated")
  table(person_par$Interpolated, is.na(person_par$person_pars))                                                               
  
  data_persons <-  cbind(df[,c(vars_id,vars_metric)],person_par)
  summary(data_persons$person_pars)
  
  for(i in 1:nrow(data_persons)){
    if(is.na(data_persons[i,"person_pars"])){
      data_persons[i,"person_pars"]<-min(data_persons[,"person_pars"], na.rm=TRUE)
    }
  }
  
  ###more persons abilities 
  Person_Abilities <- cbind(person_parameters$thetapar$NAgroup1,person_parameters$se.theta$NAgroup1)
  colnames(Person_Abilities) <- c("Abil", "SE_Abil")
  ###
  
  #------------------------------------------------------------------------------- 
  #9. Local Dependency
  
  #Correlation Plot for Local Dependence
  LID <- stats::cor(Residuals_PCM_Recoded, use="pairwise.complete", method="pearson")
  
  LIDforgraph <- LID
  
  if (LIDcutoff == "christensen") {
    
    LIDcutoff <- mean(LID[(LID < 1)]) + 0.2
    
    if (is.na(LIDcutoff) | is.null(LIDcutoff) | is.nan(LIDcutoff)) {
      
      warning("Christensen LID cut-off did not work. Defaulting to 0.2")
      LIDcutoff <- 0.2
      
    }
    
  } else if (is.character(LIDcutoff)) {
    warning("Unknown specification for LIDcutoff. Defaulting to 0.2")
    LIDcutoff <- 0.2
      
    }
  
  LID_results <- ((LIDforgraph >= LIDcutoff)*1 - diag(length(vars_metric))) %>% 
    data.frame() 
  LID_results[upper.tri(LID_results)] <- 0
  LID_results <- LID_results %>%   
    tibble::rownames_to_column("var1") %>% 
    as_tibble() %>% 
    tidyr::pivot_longer(!var1, names_to = "var2", values_to = "LID") %>% 
    filter(LID == 1) %>% 
    select(-LID) %>% 
    tidyr::unite(col = "vars", var1, var2, sep = " & ") %>% 
    pull(vars) %>% 
    paste(collapse = "; ")
  
  #------------------------------------------------------------------------------- 
  #10. Principal component analyis: PCA
  
  PCA <- try(stats::prcomp(LID,center=TRUE, retx=TRUE), silent=TRUE)
  Eigen_Value <- try(eigen(LID)$values, silent=TRUE)
  if (!any(inherits(Eigen_Value, "try-error"))) {
    Percentage_Eigen_Value <- Eigen_Value/sum(Eigen_Value)*100
    Cumulative_Percentage_Eigen_Value <- cumsum(Percentage_Eigen_Value)
    Eigen_Value_Table <- cbind(Eigen_Value,Percentage_Eigen_Value,Cumulative_Percentage_Eigen_Value)
    }
  
  #------------------------------------------------------------------------------ 
  #11. Targeting
  
  #Item Difficulties
  
  Mean_Difficulty <- mean(Thresholds_Table_Recoded[,1], na.rm=TRUE)
  SD_Difficulty <- sd(Thresholds_Table_Recoded[,1], na.rm=TRUE)
  Mean_Resi_Dif <- mean(Thresholds_Table_Recoded[,2], na.rm=TRUE)
  SD_Resi_Dif <- sd(Thresholds_Table_Recoded[,2], na.rm=TRUE)
  Target_Difficulty <- cbind(Mean_Difficulty, SD_Difficulty, Mean_Resi_Dif, SD_Resi_Dif)
  
  #Person Abilities  
  Mean_Ability <- mean(Person_Abilities[,1])
  SD_Ability <- sd(Person_Abilities[,1])
  Mean_Resi_Abil <-  mean(Person_Abilities[,2])
  SD_Resi_Abil <-  sd(Person_Abilities[,2])
  Target_Ability <- cbind(Mean_Ability, SD_Ability, Mean_Resi_Abil, SD_Resi_Abil)
  
  Targeting <- cbind(rbind(Target_Difficulty, Target_Ability), PSIreport)
  rownames(Targeting) <- c("Difficulty", "Ability")
  colnames(Targeting) <- c("Mean", "SD", "Mean Residuals", "SD Residuals", "PSI")
  
  
  #PRINT RESULTS
  if (print_results) {
    
    #model
    save(model, file=paste0(path_output,"/PCM_model.RData"))
    
    #thresholds
    utils::write.csv(Thr_PCM[3], paste0(path_output,"/Location_Threshold.csv"))
    utils::write.csv(Thresholds_Table_Recoded, file=paste0(path_output,"/PCM_thresholds_CI_Recoded.csv"))
    
    #save ICC curves
    grDevices::pdf(paste0(path_output,"/ICC_curves.pdf"))
    plotICC(model, ask=FALSE)
    grDevices::dev.off()
    
    #save the person-item map
    grDevices::pdf(file=paste0(path_output,"/PImap.pdf"), width=7, height=9)
    plotPImap(model, sorted = TRUE)
    grDevices::dev.off()
    
    #item fit
    utils::write.csv(Additional_Row, file=paste0(path_output,"/Item_MSQs.csv"))
    utils::write.csv(table_Itemfit, file=paste0(path_output,"/item_fit.csv"))
    
    #standardized residuals
    utils::write.csv(Residuals_PCM_Recoded,file=paste0(path_output,"/Residuals_PCM.csv"))
    
    #data with abilities
    readr::write_csv(data_persons, paste0(path_output,"/DatawAbilities.csv"))
    
    #person parameters
    utils::write.csv(Person_Abilities, file=paste0(path_output,"/PersonPara.csv"))
    
    #residual correlations
    utils::write.csv(LID, file=paste0(path_output,"/Residual_Correlations.csv"))
    
    grDevices::pdf(paste0(path_output,"/LID_plot.pdf"))
    print(fig_LID(LIDforgraph, LIDcutoff, path_output))
    grDevices::dev.off()
    
    #PCA
    if (!any(inherits(Eigen_Value, "try-error"))) {
      utils::write.csv(Eigen_Value_Table, file= paste0(path_output,"/Original_Data_Eigenvalues.csv"))
      }
    
    if (!any(inherits(PCA, "try-error"))) {
      utils::write.csv(PCA$rotation, file=paste0(path_output,"/Original_Data_PCA.csv"))
      
      grDevices::pdf(paste0(path_output,"/Original_Data_Screeplot.pdf"))
      graphics::barplot(Eigen_Value, main="metric")
      stats::screeplot(PCA, type="lines", main="metric")
      grDevices::dev.off()
    }
    

    
    #Targeting
    utils::write.csv(Targeting, file=paste0(path_output,"/Targeting.csv"))
    
  }
  
  
  model_result <- list(model = model,
                        df_score = data_persons,
                        thresholds = Thr_PCM,
                        person_parameters = person_parameters,
                        PSI = PSIreport,
                        item_fit = Itemfit,
                        residuals_PCM = Residuals_PCM_Recoded,
                        LID = LID,
                        targeting = Targeting,
                        fit_results = fit_results,
                        LID_results = LID_results,
                        disordered_results = disordered_results)
  
  return(model_result)

  
}

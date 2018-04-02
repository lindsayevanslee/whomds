#' Run the Rasch Model and print diagnositic results
#'
#' @param df a tibble of individual survey data, where each row is an individual 
#' @param vars_metric a character vector of items to use in the Rasch Analysis
#' @param print_results a logical vector indicating whether to print the results of the model to the \code{model_name} directory
#' @param LIDcutoff a numeric value between 0 and 1 indicating the cut-off for significant local item dependence
#'
#' @return a list \code{model_result} with results from the Rasch Model
#' @export
rasch_model <- function(df, vars_metric, print_results = TRUE, LIDcutoff = 0.2) {

  #1. PCM analysis
  model <- PCM(df[,vars_metric])                                      
  
  #-------------------------------------------------------------------------------  
  #2. Item Difficulty thresholds
  Thr_PCM <- thresholds(model)
  
  Thresholds_Table_Recoded <- cbind(Thr_PCM$threshpar, Thr_PCM$se.thresh)
  colnames(Thresholds_Table_Recoded) <- c("Threshold", "SE Threshold")

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
  
  ##additional cut-off for the fit based on Smith (see litterature)
  Sample_Size <- nrow(data_orig)
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
  Residuals_PCM_Recoded <- residuals(person_parameters)

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
  LID <- cor(Residuals_PCM_Recoded, use="pairwise.complete", method="pearson")
  
  LIDforgraph <- LID
  
  #------------------------------------------------------------------------------- 
  #10. Principal component analyis: PCA
  
  PCA <- prcomp(LID,center=TRUE, retx=TRUE)
  Eigen_Value <- eigen(LID)$values
  Percentage_Eigen_Value <- eigen(LID)$value/sum(eigen(LID)$value)*100 
  Cumulative_Percentage_Eigen_Value <- cumsum(Percentage_Eigen_Value)
  Eigen_Value_Table <- cbind(Eigen_Value,Percentage_Eigen_Value,Cumulative_Percentage_Eigen_Value)

  
  #------------------------------------------------------------------------------- 
  #11. Targetting
  
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
  
  Targetting <- cbind(rbind(Target_Difficulty, Target_Ability), PSIreport)
  rownames(Targetting) <- c("Difficulty", "Ability")
  colnames(Targetting) <- c("Mean", "SD", "Mean Residuals", "SD Residuals", "PSI")
  
  
  #PRINT RESULTS
  if (print_results) {
    
    #model
    save(model, file="/PCM_model.RData")
    
    #thresholds
    write.csv(Thr_PCM[3], "Location_Threshold.csv")
    write.csv(Thresholds_Table_Recoded, file="PCM_thresholds_CI_Recoded.csv")
    
    #save ICC curves
    pdf("ICC_curves.pdf")
    plotICC(model, ask=FALSE)
    dev.off()
    
    #save the person-item map
    pdf(file="PImap.pdf", width=7, height=9)
    plotPImap(model, sorted = TRUE)
    dev.off()
    
    #item fit
    write.csv(Additional_Row, file="Item_MSQs.csv")
    write.csv(table_Itemfit, file="item_fit.csv")
    
    #standardized residuals
    write.csv(Residuals_PCM_Recoded,file="Residuals_PCM.csv")
    
    #data with abilities
    write.csv(data_persons, "DatawAbilities.csv",row.names = FALSE)
    
    #person parameters
    write.csv(Person_Abilities, file="PersonPara.csv")
    
    #residual correlations
    write.csv(LID, file="Residual_Correlations.csv")
    fig_LID(LIDforgraph, LIDcutoff)
    
    #PCA
    write.csv(Eigen_Value_Table, file= "Original_Data_Eigenvalues.csv" )
    write.csv(PCA$rotation, file="Original_Data_PCA.csv")
    
    pdf("Original_Data_Screeplot.pdf")
    barplot(Eigen_Value, main="metric")
    screeplot(PCA, type="lines", main="metric")
    dev.off()
    
    #Targeting
    write.csv(Targetting, file="Targetting.csv")
    
  }
  
  
  model_results <- list()
  
  return(model_results)

  
}
rasch_DIF <- function(df, vars_metric, vars_DIF, residuals_PCM) {
 
  #save data frame for DIF
  df_DIF <- df %>% 
    select(c(vars_metric,vars_DIF))
  
  df_metric <- df_DIF %>% 
    select(vars_metric)
  
  
  ### "DIF analysis bases on the residuals
  residuals_PCM <- residuals_PCM %>% 
    as_data_frame() %>% 
    tibble::rownames_to_column("person") %>% 
    rename_at(vars(vars_metric), funs(paste0(.,"_Res")))
  
  rows <- residuals_PCM %>% 
    pull(person) %>% 
    stringr::str_extract_all("[:digit:]+") %>% 
    unlist() %>% 
    as.numeric()
  
  ###create class intervals
  
  # CI <- Class_Intervals(df_DIF[,vars_metric], 6)
  breaks <- 6
  
  #Get the residuals....
  residuals_PCM_cols <- residuals_PCM %>%
    select(-person)
  
  df_metric_rows <- df_metric %>% 
    slice(rows)
  
  df_metric_rows_sums <- rowSums(df_metric_rows,na.rm=TRUE)
  
  
  S <- quantile(df_metric_rows_sums, probs = seq(0,1, 1/breaks), na.rm=TRUE)
  N <- breaks-1
  vec <- c("<","<=")
  lst <- lapply(numeric(N), function(df_metric) vec)
  Gri <- as.matrix(expand.grid(lst))
  Grid <- cbind(Gri, rep("<=",nrow(Gri)))
  Mat <- matrix(rep(S[2:(breaks+1)], nrow(Grid)), nrow=nrow(Grid), byrow=TRUE)
  
  P <- function(df_metric,y){
    paste0(df_metric,y)
  }
  
  Intervals <- matrix(mapply(P, Grid, Mat), ncol=breaks, byrow=FALSE)
  
  Dmat <- matrix(rep(df_metric_rows_sums,nrow(Grid)), nrow=nrow(Grid), byrow=TRUE)
  
  ###here save the group intervals
  
  Detect <- function(df_metric,y){
    
    Dbound <- outer(df_metric, y, paste)
    Eval_Pars <- function(Z) {
      eval(parse(text = Z))
    }
    Where <- apply(Dbound, c(1, 2), Eval_Pars)
    Position <- function(df_metric) {
      min(which(df_metric %in% 1), na.rm = TRUE)
    }
    apply(Where, 1, Position)
    
  }
  
  what <- list()
  for(i in 1:nrow(Dmat)){
    what[[i]] <- Detect(Dmat[i,],Intervals[i,])
  }
  
  sol <- do.call(rbind,what)
  k <- apply(sol,1,table)
  
  
  if(class(k)=="matrix"){
    Grouping <- min(which(apply(k,2,var)==min(apply(k,2,var, na.rm=TRUE))))
  } else {
    take_in <- which(lapply(k, length)==breaks)
    K <- k[take_in]
    Grouping <- take_in[min(which(lapply(K,var)==min(unlist(lapply(K,var, na.rm=TRUE)))  ))  ]
  }
  
  CI <- sol[Grouping,]  
  
  ######
  
  df_DIF_class <- bind_cols(CI, 
                              select(residuals_PCM,-person), 
                              slice(df_DIF, rows))
  
  # write.csv(df_DIF_class, file=paste(path_rasch,"Anova_Residuals.csv", sep=""))
  
  DIF_aov <- list()
  
  PCM_res_names <- colnames(residuals_PCM)[-1] #list of names of variable residuals
  
  #for each variable 
  for(j in 1:length(PCM_res_names)){ 
    
    DIF_aov[[j]] <- list()
    
    #for each of variables in split
    for(i in 1:length(vars_DIF)){ 
      
      ###check if it is a Split case
      
      # if there was a split
      if(Split){    
        #if this variable is split by this demographic characteristic
        if(nrow(match_df(data.frame(var = vars_metric[j], split = vars_DIF[i]), Split_Design))>0) { 
          Case_Question <- FALSE 
        }
        #if this variable not split by this demo
        else{ 
          Case_Question <- TRUE
        }
      } 
      #if there was not a split
      else { 
        Case_Question <- TRUE
      }
      
      if (Case_Question) {
        ###Anova of item residual and dif group
        DIF_aov[[j]][[i]] <- try(matrix(unlist(summary(
          aov(
            as.numeric(df_DIF_class[, PCM_res_names[j]]) ~ # residual
              as.factor(df_DIF_class[, vars_DIF[i]]) + #demographic
              as.factor(df_DIF_class[, "CI"]) +  #class interval
              as.factor(df_DIF_class[, vars_DIF[i]]):as.factor(df_DIF_class[, "CI"]) #interaction between demo and CI
          )
        )), ncol = 5), silent = TRUE)
      } 
      else {
        DIF_aov[[j]][[i]] <- rbind(c("n.a. split item", "", "", "", ""), rep("", 5), rep("",5), rep("", 5) )
      }
      
      if (class(DIF_aov[[j]][[i]]) != "try-error") {
        colnames(DIF_aov[[j]][[i]]) <-
          c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
        rownames(DIF_aov[[j]][[i]]) <-
          c(
            vars_DIF[i],
            paste0("class interval", i),
            paste0(vars_DIF[i], ":class interval", i),
            paste0("residuals", i)
          )
      }}
    
    names(DIF_aov[[j]]) <- vars_DIF
    
    DIF_aov[[j]] <- do.call(rbind, DIF_aov[[j]]) #stack info for each demo characteristic
    
  }
  names(DIF_aov) <- PCM_res_names
  
  #put all information into one excel sheet
  DIF_aov_table <- as.matrix(do.call(cbind, DIF_aov)) #DIF_aov_table combines data frames for each variable horizontally
  DIF_aov_table <- rbind(" ", DIF_aov_table) #add NA row
  rownames(DIF_aov_table)[1] <- "variable"
  DIF_aov_table[1,which(colnames(DIF_aov_table)%in%"Df")] <- PCM_res_names #add variable names
  
  write.csv(DIF_aov_table, file=paste0(path_rasch, "DIF_rumm.csv"))
  
  ###simplified DIFrumm output with only the p-values
  col.keep <- which(colnames(DIF_aov_table)=="Pr(>F)")
  row.remove <- which(rownames(DIF_aov_table)%in% c("variable","residuals"))
  DIF_aov_pval <- DIF_aov_table[-row.remove,col.keep]
  
  if (ncol(DIF_aov_pval)==length(PCM_res_names)) {
    colnames(DIF_aov_pval) <- PCM_res_names
    
    #spreadsheet that flags possible DIF
    DIF_aov_signif <- DIF_aov_pval
    Bonferonni <-
      0.05 / ((dim(df_DIF[, vars_DIF])[2]) * 3 * dim(df_DIF)[2])
    DIF_aov_signif[as.numeric(DIF_aov_signif) < Bonferonni] <- "X"
    DIF_aov_signif[DIF_aov_signif != "X"] <- " "
    write.csv(
      DIF_aov_signif,
      file = paste0(path_rasch, "Flagged DIF.csv"),
      row.names = TRUE
    )
    
    
    #print DIF p-values with Bonferroni corrected p-value
    DIF_aov_pval <- rbind(DIF_aov_pval, " ")
    DIF_aov_pval[nrow(DIF_aov_pval), 1:3] <-
      c("Bonferroni Corrected P-value",
        "item*3tests*DIFtests",
        0.05 / ((dim(df_DIF[, vars_DIF])[2]) * 3 * dim(df_DIF)[2]))
    
    write.csv(DIF_aov_pval, file = paste(path_rasch, "Sig DIF rumm.csv", sep =
                                           ""))
    
  }
  
  
  
  
}
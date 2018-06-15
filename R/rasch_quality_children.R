rasch_quality_children <- function(df_nest, vars_metric) {
  
  #calculate start model quality
  df_nest <- df_nest %>% 
    mutate(itemfit_start = map(mod_start, ~ msq.itemfit(.)$itemfit),
           cor_start = map(mod_start, ~ cor(IRT.residuals(.)$stand_residuals, use="pairwise.complete.obs")),
           xsithresh_start = map(mod_start, ~ cbind(.$xsi[vars_metric[["common"]],],
                                                    tam.threshold(.)[vars_metric[["common"]], ])),
           tthresh_start = map(mod_start, ~ tam.threshold(.)),
           eigen_start = map(cor_start, ~ ifelse(any(is.na(.)),
                                                 "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                 list(eigen(.)$values))),
           PCA_start = map(cor_start, ~ ifelse(any(is.na(.)),
                                               "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                               list(eigen(.)$vectors[,1]))),
           WLE_start = map(mod_start, ~ tam.wle(., progress = FALSE)),
           EAP_start = map(mod_start, ~ .$EAP.rel[1])
    )
  
  
  #calculate multigroup model quality
  df_nest <- df_nest %>% 
    mutate(itemfit_multigroup = map(mod_multigroup, ~ msq.itemfit(.)$itemfit),
           cor_multigroup = map(mod_multigroup, ~ cor(IRT.residuals(.)$stand_residuals, use="pairwise.complete.obs")),
           xsithresh_multigroup = map(mod_multigroup, ~ cbind(.$xsi[vars_metric[["common"]],],
                                                              tam.threshold(.)[vars_metric[["common"]], ])),
           tthresh_multigroup = map(mod_multigroup, ~ tam.threshold(.)),
           eigen_multigroup = map(cor_multigroup, ~ ifelse(any(is.na(.)),
                                                           "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                           list(eigen(.)$values))),
           PCA_multigroup = map(cor_multigroup, ~ ifelse(any(is.na(.)),
                                                         "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                         list(eigen(.)$vectors[,1]))),
           WLE_multigroup = map(mod_multigroup, ~ tam.wle(., progress = FALSE)),
           EAP_multigroup = map(mod_multigroup, ~ .$EAP.rel[1])
    )
  
  #calculate anchored model quality
  if (length(vars_metric) > 1) {
    df_nest <- df_nest %>% 
      mutate(itemfit_anchored = map(mod_anchored, ~ msq.itemfit(.)$itemfit),
             cor_anchored = map(mod_anchored, ~ cor(IRT.residuals(.)$stand_residuals, use="pairwise.complete.obs")),
             xsithresh_anchored = map(mod_anchored, ~ cbind(.$xsi[vars_metric[["common"]],],
                                                            tam.threshold(.)[vars_metric[["common"]], ])),
             tthresh_anchored = map(mod_anchored, ~ tam.threshold(.)),
             eigen_anchored = map(cor_anchored, ~ ifelse(any(is.na(.)),
                                                         "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                         list(eigen(.)$values))),
             PCA_anchored = map(cor_anchored, ~ ifelse(any(is.na(.)),
                                                       "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                       list(eigen(.)$vectors[,1]))),
             WLE_anchored = map(mod_anchored, ~ tam.wle(., progress = FALSE)),
             EAP_anchored = map(mod_anchored, ~ .$EAP.rel[1])
      )
  }
  
  return(df_nest)
  
  
}

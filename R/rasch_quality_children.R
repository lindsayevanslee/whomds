rasch_quality_children <- function(df_nest, vars_metric) {
  
  #calculate start model quality
  df_nest <- df_nest %>% 
    mutate(itemfit_start = msq.itemfit(mod_start)$itemfit,
           cor_start = cor(IRT.residuals(mod_start)$stand_residuals, use="pairwise.complete.obs"),
           xsithresh_start = cbind(mod_start$xsi[vars_metric[["common"]],],
                                   tam.threshold(mod_start)[vars_metric[["common"]], ]),
           tthresh_start = tam.threshold(mod_start),
           eigen_start = ifelse(any(is.na(cor_start)), 
                                "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                eigen(cor_start)$values),
           PCA_start = ifelse(any(is.na(cor_start)), 
                              "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                              eigen(cor_start)$vectors[,1]),
           WLE_start = tam.wle(mod_start, progress = FALSE),
           EAP_start = mod_start$EAP.rel[1])
  
  #calculate multigroup model quality
  df_nest <- df_nest %>% 
    mutate(itemfit_multigroup = msq.itemfit(mod_multigroup)$itemfit,
           cor_multigroup = cor(IRT.residuals(mod_multigroup)$stand_residuals, use="pairwise.complete.obs"),
           xsithresh_multigroup = cbind(mod_multigroup$xsi[vars_metric[["common"]],],
                                        tam.threshold(mod_multigroup)[vars_metric[["common"]], ]),
           tthresh_multigroup = tam.threshold(mod_multigroup),
           eigen_multigroup = ifelse(any(is.na(cor_multigroup)), 
                                     "NA in residual correlation matrix for multigroup model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                     eigen(cor_multigroup)$values),
           PCA_multigroup = ifelse(any(is.na(cor_multigroup)), 
                                   "NA in residual correlation matrix for multigroup model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                   eigen(cor_multigroup)$vectors[,1]),
           WLE_multigroup = tam.wle(mod_multigroup, progress = FALSE),
           EAP_multigroup = mod_multigroup$EAP.rel[1])
  
  #calculate anchored model quality
  df_nest <- df_nest %>% 
    mutate(itemfit_anchored = msq.itemfit(mod_anchored)$itemfit,
           cor_anchored = cor(IRT.residuals(mod_anchored)$stand_residuals, use="pairwise.complete.obs"),
           xsithresh_anchored = cbind(mod_anchored$xsi[vars_metric[["common"]],],
                                   tam.threshold(mod_anchored)[vars_metric[["common"]], ]),
           tthresh_anchored = tam.threshold(mod_anchored),
           eigen_anchored = ifelse(any(is.na(cor_anchored)), 
                                "NA in residual correlation matrix for anchored model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                eigen(cor_anchored)$values),
           PCA_anchored = ifelse(any(is.na(cor_anchored)), 
                              "NA in residual correlation matrix for anchored model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                              eigen(cor_anchored)$vectors[,1]),
           WLE_anchored = tam.wle(mod_anchored, progress = FALSE),
           EAP_anchored = mod_anchored$EAP.rel[1])
  
  
  
  
  return(df_nest)
  
  
}

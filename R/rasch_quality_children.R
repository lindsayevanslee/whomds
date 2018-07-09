#' Calculate quality of multigroup and anchored Rasch Models
#'
#' @inheritParams rasch_mds_children
#' @inheritParams rasch_model_children
#'
#' @return a nested tibble with new columns with information about model quality
#' @export
#' 
#' @family rasch functions
#' @family children analysis functions
#'
rasch_quality_children <- function(df_nest, vars_metric) {
  
  #calculate start model quality
  df_nest <- df_nest %>% 
    mutate(itemfit_start = purrr::map(mod_start, ~ TAM::msq.itemfit(.)$itemfit),
           cor_start = purrr::map(mod_start, ~ cor(TAM::IRT.residuals(.)$stand_residuals, use="pairwise.complete.obs")),
           xsithresh_start = purrr::map(mod_start, ~ cbind(.$xsi[vars_metric[["common"]],],
                                                    TAM::tam.threshold(.)[vars_metric[["common"]], ])),
           tthresh_start = purrr::map(mod_start, ~ TAM::tam.threshold(.)),
           eigen_start = purrr::map(cor_start, ~ ifelse(any(is.na(.)),
                                                 "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                 list(eigen(.)$values))),
           PCA_start = purrr::map(cor_start, ~ ifelse(any(is.na(.)),
                                               "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                               list(eigen(.)$vectors[,1]))),
           WLE_start = purrr::map(mod_start, ~ TAM::tam.wle(., progress = FALSE)),
           EAP_start = purrr::map(mod_start, ~ .$EAP.rel[1])
    )
  
  
  #calculate multigroup model quality
  df_nest <- df_nest %>% 
    mutate(itemfit_multigroup = purrr::map(mod_multigroup, ~ TAM::msq.itemfit(.)$itemfit),
           cor_multigroup = purrr::map(mod_multigroup, ~ cor(TAM::IRT.residuals(.)$stand_residuals, use="pairwise.complete.obs")),
           xsithresh_multigroup = purrr::map(mod_multigroup, ~ cbind(.$xsi[vars_metric[["common"]],],
                                                              TAM::tam.threshold(.)[vars_metric[["common"]], ])),
           tthresh_multigroup = purrr::map(mod_multigroup, ~ TAM::tam.threshold(.)),
           eigen_multigroup = purrr::map(cor_multigroup, ~ ifelse(any(is.na(.)),
                                                           "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                           list(eigen(.)$values))),
           PCA_multigroup = purrr::map(cor_multigroup, ~ ifelse(any(is.na(.)),
                                                         "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                         list(eigen(.)$vectors[,1]))),
           WLE_multigroup = purrr::map(mod_multigroup, ~ TAM::tam.wle(., progress = FALSE)),
           EAP_multigroup = purrr::map(mod_multigroup, ~ .$EAP.rel[1])
    )
  
  #calculate anchored model quality
  if (length(vars_metric) > 1) {
    df_nest <- df_nest %>% 
      mutate(itemfit_anchored = purrr::map(mod_anchored, ~ TAM::msq.itemfit(.)$itemfit),
             cor_anchored = purrr::map(mod_anchored, ~ cor(TAM::IRT.residuals(.)$stand_residuals, use="pairwise.complete.obs")),
             xsithresh_anchored = purrr::map(mod_anchored, ~ cbind(.$xsi[vars_metric[["common"]],],
                                                            TAM::tam.threshold(.)[vars_metric[["common"]], ])),
             tthresh_anchored = purrr::map(mod_anchored, ~ TAM::tam.threshold(.)),
             eigen_anchored = purrr::map(cor_anchored, ~ ifelse(any(is.na(.)),
                                                         "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                         list(eigen(.)$values))),
             PCA_anchored = purrr::map(cor_anchored, ~ ifelse(any(is.na(.)),
                                                       "NA in residual correlation matrix for start model. Unable to calculate eigenvalues and eigenvectors. Consider changing your testlets.",
                                                       list(eigen(.)$vectors[,1]))),
             WLE_anchored = purrr::map(mod_anchored, ~ TAM::tam.wle(., progress = FALSE)),
             EAP_anchored = purrr::map(mod_anchored, ~ .$EAP.rel[1])
      )
  }
  
  return(df_nest)
  
  
}

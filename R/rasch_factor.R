#' Calculate a factor analysis for a Rasch Model
#'
#' @param path_output a string with the path to the output folder. Default is NULL.
#' @inheritParams rasch_mds
#' 
#' @return a named list with results from the factor analysis for a Rasch Model:
#' \item{cor_poly}{the matrix of polychoric correlations}
#' \item{eigenvalues}{the eigenvalues}
#' \item{parallel_analysis}{permutation parallel analysis distribution}
#' \item{results_scree}{results of a scree analysis}
#' \item{n_group_factors}{number of factors from the parallel analysis in the scree analysis}
#' \item{fa_onefactor}{results from factor analysis with one factor}
#' \item{fa_resid}{local dependency based on polychoric correlations of the items}
#' 
#' @details Unidimensionality of the data is one of the core assumptions of the Rasch Model. This function performs the factor analysis to assess the unidimensionality of the data.
#' 
#' @family rasch functions
#' 
#' @export
#' 
#' @importFrom GPArotation GPForth
#' @importFrom psych fa factor.residuals vgQ.bimin
rasch_factor <- function(df, vars_metric, print_results = FALSE, path_output = NULL) {
  #----------------------------
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  # create data frame with ordered factors
  df_ordered <- df %>% 
    dplyr::select(all_of(vars_metric)) %>% 
    dplyr::mutate_all(list(~ ordered(.))) %>% 
    as.data.frame()
  
  df_numeric <- df %>% 
    dplyr::select(all_of(vars_metric)) %>% 
    as.data.frame()
  
  
  # calculate polychoric correlations
  cor_poly <- polycor::hetcor(df_ordered, use ="pairwise.complete.obs", ML = FALSE, std.err=FALSE)
  
  
  #----------------------------
  # permuted parallel analysis to test the unidimensionality
  eigenvalues <- nFactors::eigenComputes(x=df_numeric, use="pairwise.complete.obs")
  
  # Permutation parallel analysis distribution
  parallel_analysis <- nFactors::eigenBootParallel(x=df_numeric, quantile=0.95, nboot=30, option="permutation",
                                         cor=TRUE, model="components", use="pairwise.complete.obs")$quantile
  
  # number of components to retain
  results_scree <- nFactors::nScree(x = eigenvalues, aparallel = parallel_analysis)
  n_group_factors <- results_scree$Components$nparallel
  
  #---------------------------- 
  # bi-factor analysis to test the unidimensionality
  fa_bifactor <- try(psych::fa(cor_poly$correlations,n_group_factors+1,rotate="bifactor"), silent=TRUE)    # bi-factor model
  fa_onefactor <- psych::fa(cor_poly$correlations,1,rotate="bifactor")                   # single factor model
  
  if (any(inherits(fa_bifactor, "try-error"))) message("Bi-factor model unable to be computed") 
  #------------------------------------
  # local dependency based on polychoric correlations of the items
  fa_resid <- psych::factor.residuals(cor_poly$correlations,fa_onefactor)
  
  
  
  # PRINT RESULTS
  if (print_results) {
    
    if (is.null(path_output)) stop("You need to give an path for the output")
    
    # polychoric correlations
    save(cor_poly, file = paste0(path_output,"/cor_poly.RData"))
    utils::write.csv(round(cor_poly$correlations, 3), file=paste0(path_output,"/cor_poly.csv"))
    
    # scree plot
    grDevices::pdf(file=paste0(path_output,"/parallel_analysis_scree.pdf"), width=7, height=7)
    nFactors::plotnScree(results_scree)
    grDevices::dev.off()
    
    #bi-factor plot
    if (!any(inherits(fa_bifactor, "try-error"))) { #if fa_bifactor was able to be computed
      
      #create vector of possible colors
      col_factors <- RColorBrewer::brewer.pal(ncol(fa_bifactor$loadings),"Spectral")
      
      #create pdf of bifactor analysis
      grDevices::pdf(file=paste0(path_output,"/bifactor_analysis.pdf"), width=7, height=7)
      # par(col="black", mar=c(13, 4, 4, 2) + 0.1)
      
      graphics::plot(fa_bifactor$loadings[,1], type="l", ylim=c(-0.5,1), lwd=1.5, col="black", xaxt="n", xlab="", ylab="Loadings" )
      graphics::axis(side=1, at = 1:length(vars_metric), labels = vars_metric, las=2)
      
      for(i in 2:ncol(fa_bifactor$loadings)){
        graphics::lines(fa_bifactor$loadings[,i], col=col_factors[i], lwd=1.3)
      }
      
      graphics::lines(fa_onefactor$loadings[,1], col="black", lty="dotted", lwd=1.5)    
      
      grDevices::dev.off()
      
      utils::write.csv(cbind(fa_bifactor$loadings, fa_onefactor$loadings), file=paste0(path_output,"/bifactor_loadings.csv"))
      
      
    } else { #if fa_bifactor was not able to be computed
      utils::write.csv(unclass(fa_onefactor$loadings), file=paste0(path_output,"/bifactor_loadings.csv"))
    }
    
    # local dependency based on polychoric correlations of the items
    utils::write.csv(round(fa_resid,3), file=paste0(path_output,"/fa_resid.csv"))
    
  }
  
  
  factor_result <- list(cor_poly = cor_poly,
                        eigenvalues = eigenvalues, 
                        parallel_analysis = parallel_analysis,
                        results_scree = results_scree,
                        n_group_factors = n_group_factors,
                        fa_onefactor = fa_onefactor,
                        fa_resid = fa_resid)
  
  return(factor_result)
  
}
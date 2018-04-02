#' Calculate a factor analysis for a Rasch Model
#'
#' @param df a tibble of individual survey data, where each row is an individual 
#' @param vars_metric a character vector of items to use in the Rasch Analysis
#' @param print_results a logical vector indicating whether to print the results of the model to the \code{model_name} directory
#'
#' @return a list with results from the factor analysis for a Rasch Model
#' @export
#' 
#' @import dplyr
rasch_factor <- function(df, vars_metric, print_results = TRUE) {
  #----------------------------
  # create data set with ordered factors
  df_ordered <- df %>% select(vars_metric)
  
  df_ordered <- df_ordered %>% 
    mutate_all(funs(ordered))
  
  
  # calculate polychoric correlations
  cor_poly <- polycor::hetcor(df_ordered[,vars_metric], use ="pairwise.complete.obs", ML = FALSE, std.err=FALSE)
  
  
  #----------------------------
  # permuted parallel analysis to test the unidimensionality
  eigenvalues <- nFactors::eigenComputes(x=df[,vars_metric], use="pairwise.complete.obs")
  
  # Permutation parallel analysis distribution
  parallel_analysis <- nFactors::eigenBootParallel(x=df[,vars_metric], quantile=0.95, nboot=30, option="permutation",
                                         cor=TRUE, model="components", use="pairwise.complete.obs")$quantile
  
  # number of components to retain
  results_scree <- nFactors::nScree(x = eigenvalues, aparallel = parallel_analysis)
  n_group_factors <- results_scree$Components$nparallel
  
  #---------------------------- 
  # bi-factor analysis to test the unidimensionality
  fa_bifactor <- try(psych::fa(cor_poly$correlations,n_group_factors+1,rotate="bifactor"), silent=TRUE)    # bi-factor model
  fa_onefactor <- psych::fa(cor_poly$correlations,1,rotate="bifactor")                   # single factor model
  
  if (any(class(fa_bifactor)=="try-error")) message("Bi-factor model unable to be computed--it is likely there are not 2 factors") 
  
  #------------------------------------
  # local dependency based on polychoric correlations of the items
  fa_resid <- psych::factor.residuals(cor_poly$correlations,fa_onefactor)
  
  
  
  # PRINT RESULTS
  if (print_results) {
    
    # polychoric correlations
    save(cor_poly, file = "cor_poly.RData")
    utils::write.csv(round(cor_poly$correlations, 3), file="cor_poly.csv")
    
    # scree plot
    grDevices::pdf(file="parallel_analysis_scree.pdf", width=7, height=7)
    nFactors::plotnScree(results_scree)
    grDevices::dev.off()
    
    #bi-factor plot
    if (!any(class(fa_bifactor)=="try-error")) { #if fa_bifactor was able to be computed
      
      #create vector of possible colors
      col_factors <- RColorBrewer::brewer.pal(ncol(fa_bifactor$loadings),"Spectral")
      
      #create pdf of bifactor analysis
      grDevices::pdf(file="bifactor_analysis.pdf", width=7, height=7)
      # par(col="black", mar=c(13, 4, 4, 2) + 0.1)
      
      graphics::plot(fa_bifactor$loadings[,1], type="l", ylim=c(-0.5,1), lwd=1.5, col="black", xaxt="n", xlab="", ylab="Loadings" )
      graphics::axis(side=1, at = 1:length(vars_metric), labels = vars_metric, las=2)
      
      for(i in 2:ncol(fa_bifactor$loadings)){
        graphics::lines(fa_bifactor$loadings[,i], col=col_factors[i], lwd=1.3)
      }
      
      graphics::lines(fa_onefactor$loadings[,1], col="black", lty="dotted", lwd=1.5)    
      
      grDevices::dev.off()
      
      utils::write.csv(cbind(fa_bifactor$loadings, fa_onefactor$loadings), file="bifactor_loadings.csv")
      
      
    } else { #if fa_bifactor was not able to be computed
      utils::write.csv(unclass(fa_onefactor$loadings), file="bifactor_loadings.csv")
    }
    
    # local dependency based on polychoric correlations of the items
    utils::write.csv(round(fa_resid,3), file="fa_resid.csv")
    
  }
  
  
  factor_result <- list()
  
  return(factor_result)
  
}
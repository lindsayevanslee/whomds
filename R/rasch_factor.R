rasch_factor <- function(df, vars_metric, print_results = TRUE) {
  #----------------------------
  # create data set with ordered factors
  df_ordered <- df %>% select(vars_metric)
  
  df_ordered <- df_ordered %>% 
    mutate_all(funs(ordered))
  
  
  # calculate polychoric correlations
  cor_poly <- hetcor(df_ordered[,vars_metric], use ="pairwise.complete.obs", ML = FALSE, std.err=FALSE)
  
  
  #----------------------------
  # permuted parallel analysis to test the unidimensionality
  eigenvalues <- eigenComputes(x=df[,vars_metric], use="pairwise.complete.obs")
  
  # Permutation parallel analysis distribution
  parallel_analysis <- eigenBootParallel(x=df[,vars_metric], quantile=0.95, nboot=30, option="permutation",
                                         cor=TRUE, model="components", use="pairwise.complete.obs")$quantile
  
  # number of components to retain
  results_scree <- nScree(x = eigenvalues, aparallel = parallel_analysis)
  n_group_factors <- results_scree$Components$nparallel
  
  #---------------------------- 
  # bi-factor analysis to test the unidimensionality
  fa_bifactor <- try(fa(cor_poly$correlations,n_group_factors+1,rotate="bifactor"), silent=TRUE)    # bi-factor model
  fa_onefactor <- fa(cor_poly$correlations,1,rotate="bifactor")                   # single factor model
  
  if (any(class(fa_bifactor)=="try-error")) message("Bi-factor model unable to be computed--it is likely there are not 2 factors") 
  
  #------------------------------------
  # local dependency based on polychoric correlations of the items
  fa_resid <- factor.residuals(cor_poly$correlations,fa_onefactor)
  
  
  
  # PRINT RESULTS
  if (print_results) {
    
    # polychoric correlations
    save(cor_poly, file = "cor_poly.RData")
    write.csv(round(cor_poly$correlations, 3), file="cor_poly.csv")
    
    # scree plot
    pdf(file="parallel_analysis_scree.pdf", width=7, height=7)
    plotnScree(results_scree)
    dev.off()
    
    #bi-factor plot
    if (!any(class(fa_bifactor)=="try-error")) { #if fa_bifactor was able to be computed
      
      #create vector of possible colors
      col_factors <- RColorBrewer::brewer.pal(ncol(fa_bifactor$loadings),"Spectral")
      
      #create pdf of bifactor analysis
      pdf(file="bifactor_analysis.pdf", width=7, height=7)
      # par(col="black", mar=c(13, 4, 4, 2) + 0.1)
      
      plot(fa_bifactor$loadings[,1], type="l", ylim=c(-0.5,1), lwd=1.5, col="black", xaxt="n", xlab="", ylab="Loadings" )
      axis(side=1, at = 1:length(vars_metric), labels = vars_metric, las=2)
      
      for(i in 2:ncol(fa_bifactor$loadings)){
        lines(fa_bifactor$loadings[,i], col=col_factors[i], lwd=1.3)
      }
      
      lines(fa_onefactor$loadings[,1], col="black", lty="dotted", lwd=1.5)    
      
      dev.off()
      
      write.csv(cbind(fa_bifactor$loadings, fa_onefactor$loadings), file="bifactor_loadings.csv")
      
      
    } else { #if fa_bifactor was not able to be computed
      write.csv(unclass(fa_onefactor$loadings), file="bifactor_loadings.csv")
    }
    
    # local dependency based on polychoric correlations of the items
    write.csv(round(fa_resid,3), file="fa_resid.csv")
    
  }
  
  
}
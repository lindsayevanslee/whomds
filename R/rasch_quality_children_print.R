 rasch_quality_children_print <- function(df_nest, vars_metric, vars_age_group, TAM_model, LID_cutoff, path_output) {
  
  for (r in 1:nrow(df_nest)) {
    age_name <- df_nest %>% 
      pull(vars_age_group) %>% 
      .[r]
    
    ### Print multigroup results (only need to do once because should all be identical) ----------
    if (r==1) {
      #print scree plot multigroup
      pdf(paste0(path_output,"ScreePlot_Multigroup_",TAM_model,".pdf"))
      try(plotnScree(nScree(x = df_nest$eigen_multigroup)))
      dev.off()
      
      capture.output(summary(df_nest$mod_multigroup[[r]]), file = paste0(path_output,TAM_model,"_summary_multigroup.txt"))
      write.xlsx(df_nest$itemfit_multigroup[[r]], file = paste0(path_output,TAM_model,"_itemfit.xlsx"), row.names = TRUE, keepNA = TRUE)
      write.xlsx(df_nest$cor_multigroup[[r]], file = paste0(path_output,TAM_model,"_residual_correlation.xlsx"), row.names = TRUE, keepNA = TRUE)
      write.xlsx(df_nest$xsithresh_multigroup[[r]], file = paste0(path_output,TAM_model,"_xsi_thresholds.xlsx"), row.names = TRUE, keepNA = TRUE)
      write.xlsx(df_nest$eigen_multigroup[[r]], file = paste0(path_output,TAM_model,"_eigenvalues.xlsx"), row.names = TRUE, keepNA = TRUE)
      write.xlsx(df_nest$PCA_multigroup[[r]], file = paste0(path_output,TAM_model,"_first_vector_loadings.xlsx"), row.names = TRUE, keepNA = TRUE)
      capture.output(df_nest$WLE_multigroup[[r]],file = paste0(path_output,TAM_model,"_WLE.txt"))
      write.xlsx(df_nest$EAP_multigroup[[r]], file = paste0(path_output,TAM_model,"_EAP_reliability.xlsx"), row.names = TRUE, keepNA = TRUE)
      
      #save Thurstonian thresholds (always ordered) and save
      write.csv(df_nest$tthresh_multigroup[[r]],
                paste0(path_output, "ThurstonianThresholds.csv"),
                row.names = TRUE)
      
      
      #generate matrix for color of symbols
      color_matrix <-
        matrix(rep(helper_palette(ncol(df_nest$tthresh_multigroup[[r]])), nrow(df_nest$tthresh_multigroup[[r]])),
               nrow(df_nest$tthresh_multigroup[[r]]),
               ncol(df_nest$tthresh_multigroup[[r]]),
               byrow = TRUE)
      
      #print Wright Map
      pdf(paste0(path_output,"WrightMap.pdf"))
      wrightMap(
        df_nest$WLE_multigroup[[r]]$theta,
        df_nest$tthresh_multigroup[[r]][order(df_nest$mod_multigroup[[r]]$item$xsi.item),],
        label.items.srt = 45,
        show.thr.lab = FALSE,
        thr.sym.col.fg = color_matrix,
        thr.sym.col.bg = color_matrix,
        thr.sym.cex = 1.5,
        breaks=50
      )
      dev.off()
      
      cor_matrix <- 
      
      #create dependency graph
      fig_LID(LIDforgraph = df_nest$cor_multigroup[[r]], 
              LID_cutoff = 0.2, 
              path_output = path_output,
              extra_file_label = "Multigroup")
      
      
    }
    
    
    ### Print Anchored results --------
    
    
    #print scree plot start
    pdf(paste0(path_output,"ScreePlot_Start_",TAM_model,"_",age_name,".pdf"))
    try(plotnScree(nScree(x = df_nest$eigen_start[[r]])))
    dev.off()
    
    #print scree plot anchored
    pdf(paste0(path_output,"ScreePlot_Anchored_",TAM_model,"_",age_name,".pdf"))
    try(plotnScree(nScree(x = df_nest$eigen_anchored[[r]])))
    dev.off()
    
    capture.output(summary(df_nest$mod_anchored[[r]]), file = paste0(path_output,TAM_model,"_summary_anchored_",age_name,".txt"))
    
    #save Thurstonian thresholds (always ordered)
    write.csv(df_nest$tthresh_anchored[[i]],
              paste0(path_output, "ThurstonianThresholds_",age_name, ".csv"),
              row.names = TRUE)
    
    
    #generate matrix for color of symbols
    color_matrix <-
      matrix(rep(helper_palette(ncol(df_nest$tthresh_anchored[[i]])), nrow(df_nest$tthresh_anchored[[i]])),
             nrow(df_nest$tthresh_anchored[[i]]),
             ncol(df_nest$tthresh_anchored[[i]]),
             byrow = TRUE)
    
    #print Wright Map
    pdf(paste0(path_output,"WrightMap_",age_name,".pdf"))
    wrightMap(
      df_nest$WLE_anchored[[r]][[i]]$theta,
      df_nest$tthresh_anchored[[i]][order(df_nest$mod_anchored[[i]]$item$xsi.item),],
      label.items.srt = 45,
      show.thr.lab = FALSE,
      thr.sym.col.fg = color_matrix,
      thr.sym.col.bg = color_matrix,
      thr.sym.cex = 1.5,
      breaks=50
    )
    dev.off()
    
    #create dependency graph
    fig_LID(LIDforgraph = df_nest$cor_anchored[[r]], 
            LID_cutoff = 0.2, 
            path_output = path_output,
            extra_file_label = age_name,
            vertex_print_grey = vars_metric[["common"]])
    
  }
  
  
  write.xlsx(df_nest$itemfit_anchored, file = paste0(path_output,TAM_model,"_itemfit.xlsx"), row.names = TRUE, keepNA = TRUE)
  write.xlsx(df_nest$cor_anchored, file = paste0(path_output,TAM_model,"_residual_correlation.xlsx"), row.names = TRUE, keepNA = TRUE)
  write.xlsx(df_nest$xsithresh_anchored, file = paste0(path_output,TAM_model,"_xsi_thresholds.xlsx"), row.names = TRUE, keepNA = TRUE)
  write.xlsx(df_nest$eigen_anchored, file = paste0(path_output,TAM_model,"_eigenvalues.xlsx"), row.names = TRUE, keepNA = TRUE)
  write.xlsx(df_nest$PCA_anchored, file = paste0(path_output,TAM_model,"_first_vector_loadings.xlsx"), row.names = TRUE, keepNA = TRUE)
  capture.output(df_nest$WLE_anchored,file = paste0(path_output,TAM_model,"_WLE.txt"))
  write.xlsx(df_nest$EAP_anchored, file = paste0(path_output,TAM_model,"_EAP_reliability.xlsx"), row.names = TRUE, keepNA = TRUE)
  
}
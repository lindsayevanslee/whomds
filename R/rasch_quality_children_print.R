#' Print results of analysis of Rasch Model quality
#'
#' @inheritParams rasch_mds
#' @inheritParams rasch_mds_children
#' @inheritParams rasch_model
#' @inheritParams rasch_model_children
#' @inheritParams rasch_factor
#'
#' @return does not return anything to the environment, but prints files to the folder specified in \code{path_output}
#' @export
#'
#' @family rasch functions
#' @family children analysis functions
#' 
rasch_quality_children_print <- function(df_nest, vars_metric, vars_group, TAM_model, LIDcutoff = 0.2, path_output) {
  
  for (r in 1:nrow(df_nest)) {
    age_name <- df_nest %>% 
      dplyr::pull(vars_group) %>% 
      .[r]
    
    ### Print multigroup results (only need to do once because should all be identical) ----------
    if (r==1) {
      #print scree plot multigroup
      if (is.numeric(unlist(df_nest$eigen_multigroup[[r]]))) {
        grDevices::pdf(paste0(path_output,"/ScreePlot_Multigroup_",TAM_model,".pdf"))
        nFactors::plotnScree(nFactors::nScree(x = unlist(df_nest$eigen_multigroup[[r]])))
        grDevices::dev.off()
      }
      
      utils::capture.output(summary(df_nest$mod_multigroup[[r]]), file = paste0(path_output, "/", TAM_model,"_summary_multigroup.txt"))
      openxlsx::write.xlsx(df_nest$itemfit_multigroup[[r]], file = paste0(path_output, "/", TAM_model,"_itemfit_multigroup.xlsx"), row.names = TRUE, keepNA = TRUE)
      openxlsx::write.xlsx(df_nest$cor_multigroup[[r]], file = paste0(path_output, "/", TAM_model,"_residual_correlation_multigroup.xlsx"), row.names = TRUE, keepNA = TRUE)
      openxlsx::write.xlsx(df_nest$xsithresh_multigroup[[r]], file = paste0(path_output, "/", TAM_model,"_xsi_thresholds_multigroup.xlsx"), row.names = TRUE, keepNA = TRUE)
      openxlsx::write.xlsx(df_nest$eigen_multigroup[[r]], file = paste0(path_output, "/", TAM_model,"_eigenvalues_multigroup.xlsx"), row.names = TRUE, keepNA = TRUE)
      openxlsx::write.xlsx(df_nest$PCA_multigroup[[r]], file = paste0(path_output, "/", TAM_model,"_first_vector_loadings_multigroup.xlsx"), row.names = TRUE, keepNA = TRUE)
      utils::capture.output(df_nest$WLE_multigroup[[r]],file = paste0(path_output, "/", TAM_model,"_WLE_multigroup.txt"))
      openxlsx::write.xlsx(df_nest$EAP_multigroup[[r]], file = paste0(path_output, "/", TAM_model,"_EAP_reliability_multigroup.xlsx"), row.names = TRUE, keepNA = TRUE)
      
      #save Thurstonian thresholds (always ordered) and save
      utils::write.csv(df_nest$tthresh_multigroup[[r]],
                paste0(path_output, "/ThurstonianThresholds_multigroup.csv"),
                row.names = TRUE)
      
      
      #generate matrix for color of symbols
      color_matrix <-
        matrix(rep(helper_palette(ncol(df_nest$tthresh_multigroup[[r]])), nrow(df_nest$tthresh_multigroup[[r]])),
               nrow(df_nest$tthresh_multigroup[[r]]),
               ncol(df_nest$tthresh_multigroup[[r]]),
               byrow = TRUE)
      
      #print Wright Map
      grDevices::pdf(paste0(path_output,"/WrightMap_multigroup.pdf"))
      WrightMap::wrightMap(
        df_nest$WLE_multigroup[[r]]$theta,
        df_nest$tthresh_multigroup[[r]][order(df_nest$mod_multigroup[[r]]$item$xsi.item),],
        label.items.srt = 45,
        show.thr.lab = FALSE,
        thr.sym.col.fg = color_matrix,
        thr.sym.col.bg = color_matrix,
        thr.sym.cex = 1.5,
        breaks=50
      )
      grDevices::dev.off()
      
      
      #create dependency graph
      grDevices::pdf(paste0(path_output,"/LID_plot_Multigroup.pdf"))
      print(fig_LID(LIDforgraph = df_nest$cor_multigroup[[r]], 
              LIDcutoff = LIDcutoff, 
              path_output = path_output,
              extra_file_label = "Multigroup"))
      grDevices::dev.off()
      
      
    }
    
    #print scree plot start
    if (is.numeric(unlist(df_nest$eigen_start[[r]]))) {
      grDevices::pdf(paste0(path_output,"/ScreePlot_Start_",TAM_model,"_",age_name,".pdf"))
      nFactors::plotnScree(nFactors::nScree(x = unlist(df_nest$eigen_start[[r]])))
      grDevices::dev.off()
    }
    
    ### Print Anchored results --------
    if (length(vars_metric) > 1) {
      
      #print scree plot anchored
      if (is.numeric(unlist(df_nest$eigen_anchored[[r]]))) {
        grDevices::pdf(paste0(path_output,"/ScreePlot_Anchored_",TAM_model,"_",age_name,".pdf"))
        nFactors::plotnScree(nFactors::nScree(x = unlist(df_nest$eigen_anchored[[r]])))
        grDevices::dev.off()
      }
      
      utils::capture.output(summary(df_nest$mod_anchored[[r]]), file = paste0(path_output, "/", TAM_model,"_summary_anchored_",age_name,".txt"))
      
      #save Thurstonian thresholds (always ordered)
      utils::write.csv(df_nest$tthresh_anchored[[r]],
                paste0(path_output, "/ThurstonianThresholds_anchored",age_name, ".csv"),
                row.names = TRUE)
      
      
      #generate matrix for color of symbols
      color_matrix <-
        matrix(rep(helper_palette(ncol(df_nest$tthresh_anchored[[r]])), nrow(df_nest$tthresh_anchored[[r]])),
               nrow(df_nest$tthresh_anchored[[r]]),
               ncol(df_nest$tthresh_anchored[[r]]),
               byrow = TRUE)
      
      #print Wright Map
      grDevices::pdf(paste0(path_output,"/WrightMap_anchored_",age_name,".pdf"))
      WrightMap::wrightMap(
        df_nest$WLE_anchored[[r]]$theta,
        df_nest$tthresh_anchored[[r]][order(df_nest$mod_anchored[[r]]$item$xsi.item),],
        label.items.srt = 45,
        show.thr.lab = FALSE,
        thr.sym.col.fg = color_matrix,
        thr.sym.col.bg = color_matrix,
        thr.sym.cex = 1.5,
        breaks=50
      )
      grDevices::dev.off()
      
      #create dependency graph
      grDevices::pdf(paste0(path_output,"/LID_plot_",age_name,".pdf"))
      print(fig_LID(LIDforgraph = df_nest$cor_anchored[[r]], 
              LIDcutoff = LIDcutoff, 
              path_output = path_output,
              extra_file_label = age_name,
              vertex_print_grey = vars_metric[["common"]]))
      grDevices::dev.off()
      
    }
  }
  
  if (length(vars_metric) > 1) {
    
    openxlsx::write.xlsx(df_nest$itemfit_anchored, file = paste0(path_output, "/", TAM_model,"_itemfit_anchored.xlsx"), row.names = TRUE, keepNA = TRUE)
    openxlsx::write.xlsx(df_nest$cor_anchored, file = paste0(path_output, "/", TAM_model,"_residual_correlation_anchored.xlsx"), row.names = TRUE, keepNA = TRUE)
    openxlsx::write.xlsx(df_nest$xsithresh_anchored, file = paste0(path_output, "/", TAM_model,"_xsi_thresholds_anchored.xlsx"), row.names = TRUE, keepNA = TRUE)
    openxlsx::write.xlsx(df_nest$eigen_anchored, file = paste0(path_output, "/", TAM_model,"_eigenvalues_anchored.xlsx"), row.names = TRUE, keepNA = TRUE)
    openxlsx::write.xlsx(df_nest$PCA_anchored, file = paste0(path_output, "/", TAM_model,"_first_vector_loadings_anchored.xlsx"), row.names = TRUE, keepNA = TRUE)
    utils::capture.output(df_nest$WLE_anchored,file = paste0(path_output, "/", TAM_model,"_WLE_anchored.txt"))
    openxlsx::write.xlsx(df_nest$EAP_anchored, file = paste0(path_output, "/", TAM_model,"_EAP_reliability_anchored.xlsx"), row.names = TRUE, keepNA = TRUE)
  }
}
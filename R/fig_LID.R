#' Print a graph showing significant correlations between survey items
#'
#' @param LIDforgraph a square matrix of item correlations
#' @param LIDcutoff a numeric value between 0 and 1 for the cut-off for signicant correlation
#' @param path_output a string with the path to the output folder
#' @param extra_file_label a string to tack on to the end of names of files outputted and the title of the plot. Default is NULL.
#' @param vertex_print_grey a character vector with the names of vertices to print in \code{"lightgrey"}, with all others printed in \code{"skyblue"}. If left as default NULL, all vertices will be printed in \code{"lightgrey"}.
#'
#' @return Prints a pdf graph if the items with correlation > \code{LIDcutoff} and a csv of the corresponding correlations
#' 
#' @details This function could be applied to visualize any kind of correlations. But within the context of the Rasch Analysis used for the WHO Model Disability Survey, the  residual correlations are used when analyzing item dependence.
#' @export
#' 
#' @family figure functions
fig_LID <- function(LIDforgraph, LIDcutoff = 0.2, path_output, extra_file_label = NULL, vertex_print_grey = NULL) {
  
  
  #create dependency graph
  fullgraph <- igraph::graph_from_adjacency_matrix((LIDforgraph >= LIDcutoff)*1,diag=FALSE,mode="upper")
  
  comp.no <- which(igraph::components(fullgraph)$csize>1)
  igraph::V(fullgraph)$comp <- igraph::components(fullgraph)$membership
  
  finalgraph <- igraph::induced_subgraph(fullgraph,igraph::V(fullgraph)$comp %in% comp.no)
  
  #save file names
  if (is.null(extra_file_label)) {
    plot_file_label <- paste0(path_output,"/LID_plot.pdf")
    spreadsheet_file_label <- paste0(path_output,"/LID_above_", LIDcutoff,".csv")
    title_label <- "Local Dependendencies"
  } else {
    plot_file_label <- paste0(path_output,"/LID_plot_", extra_file_label,".pdf")
    spreadsheet_file_label <- paste0(path_output,"/LID_above_", LIDcutoff, "_", extra_file_label,".csv")
    title_label <- paste("Local Dependendencies", extra_file_label)
  }
  
  #save vector for colors
  if (is.null(vertex_print_grey)) vertex_color <- "lightgrey"
  else vertex_color <- c("skyblue","lightgrey")[1+igraph::V(finalgraph)$name %in% vertex_print_grey]
  
  #print plot
  if (igraph::components(finalgraph)$no==0) {
    grDevices::pdf(plot_file_label)
    graphics::plot(0,0, col="white", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
    graphics::text(-0.25,0, "No LID found!", col="blue", cex=1.4)
    grDevices::dev.off()
  } else {
    grDevices::pdf(plot_file_label)
    graphics::plot(finalgraph,
                   layout=igraph::layout.kamada.kawai, 
                   vertex.label=igraph::V(finalgraph)$name, 
                   vertex.size=25, 
                   main= title_label,
                   vertex.color=vertex_color, 
                   vertex.label.color="black", 
                   edge.color="black")
    grDevices::dev.off()
  }
  
  #print spreadsheet with only correlations above cut-off
  LIDforgraph[is.na(LIDforgraph)] <- 0
  LIDforgraph[which(LIDforgraph < LIDcutoff)] <- 0
  
  utils::write.csv(LIDforgraph, spreadsheet_file_label, row.names=TRUE)
  
}
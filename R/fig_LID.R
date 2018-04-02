#' Print a graph showing significant correlations between survey items
#'
#' @param LIDforgraph a square matrix of item correlations
#' @param LIDcutoff a numeric value between 0 and 1 for the cut-off for signicant correlation
#' @param path_output a string with the path to the output folder
#'
#' @return Prints a pdf graph if the items with correlation > \code{LIDcutoff} and a csv of the corresponding correlations
#' @export
fig_LID <- function(LIDforgraph, LIDcutoff = 0.2, path_output) {

  
  #create dependency graph
  fullgraph <- igraph::graph_from_adjacency_matrix((LIDforgraph >= LIDcutoff)*1,diag=FALSE,mode="upper")
  
  comp.no <- which(igraph::components(fullgraph)$csize>1)
  igraph::V(fullgraph)$comp <- igraph::components(fullgraph)$membership
  
  finalgraph <- igraph::induced_subgraph(fullgraph,igraph::V(fullgraph)$comp %in% comp.no)
  
  if (igraph::components(finalgraph)$no==0) {
    grDevices::pdf(paste0(path_output,"/LID_plot.pdf"))
    graphics::plot(0,0, col="white", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
    graphics::text(-0.25,0, "No LID found!", col="blue", cex=1.4)
    grDevices::dev.off()
  } else {
    grDevices::pdf("LID_plot.pdf")
    graphics::plot(finalgraph,layout=layout.kamada.kawai, vertex.label=igraph::V(finalgraph)$name, vertex.size=25, main="Local Dependendencies",
         vertex.color="lightgrey", vertex.label.color="black", edge.color="black")
    grDevices::dev.off()
  }
  
  #print spreadsheet with only correlations above cut-off
  LIDforgraph[is.na(LIDforgraph)] <- 0
  LIDforgraph[which(LIDforgraph < LIDcutoff)] <- 0
  
  utils::write.csv(LIDforgraph, paste0(path_output,"/LID_above_", LIDcutoff,".csv"), row.names=TRUE)
  
}
#' Print a graph showing significant correlations between survey items
#'
#' @param LIDforgraph a square matrix of item correlations
#' @param LIDcutoff a numeric value between 0 and 1 for the cut-off for signicant correlation
#'
#' @return Prints a pdf graph if the items with correlation > \code{LIDcutoff} and a csv of the corresponding correlations
#' @export
fig_LID <- function(LIDforgraph, LIDcutoff = 0.2) {

  
  #create dependency graph
  fullgraph <- graph_from_adjacency_matrix((LIDforgraph >= LIDcutoff)*1,diag=FALSE,mode="upper")
  
  comp.no <- which(components(fullgraph)$csize>1)
  V(fullgraph)$comp <- components(fullgraph)$membership
  
  finalgraph <- induced_subgraph(fullgraph,V(fullgraph)$comp %in% comp.no)
  
  if (components(finalgraph)$no==0) {
    pdf("LID_plot.pdf")
    plot(0,0, col="white", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
    text(-0.25,0, "No LID found!", col="blue", cex=1.4)
    dev.off()
  } else {
    pdf("LID_plot.pdf")
    plot(finalgraph,layout=layout.kamada.kawai, vertex.label=V(finalgraph)$name, vertex.size=25, main="Local Dependendencies",
         vertex.color="lightgrey", vertex.label.color="black", edge.color="black")
    dev.off()
  }
  
  #print spreadsheet with only correlations above cut-off
  LIDforgraph[is.na(LIDforgraph)] <- 0
  LIDforgraph[which(LIDforgraph < LIDcutoff)] <- 0
  
  write.csv(LIDforgraph, paste0("LID_above_", LIDcutoff,".csv"), row.names=TRUE)
  
}
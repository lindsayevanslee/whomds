#' Plot a density of a score
#'
#' @param df a data frame where each row is an individual, containing at least a score column (between 0 and 100)
#' @param score a string (length 1) of the column name for the score variable to print the distribution of
#' @param var_color a string (length 1) of the column name for the variable to set color of density lines by. Default is NULL.
#' @param var_facet a string (length 1) of the column name for the variable to create a \code{ggplot2::facet_grid()} with. Default is NULL.
#' @param cutoffs a numeric vector of the cut-offs for the score categorization. Default is NULL.
#' @param x_lab a string (length 1) of x-axis label. Default is "Score".
#' @param pal a string specifying either a manual color to use for the color aesthetic, a character vector explictly specifying the colors to use for the color scale, or as the name of a palette to pass to \code{RColorBrewer::brewer.pal()} with the name of the color palette to use for the color scale. Default is \code{"Paired"}
#' @param adjust a numeric value to pass to \code{adjust} argument of \code{ggplot2::geom_density()}. Default is 2.
#' @param size a numeric value to pass to \code{size} argument of \code{ggplot2::geom_density()}. Default is 1.5.
#' 
#' @return A density figure
#' 
#' @details Plots a histogram of a score that ranges between 0 and 100, with the fill determined by some set categorization of the score. This is the function used to plot the distributions of disability scores resulting from the WHO Model Disability Survey.
#' 
#' @family figure functions
#' @export 
#' 
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' fig_density(df_adults, score = "disability_score", cutoffs = c(19.1, 34.4, 49.6), 
#' x_lab = "Disability score")
#' fig_density(df_adults, score = "disability_score", var_color = "sex", 
#' cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")
#' fig_density(df_adults, score = "disability_score", var_color = "sex", 
#' var_facet = "age_cat",  cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")
fig_density <- function(df, score, var_color = NULL, var_facet = NULL,
                        cutoffs = NULL, x_lab = "Score", 
                        pal = "Paired", adjust = 2, size = 1.5){
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  
  #Initialize plot
  if (is.null(var_color)) {
    plot_density <- ggplot(df, aes(x = !!rlang::sym(score))) 
    
  } else {
    
    if (!is.factor(pull(df, var_color))) stop("var_color must be a factor")
    
    plot_density <- ggplot(df, aes(x = !!rlang::sym(score), 
                                   color = !!rlang::sym(var_color))) 
  }
  
  #set colors
  if (length(pal) == 1) {
    if (pal %in% rownames(RColorBrewer::brewer.pal.info)) {
      #if length == 1 and pal in palettes -> brewer scale
      plot_density <- plot_density +
        geom_density(adjust = adjust, size = size) +
        scale_color_brewer(palette = pal)
    } else {
      #if length == 1 and pal not in palettes -> color aesthetic
      plot_density <- plot_density +
        geom_density(adjust = adjust, size = size, color = pal)
    }
  } else {
    #if length != 1 -> manual color scale
    plot_density <- plot_density +
      geom_density(adjust = adjust, size = size) +
      scale_color_manual(values = pal)
  }
  
  
  
  
  #Continue plot with formatting
  plot_density <- plot_density + 
    scale_x_continuous(limits = c(min(0, min(cutoffs)), 100)) +
    labs(x = "Score") +
    theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_blank(), # get rid of minor grid
      
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.title = element_blank(),
      legend.position="top",
      legend.box = "horizontal",
      legend.text = element_text(size = 12),
      
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      plot.margin = unit(c(1, 1, 3, 1), "lines"),
      
      axis.text.y = element_text(size = 12, colour = "black"),
      axis.text.x = element_text(size = 12, colour = "black", angle = 0, hjust = 0.5, vjust = 0.5),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14)
      
    ) 
  
  
  
  #add cutoffs lines if non-NULL
  if (!is.null(cutoffs)) {
    plot_density <- plot_density + 
      geom_vline(xintercept = cutoffs, color="black", linetype="dashed", size=.5)
  }
  
  #add facet grid if var_facet non-NULL
  if (!is.null(var_facet)) {
    plot_density <- plot_density +
      facet_grid(stats::formula(paste("~", var_facet)))
  }
  
  
  return(plot_density)
}

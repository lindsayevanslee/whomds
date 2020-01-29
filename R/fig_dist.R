#' Plot a distribution of a score
#'
#' @param df a data frame where each row is an individual, containing at least a score column (between 0 and 100) and a categorization of that score
#' @param score a string (length 1) of the column name for the score variable to print the distribution of
#' @param score_cat a string (length 1) of the column name for the categorization of the score variable
#' @param cutoffs a numeric vector of the cut-offs for the score categorization
#' @param x_lab a string (length 1) of x-axis label. Default is "Score".
#' @param y_max a numeric value of the maximum limit on the y-axis. Default is NULL to use default value from \code{geom_histogram()}
#' @param pcent a logical value determining whether or not to display the distribution as percentages or frequency. Default is FALSE, to display as frequency.
#' @param pal a string to pass to \code{RColorBrewer::brewer.pal()} with the name of the color palette to use
#' @param binwidth a numeric value giving the width of the bins in the histograph. Default is 5.
#' 
#' @return A score distribution figure with fill based on categorization of the score
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
#' fig_dist(df_adults, score = "disability_score", score_cat = "disability_cat", 
#' cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score")
#' fig_dist(df_adults, score = "disability_score", score_cat = "disability_cat", 
#' cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score", y_max = 2000)
#' fig_dist(df_adults, score = "disability_score", score_cat = "disability_cat", 
#' cutoffs = c(19.1, 34.4, 49.6), x_lab = "Disability score", y_max = 0.2, pcent=TRUE)
fig_dist <- function(df, score, score_cat, cutoffs,
                        x_lab = "Score", y_max = NULL, pcent=FALSE, pal = "Blues", binwidth = 5){
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  if (pcent) {
    y_lab <- "Percent"
    type <- "..count../sum(..count..)"
    
    # default y-axis values to try
    possible_scale <- c(0.01,0.05,0.1,0.2,0.25)
    final_scale <- 0.05
    lab <- scales::percent_format()
    
  } else {
    y_lab <- "Frequency"
    type <- "..count.."

    # default y-axis values to try
    possible_scale <- c(200,250,300,500,1000)
    final_scale <- 100
    lab <- waiver()
    
  }
  
  fill_colors <- RColorBrewer::brewer.pal(length(levels(pull(df,score_cat))),pal)

  
  #Begin plot
  plot_dist <- ggplot(df, aes_string(x = score, fill = score_cat)) +
    geom_histogram(position = "identity", binwidth = binwidth, aes_string(y=type), col="black") +
    scale_fill_manual(values = fill_colors, drop=FALSE)  +
    scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(min(-2.5,min(cutoffs)),102.5)) +
    # scale_y_continuous(breaks = seq(0, y_max, by = final_scale), limits=c(NA,y_max), labels=lab) +
    labs(x = x_lab, y = y_lab) +
    geom_vline(xintercept = cutoffs, color="black", linetype="dashed", size=1) +
    theme(plot.margin = unit(c(1, 1, 3, 1), "lines"),
          legend.title=element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          panel.background = element_rect(fill = "white"),
          legend.position="top",
          legend.box = "horizontal",
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.text.x = element_text(size = 12, colour = "black"),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          legend.text = element_text(size = 12)) 
  
  
  if (!is.null(y_max)) {
    
    # Calculate a good y-axis scale to use
    divis <- possible_scale[y_max %% possible_scale == 0]
    cond <- y_max/divis > 5 & y_max/divis < 16
    
    if (any(cond)) {
      final_possible_scale <- divis[cond]
      final_scale <- utils::tail(final_possible_scale,1)
    }
    
    plot_dist <- plot_dist +
      scale_y_continuous(breaks = seq(0, y_max, by = final_scale), limits=c(NA,y_max), labels=lab)
    
  } else {
    plot_dist <- plot_dist +
      scale_y_continuous(labels=lab)
  }
  
  
  return(plot_dist)
}

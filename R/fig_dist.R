################## Arguments of function DistPlotFun #########################
# - data: data frame name                                                    #
# - score_abs: character variable of score variable name                     #
#              ranging from 0 to 100; ex. "CapacityScore"                    #
# - score_cat: character variable of score categorization variable name,     #
#              ex. "capacity_cat"                                            #
# - cat_mean: numeric variable for mean of score variable in score_abs       #
# - cat_sd: numeric variable for sd of score variable in score_abs           #
# - score_type: character variable of score in score_abs in plain language;  #
#               ex. "capacity_cat"                                           # 
# - y_max: numeric variable of upper limit on y-axis                         #
# - pcent: logical variable indicating whether or not to use Percent, Default is FALSE #
##############################################################################


DistPlotFun <- function(data, score_abs, score_cat, cat_mean, cat_sd,
                        score_type,y_max,pcent=FALSE){
  
  if (grepl("Capacity",score_type)){pal <- "Greens"}
  else if (grepl("Performance",score_type) | grepl("Disability",score_type)) {pal <- "Blues"}
  else {pal <- "Reds"}
  
  if (pcent) {
    y.lab <- "Percent"
    type <- "..count../sum(..count..)"
    
    # default y-axis values to try
    pos.scale <- c(0.01,0.05,0.1,0.2,0.25)
    final.scale <- 0.05
    lab <- percent_format()
    
  } else {
    y.lab <- "Frequency"
    type <- "..count.."

    # default y-axis values to try
    pos.scale <- c(200,250,300,500,1000)
    final.scale <- 100
    lab <- waiver()
    
  }
  
  
  # Calculate a good y-axis scale to use
  divis <- pos.scale[y_max %% pos.scale == 0]
  cond <- y_max/divis > 5 & y_max/divis < 16
  
  if (any(cond)) {
    final.pos.scale <- divis[cond]
    final.scale <- tail(final.pos.scale,1)
  }
  
  #Begin plot
  plot.cap <- ggplot(data, aes_string(x = score_abs,
                                      fill = score_cat)) +
                     # aes(x = data[, score_abs],
                     #                  color = data[, score_cat],
                     #                  fill = data[, score_cat])) +

    geom_histogram(position = "identity", binwidth = 5, aes_string(y=type), col="black") +
    
    scale_fill_manual(values = brewer.pal(length(levels(data[,score_cat])),pal), drop=FALSE)  +
    scale_x_continuous(breaks = seq(0, 100, by = 10),limits=c(min(-2.5,cat_mean-cat_sd),102.5)) +
    scale_y_continuous(breaks = seq(0, y_max, by = final.scale),limits=c(NA,y_max), labels=lab) +
    labs(x = score_type, y = y.lab) +
    
    theme(plot.margin = unit(c(1, 1, 3, 1), "lines"),
          legend.title=element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          panel.background = element_rect(fill = "white"),
          legend.position="top",
          legend.box = "horizontal",
          axis.text.y = element_text(size = 12, colour = "black", family=par()$family),
          axis.text.x = element_text(size = 12, colour = "black", family=par()$family),
          axis.title.y = element_text(size = 14, family=par()$family),
          axis.title.x = element_text(size = 14, family=par()$family),
          legend.text = element_text(size = 12, family=par()$family)) +
    geom_vline(aes(xintercept = cat_mean+cat_sd), color="black", linetype="dashed", size=1)+
    geom_vline(aes(xintercept = cat_mean), color="black", linetype="dashed", size=1)+
    geom_vline(aes(xintercept = cat_mean-cat_sd), color="black", linetype="dashed", size=1)

  gt <- ggplot_gtable(ggplot_build(plot.cap))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}
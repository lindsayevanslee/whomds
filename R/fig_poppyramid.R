################## Arguments of function popPyrFun ###########################
# - data: data frame name                                                    #
# - age: name of the age variable in the data frame "data". It should be     #
#   continuous age; ex. "edad"                                               #
# - gender: name of the gender variable. It should be a factor with 2 levels,# 
#   with labels (Male and Female); ex. "gender"                              #
##############################################################################

popPyramidFun <- function(data, age, gender){
  # Preparing age 
  data$age_cat <- NULL
  data$age_cat <- cut(data[, age], breaks = c(seq(-1, 100, by = 5), 110), 
                      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                 "25-29", "30-34", "35-39", "40-44", "45-49",
                                 "50-54", "55-59", "60-64", "65-69", "70-74",
                                 "75-79", "80-84", "85-89", "90-94", "95-99",
                                 "100+"))
  # Preapring the data
  data.new <- as.data.frame(table(data$age_cat, data[,gender]))
  colnames(data.new) <- c("age", "gender", "population")
  
  # Plot
  plot.pyr <- ggplot(data.new, aes(x = age, y = population, fill = gender)) + 
    guides(fill = guide_legend(title = NULL)) + 
    geom_bar(data = subset(data.new, gender == "Female"), stat = "identity") + 
    geom_bar(data = subset(data.new, gender == "Male"), stat = "identity",
             position = "identity", mapping = aes(y = -population)) +  
    scale_y_continuous(labels = abs, name = "Population") +  
    scale_x_discrete(name = "Age group (years)") + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    theme_bw() +
    theme(text=element_text(family=par()$family))
  
  return(plot.pyr)
}
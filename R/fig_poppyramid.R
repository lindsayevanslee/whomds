#' Print a population pyramid
#'
#' @param var_age a string (length 1) of the name of the age column
#' @param var_sex a string (length 1) of the name of the sex column
#' @param x_axis a string (length 1) indicating whether to use absolute numbers or sample percentage on the x-axis. Choices are \code{"n"} (default) or \code{"pct"}. 
#' @param age_plus a numeric value (length 1) indicating the age that is the first value of the oldest age group. Default is 100, for the last age group to be 100+
#' @param age_by a numeric value (length 1) indicating the width of each age group, in years. Default is 5.
#' @inheritParams rasch_mds
#'
#' @return A population pyramid figure 
#' 
#' @details A population pyramid gives an image of the age and sex distribution of a population.
#' 
#' The function divides the \code{var_age} variable into intervals of width \code{age_by}.
#' 
#' @export
#' 
#' @family figure functions
#' 
#' @import ggplot2
#' @import dplyr
#'
#' @examples 
#' fig_poppyramid(df_adults, "age", "sex")
fig_poppyramid <- function(df, var_age, var_sex, x_axis = c("n", "pct"), age_plus = 100, age_by = 5){
  
  #make sure x_axis contains acceptable answers
  x_axis <- match.arg(x_axis)
  
  #store x axis label based on x_axis
  if (x_axis == "n") x_axis_label <- "Sample (N)"
  else if (x_axis == "pct") x_axis_label <- "Sample (%)"
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #create age labels
  age_labels <- c(paste0(seq(0,age_plus-age_by,age_by), 
                         "-", 
                         seq(age_by-1, age_plus-1, age_by)), 
                  paste0(age_plus,"+"))
  
  #create age groups
  df <- df %>% 
    mutate(age_cat = cut(!!rlang::sym(var_age),
                         breaks = c(seq(0, age_plus, by = age_by), Inf), 
                         labels = age_labels,
                         right = FALSE))
  
  #filter df, count, calculate percent
  df_new <- df %>% 
    group_by_at(c("age_cat", var_sex)) %>% 
    summarize(n=n()) %>% 
    rename("sex" = !!enquo(var_sex)) %>% 
    ungroup() %>% 
    mutate(pct = round(n/sum(n)*100, 1))
  
  #create plot
  plot_pry <- df_new %>% 
    ggplot(aes(x = age_cat, y = !!rlang::sym(x_axis), fill = sex)) + 
    guides(fill = guide_legend(title = NULL)) + 
    geom_bar(data = filter(df_new, sex == "Female"), 
             stat = "identity") + 
    geom_bar(data = filter(df_new, sex == "Male"), 
             stat = "identity",
             position = "identity", 
             mapping = aes(y = -!!rlang::sym(x_axis))) +  
    scale_y_continuous(labels = abs, name = x_axis_label) +  
    scale_x_discrete(name = "Age group (years)") + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    theme_bw()
  
  return(plot_pry)
}

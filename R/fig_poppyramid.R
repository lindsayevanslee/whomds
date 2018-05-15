#' Print a population pyramid
#'
#' @param df a data frame of household survey data where each row is a household member
#' @param var_age a string (length 1) of the name of the age column
#' @param var_sex a string (length 1) of the name of the sex column
#'
#' @return A population pyramid figure 
#' 
#' @details A population pyramid gives an image of the age and sex distribution of a population.
#' 
#' The function divides the \code{var_age} variable into 5-year intervals.
#' 
#' @export
#' 
#' @family figure functions
#' 
#' @import ggplot2
#' @import dplyr
#'
#' @examples 
#' fig_poppyramid(mdstest, "edad", "sex")
fig_poppyramid <- function(df, var_age, var_sex){
  
  #convert to tibble
  if (!is_tibble(df)) df <- df %>% as_tibble()
  
  df <- df %>% 
    mutate(age_cat = cut(pull(df,var_age),
                         breaks = c(seq(-1, 100, by = 5), Inf), 
                         labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                    "25-29", "30-34", "35-39", "40-44", "45-49",
                                    "50-54", "55-59", "60-64", "65-69", "70-74",
                                    "75-79", "80-84", "85-89", "90-94", "95-99",
                                    "100+")))
  
  df_new <- df %>% 
    group_by_at(c("age_cat", var_sex)) %>% 
    summarize(n=n()) %>% 
    rename("sex" = !!enquo(var_sex))
  
  plot_pry <- df_new %>% 
    ggplot(aes(x = age_cat, y = n, fill = sex)) + 
    guides(fill = guide_legend(title = NULL)) + 
    geom_bar(data = filter(df_new, sex == "Female"), stat = "identity") + 
    geom_bar(data = filter(df_new, sex == "Male"), stat = "identity",
             position = "identity", mapping = aes(y = -n)) +  
    scale_y_continuous(labels = abs, name = "Population") +  
    scale_x_discrete(name = "Age group (years)") + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    theme_bw()
  
  return(plot_pry)
}
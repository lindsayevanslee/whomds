#' Compute basic statistics of the number of members per group per household
#'
#' @param df a data frame of household data where the rows represent members of the households in the sample
#' @param hh_id string (length 1) indicating the name of the variable in \code{df} uniquely identifying households
#' @param group_by_var string (length 1) to pass to \code{group_by_at()} with name of variable in \code{df} to group results by
#'
#' @return A tibble with rows for each level of \code{group_by_var} and "Total" and columns for the Mean (SD), Median and Range of the number of people in each group per household.
#' 
#' @note Includes a call to \code{tidyr::complete()}, which causes the function to be a bit slow.
#'
#' @export
#'
#' @family table functions
#'
#' @import dplyr
#' @import rlang
#' 
#' @examples
#' table_basicstats(df_adults, "HHID", "age_cat")
table_basicstats <- function(df, hh_id, group_by_var) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()

  sym_group_by_var <- sym(group_by_var)
  
  disaggregated <- df %>%
    group_by_at(c(hh_id, group_by_var)) %>%
    summarize(nperHH = n()) %>%
    tidyr::complete(!!sym_group_by_var, fill = list(nperHH=0)) %>%
    group_by_at(c(group_by_var)) %>%
    summarize(mean = round(mean(nperHH),1),
              sd=round(stats::sd(nperHH),1),
              median = round(stats::median(nperHH),1),
              min=round(min(nperHH),1),
              max=round(max(nperHH),1)) %>%
    transmute(!!sym_group_by_var,
              mean_sd = paste0(mean," (", sd, ")"),
              median,
              range = paste0(min, " - ", max))
  
  total <- df %>%
    group_by_at(c(hh_id)) %>%
    summarize(nperHH = n()) %>%
    summarize(mean = round(mean(nperHH),1),
              sd=round(stats::sd(nperHH),1), 
              median = round(stats::median(nperHH),1),
              min=round(min(nperHH),1),
              max=round(max(nperHH),1)) %>%
    transmute(mean_sd = paste0(mean," (", sd, ")"),
              median,
              range = paste0(min, " - ", max)) %>%
    tibble::add_column(!!sym_group_by_var := "Total", .before=1)
  
  
  tab <- bind_rows(disaggregated,total)
  return(tab)
  
  
}


#only for disaggregating by one variable for now (group_by_var)
BasicStatsFunNew <- function(df, hh_id, group_by_var) {
  
  sym_group_by_var <- rlang::sym(group_by_var)
  
  disaggregated <- df %>%
    group_by_at(c(hh_id, group_by_var)) %>%
    summarize(nperHH = n()) %>%
    complete(!!sym_group_by_var, fill = list(nperHH=0)) %>%
    group_by_at(c(group_by_var)) %>%
    summarize(mean = round(mean(nperHH),1),
              sd=round(sd(nperHH),1),
              median = round(median(nperHH),1),
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
              sd=round(sd(nperHH),1), 
              median = round(median(nperHH),1),
              min=round(min(nperHH),1),
              max=round(max(nperHH),1)) %>%
    transmute(mean_sd = paste0(mean," (", sd, ")"),
              median,
              range = paste0(min, " - ", max)) %>%
    add_column(!!sym_group_by_var := "Total", .before=1)
  
  
  tab <- bind_rows(disaggregated,total)
  return(tab)
  
  
}

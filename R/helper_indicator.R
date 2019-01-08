#' Create indicators from data frame
#'
#' @param df a data frame
#' @param vars_indicators a character vector of the variables from \code{df} to create indicators for
#' @param mapvalues_from vector to pass to \code{plyr::mapvalues()} argument \code{from}
#' @param mapvalues_to vector to pass to \code{plyr::mapvalues()} argument \code{to}
#'
#' @return a data frame with new columns that are the indicators (factors) from \code{vars_indicators}, with the same names pasted with \code{"_ind"}.
#' @export
#'
#' @examples helper_indicator(df = chile_adults, 
#' vars_indicators = c("fa1", "fa2", "fa3"), 
#' mapvalues_from = 1:5, 
#' mapvalues_to = c(0,0,0,1,1))
helper_indicator <- function(df, vars_indicators, mapvalues_from , mapvalues_to) {
  
  vars_indicators_new <- paste0(vars_indicators, "_ind")
  
  df_new <- df %>%
    bind_cols(
      df %>% 
        select(!!vars_indicators) %>% 
        rename_at(vars(vars_indicators), 
                  funs(paste0(., "_ind")))
    ) %>%
    mutate_at(vars(vars_indicators_new), 
              funs(plyr::mapvalues(., from = mapvalues_from, to = mapvalues_to, warn_missing = TRUE))) %>% 
    mutate_at(vars(vars_indicators_new), funs(factor))
  
  return(df_new)
  
}

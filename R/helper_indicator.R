#' Create indicators from data frame
#'
#' @param df a data frame
#' @param vars_indicators a character vector of the variables from \code{df} to create indicators for
#' @param mapvalues_from vector to pass to \code{plyr::mapvalues()} argument \code{from}
#' @param mapvalues_to vector to pass to \code{plyr::mapvalues()} argument \code{to}
#' @param make_factor a logical indicating whethre resulting indicators should be factors. Default is \code{TRUE}.
#'
#' @return a data frame with new columns that are the indicators from \code{vars_indicators}, with the same names pasted with \code{"_ind"}.
#' @export
#'
#' @examples helper_indicator(df = df_adults, 
#' vars_indicators = c("EF1", "EF2", "EF3"), 
#' mapvalues_from = 1:5, 
#' mapvalues_to = c(0,0,0,1,1))
helper_indicator <- function(df, vars_indicators, mapvalues_from , mapvalues_to, make_factor = TRUE) {
  
  vars_indicators_new <- paste0(vars_indicators, "_ind")
  
  df_new <- df %>%
    bind_cols(
      df %>% 
        select(!!vars_indicators) %>% 
        rename_at(vars(all_of(vars_indicators)), 
                  list(~ paste0(., "_ind")))
    ) %>%
    mutate_at(vars(all_of(vars_indicators_new)), 
              list(~ plyr::mapvalues(., from = mapvalues_from, to = mapvalues_to, warn_missing = TRUE))) 
  
  if (make_factor) df_new <- df_new %>% 
    mutate_at(vars(all_of(vars_indicators_new)), 
              list( ~ factor(.)))
  
  return(df_new)
  
}

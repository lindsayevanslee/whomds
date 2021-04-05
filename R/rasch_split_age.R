#' Split all survey items by age category for a Rasch Model if they are not discrete
#'
#' @inheritParams rasch_mds_children
#' @inheritParams rasch_mds
#' @inheritParams rasch_testlet
#'
#' @return a named list with:
#' \item{df}{new \code{df} after splitting the variables}
#' \item{vars_metric}{new \code{vars_metric} after splitting the variables}
#' \item{max_values}{new \code{max_values} after splitting the variables}
#' @export
#' 
#' @family rasch functions
#' @family children analysis functions
#'
rasch_split_age <- function (df, vars_group, vars_metric, vars_id, max_values) {
  
  #capture levels of age_group
  levels_age_group <- levels(pull(df, vars_group))
  
  #initialize list of overlapping varibles
  vars_metric_overlap <- vector("list",length(vars_metric))
  names(vars_metric_overlap) <- names(vars_metric)
  
  #create list of overlapping variables for each age group and vector of all variables
  for (i in seq_along(vars_metric)) {
    vars_metric_overlap[[i]] <- vars_metric[[i]][vars_metric[[i]] %in% unlist(vars_metric[-i])]
  }
  
  vars_metric_overlap_all <- helper_varslist(vars_metric_overlap)
  
  #if there are overlapping variables, make the split
  if (length(vars_metric_overlap_all) != 0) {
    
    #initialize list to store spread data with new discrete variables by age group
    df_split <- vector("list", length(vars_metric_overlap_all)) 
    names(df_split) <- vars_metric_overlap_all
    
    
    #for each variable that overlaps over multiple age groups
    for (var in vars_metric_overlap_all) {
      
      #select vars needed
      subtbl <- df %>% 
        select(vars_id, vars_group, var)
      
      #spread variables to create three discrete variables
      subtbl_spread <- subtbl %>% 
        tidyr::spread(key = !!quo(vars_group), value = !!quo(var)) %>%
        rename_at(vars(levels_age_group), list(~ paste0(var,"_",.)))
      
      #give error if number of rows isn't maintained
      if (nrow(subtbl_spread) != nrow(df)) stop(paste0("Spread table for ", var, " has nrow that doesn't match nrow(df). Check what's going on."))
      
      #save spread data in list
      df_split[[var]] <- subtbl_spread
      
    }
    
    #combine new vars with rest of the data
    df <- df_split %>% 
      purrr::reduce(left_join) %>%
      left_join(df)
    
    
    
    #edit list of variables - all list and grouped list
    vars_metric_almost <- purrr::map(names(vars_metric), function(nm_vset) {
      vset <- vars_metric[[nm_vset]]
      test <- vset %in% vars_metric_overlap_all
      
      if (any(test)) {
        
        new_split_vars <- paste0(vset[which(test)], "_",
                                 nm_vset)
        
        new_vset <- vset[-which(test)]
        new_vset <- c(new_vset, new_split_vars)
        
        
      } else {
        new_vset <- vset
      }
      
      return(new_vset)
      
    })
    names(vars_metric_almost) <- names(vars_metric) 
    
    #edit max values
    max_values_almost <- purrr::map(names(vars_metric), function(nm_vset)  {
      vset <- vars_metric[[nm_vset]]
      test <- vset %in% vars_metric_overlap_all
      
      if (any(test)) {
        
        new_max_values <- max_values %>% 
          filter(var %in% vset[which(test)]) %>% 
          mutate(var = paste0(var, "_", nm_vset))
        
        return(new_max_values)
        
      } 
      
    })
    
    #finish up edits
    vars_metric <- vars_metric_almost
    max_values <- max_values %>%
      filter(!(var %in% vars_metric_overlap_all)) %>% 
      bind_rows(max_values_almost)
    
    
  } 
  
  split_age_result <- list(df = df,
                           vars_metric = vars_metric,
                           max_values = max_values)
  
  return(split_age_result)
  
}

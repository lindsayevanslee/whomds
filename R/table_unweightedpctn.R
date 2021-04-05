#' Compute unweighted percent and N for multiple variables, disaggregated
#'
#' @param vars_demo a character vector of names of variables to calculate percent and N for
#' @param group_by_var a string (length 1) with the name of the variable from \code{df} to disaggregate by
#' @param spread_by_group_by_var logical determining whether to pass \code{group_by_var} to \code{tidyr::spread()} to give a wide-format tab. Default is FALSE.
#' @param group_by_var_sums_to_100 logical determining whether percentages sum to 100 along the margin of \code{group_by_var}, if applicable. Default is FALSE.
#' @param add_totals logical determinging whether to create total rows or columns (as appropriate) that demonstrate the margin that sums to 100. Default is FALSE.
#' @inheritParams rasch_mds
#'
#' @return A tibble with percent and N for each level of each variable in \code{vars_demo}
#' 
#' @family table functions
#' 
#' @export
#' 
#' @import dplyr
#' @import rlang
#' @import tidyr
#'
#' @examples
#' table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"))
#' table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), 
#' group_by_var = "disability_cat")
#' table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), 
#' group_by_var = "disability_cat", spread_by_group_by_var = TRUE)
table_unweightedpctn <- function(df, vars_demo, 
                                 group_by_var=NULL, 
                                 spread_by_group_by_var = FALSE, 
                                 group_by_var_sums_to_100 = FALSE,
                                 add_totals = FALSE) {
  
  #convert to tibble
  if (!tibble::is_tibble(df)) df <- df %>% as_tibble()
  
  #create sym of group_by_var if applicable
  if (!is.null(group_by_var)) {
    sym_group_by_var <- sym(group_by_var)
    
    #initialize final table
    final_tab <- tibble(
      item = character(),
      demo = character(),
      !!sym_group_by_var := character(),
      n = numeric(),
      pct = numeric()
    )
  } else {
    final_tab <- tibble(
      item = character(),
      demo = character(),
      n = numeric(),
      pct = numeric())
  }
  
  
  #for each vars_demo...
  for (i in seq_along(vars_demo)){
    
    #create sym of vars_demo at this iteration
    sym_this_vars_demo <- sym(vars_demo[i])
    
    #remove NAs of this vars_demo
    tab <- df %>% 
      filter(!is.na(!!sym_this_vars_demo)) 
    
    #if grouping, remove NAs of group_by_var
    if (!is.null(group_by_var)) {
      tab <- tab %>%
        filter(!is.na(!!sym_group_by_var))
      
      
      
    }
    
    #start grouping
    if (group_by_var_sums_to_100) {
      tab <- tab %>% 
        group_by_at(c(vars_demo[i], group_by_var))
    }
    if (!group_by_var_sums_to_100) {
      tab <- tab %>% 
        group_by_at(c(group_by_var, vars_demo[i]))
    }
  
    
    
    #create summary table
    tab <- tab %>%
      summarize(n=n()) %>% 
      mutate(pct=round(n/sum(n)*100,1)) %>% 
      rename(demo = !!sym_this_vars_demo) %>% 
      tibble::add_column(item = vars_demo[i], .before = 1)
    
    final_tab <- bind_rows(final_tab, tab)
    
  }
  
  
  if (!is.null(group_by_var)) {
    final_tab <- final_tab %>% 
      mutate(demo = ordered(demo, levels = unique(demo)), 
             !!sym_group_by_var := ordered(!!sym_group_by_var, levels=unique(!!sym_group_by_var))) %>% 
      select(item, demo, !!sym_group_by_var, pct, n)
    
    #spread, if applicable
    if (spread_by_group_by_var) {
      final_tab <- final_tab %>% 
        transmute(item, demo, !!sym_group_by_var,
                  pct_n = paste0(pct, "_", n)) %>% 
        tidyr::spread(!!sym_group_by_var, pct_n)
      
      #separate pct and n
      for (col in colnames(final_tab)[-(1:2)]) {
        final_tab <- final_tab %>% 
          tidyr::separate(!!rlang::sym(col), 
                          into = paste0(col, "_", c("pct", "n")), 
                          sep = "_")
      }
      
      
    }
    
  } else {
    final_tab <- final_tab %>% 
      mutate(demo = ordered(demo, levels = unique(demo))) %>% 
      select(item, demo, pct, n)
    
  }
  
  
  #make sure appropriate columns are numeric 
  if ((!is.null(group_by_var)) & !spread_by_group_by_var) {
      final_tab <- final_tab %>% 
        mutate_at(vars(-item, -demo, -!!sym(group_by_var)), as.numeric)
    } else {
    final_tab <- final_tab %>% 
      mutate_at(vars(-item, -demo), as.numeric) 
    }
  
  if (!is.null(group_by_var) & spread_by_group_by_var) {
    final_tab <- final_tab %>% 
      arrange(match(item, vars_demo))
  }

  
  
  #add totals, if applicable
  if (add_totals) {
    
    #if grouping...
    if (!is.null(group_by_var)) {
      
      #if spreading...
      if (spread_by_group_by_var) {
        
        #if summing along group_by_var... (total col)
        if (group_by_var_sums_to_100) {
          
          # table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), add_totals = TRUE, group_by_var = "disability_cat", spread_by_group_by_var = TRUE, group_by_var_sums_to_100 = FALSE)
          
          final_tab <- final_tab %>% 
            tibble::add_column(Total_pct = (final_tab %>% 
                                              select(ends_with("_pct")) %>% 
                                              rowSums(na.rm = TRUE)),
                               Total_n = (final_tab %>% 
                                            select(ends_with("_n")) %>% 
                                            rowSums(na.rm = TRUE)))
          

          
        } 
        #if not summing along group_by_var... (total row)
        else {
          # table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), add_totals = TRUE, group_by_var = "disability_cat", spread_by_group_by_var = TRUE, group_by_var_sums_to_100 = TRUE)
          
          
          final_tab <- final_tab %>%
            mutate(item = ordered(item)) %>%
            split(.$item) %>%
            purrr::map_dfr(function(df) {
              df %>%
                bind_rows(
                  df %>%
                    summarize_at(vars(-item, -demo), list(~ sum(., na.rm = TRUE))) %>%
                    tibble::add_column(item = unique(df$item), demo = "Total", .before = 1)
                )
            }) 

        }
        
        final_tab <- final_tab %>% 
          arrange(match(item, vars_demo))
        
      }
      #if not spreading...
      else {
        
        #if summing along group_by_var... 
        if (group_by_var_sums_to_100) {
          
          # table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), add_totals = TRUE, group_by_var = "disability_cat", spread_by_group_by_var = FALSE, group_by_var_sums_to_100 = TRUE)
          
          final_tab <-  final_tab %>% 
            group_by(item, !!sym(group_by_var)) %>% 
            nest() %>% 
            mutate(data = purrr::map(data, function(df) {
              df %>% 
                add_row(demo = "Total", pct = sum(df$pct, na.rm = TRUE), n = sum(df$n, na.rm = TRUE))
            })) %>% 
            unnest()
          

        }
        #if not summing along group_by_var...
        else {
          # table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), add_totals = TRUE, group_by_var = "disability_cat", spread_by_group_by_var = FALSE, group_by_var_sums_to_100 = FALSE)
          
          final_tab <-  final_tab %>% 
            group_by(item, demo) %>% 
            nest() %>% 
            mutate(data = purrr::map(data, function(df) {
              df %>% 
                add_row(!!sym(group_by_var) := "Total", pct = sum(df$pct, na.rm = TRUE), n = sum(df$n, na.rm = TRUE))
            })) %>% 
            unnest()
          
        }
        
      }
      
    } 
    #if not grouping...
    else {
      # table_unweightedpctn(df_adults, vars_demo = c("sex", "age_cat", "work_cat", "edu_cat"), add_totals = TRUE, group_by_var = NULL)
      final_tab <-  final_tab %>% 
        group_by(item) %>% 
        nest() %>% 
        mutate(data = purrr::map(data, function(df) {
          df %>% 
            add_row(demo := "Total", pct = sum(df$pct, na.rm = TRUE), n = sum(df$n, na.rm = TRUE))
        })) %>% 
        unnest()
      
    }

    
  }
  
 
  return(final_tab)
}



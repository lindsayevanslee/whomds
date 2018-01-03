#' Calculate a prevalence table
#'
#' @param data a data.table of the survey data
#' @param prev_vars a character vector with the names of the survey items for which prevalences should be calculated
#' @param stratify a logical vector (length 1) specifying whether to stratify the results by a factor, default is FALSE
#' @param stratum a character vector (length 1) with the name of the column to stratify by, default is NULL
#' @param disagg a character vector (length 1) with the name of the column by which to perform a second level of disaggregation (e.g. sex), default is NULL
#' @param form_ids formula with cluster ids to pass to survey::svydesign()
#' @param form_weights formula with survey weights to pass to survey::svydesign(), default is NULL
#' @param form_strata formula with survey strata to pass to survey::svydesign(), default is NULL
#' @param lev a numeric vector giving the response options of prev_vars, default is 1:5
#' @param collapse a logical vector (length 1) indicating whether to collapse response option percentages, default is FALSE
#' @param map_lev a numeric vector of new response options to map original response options (lev) to in order to collapse response options, default is equal to lev. Must be same length as lev. Only change if collapse=TRUE
#' @param which.prev a numeric vector giving the index of the response options that you want prevalences printed for, default is the vector of integers from 1 to the number of unique respone options in map_lev. For example if lev=0:4, and you collapse the last two response options by setting map_lev=c(1,2,3,4,4), and you only want percentages for new response options 3 and 4 (the second response option), then which.prev=3:4
#' @param stack a logical vector (length 1) indicating whether the prevalence table should be stacked, default is FALSE. This is most useful when prev_vars is length 1
#'
#' @return A table with the prevalences for each response option for the given prev_vars, stratified by stratum and disaggregated by disagg
#' @export
#'
#' @examples
prev_table <-
  function(data,
           prev_vars,
           stratify = FALSE,
           stratum = NULL,
           disagg = NULL,
           form_ids,
           form_weights = NULL,
           form_strata = NULL,
           lev = 1:5,
           collapse = FALSE,
           map_lev = lev,
           which.prev = 1:length(unique(map_lev)),
           stack = FALSE) {
    
    
    if (!identical(map_lev, lev) &
        !(collapse))
      stop("If you want to collapse categories, make sure collapse=TRUE!!!")
    
    options(survey.lonely.psu = "adjust")
    
    ### if wanting to collapse columns
    if (collapse) {
      for (i in seq_along(prev_vars)) {
        data[, prev_vars[i]] <- mapvalues(data[, prev_vars[i]],
                                          from = lev,
                                          to = map_lev,
                                          warn_missing = FALSE)
      }
    }
    
    # browser()
    
    ### Converting all prev_vars to factors
    prev.vars <-
      data.frame(lapply(as.data.frame(data[, prev_vars]),
                        function(X) {
                          factor(X, levels = unique(map_lev), ordered = TRUE)
                        }))
    
    colnames(prev.vars) <- prev_vars
    
    n.col <- length(which.prev)
    
    # browser()
    
    
    #####
    
    
    if (is.null(form_weights)) {
      df_weights <- model.frame( ~ 1, data)
    } else {
      df_weights <- model.frame(form_weights, data)
    }
    
    df_ids <- model.frame(form_ids, data)
    
    if (is.null(form_strata)) {
      df_strata <- model.frame( ~ 1, data)
    } else {
      df_strata <- model.frame(form_strata, data)
    }
    
    data.new <- cbind(prev.vars,
                      df_weights,
                      df_ids,
                      df_strata,
                      data[, disagg],
                      1:dim(data)[1],
                      data[, stratum])
    colnames(data.new) <- c(
      prev_vars,
      names(df_weights),
      names(df_ids),
      names(df_strata),
      disagg,
      "index",
      stratum
    )
    
    ## Disaggregation = NULL
    if (is.null(disagg)) {
      ## Stratification = TRUE
      if (stratify == TRUE) {
        # Checking if stratum is a factor
        if (is.factor(data.new[, stratum]) == FALSE) {
          stratum.fac <- factor(data.new[, stratum])
        }
        else {
          stratum.fac <- data.new[, stratum]
        }
        stratum.levels <- sort(unique(stratum.fac))
        stratum.n.levels <- length(levels(stratum.levels))
        
        data.stratum <- split(data.new, list(stratum.fac))
        
        survey.design <- list()
        
        prev.table <- NULL
        
        for (k in 1:stratum.n.levels) {
          survey.design[[k]] <- svydesign(
            id = form_ids,
            strata = form_strata,
            weights = form_weights,
            data = data.stratum[[k]],
            nest = TRUE
          )
          
          prev <- list()
          for (j in 1:length(prev_vars)) {
            if (all(is.na(data.stratum[[k]][, j]))) {
              prev[[j]] = rep(NA, n.col)
            }
            else {
              prev[[j]] <- round(svymean(
                ~ data.stratum[[k]][, j],
                design = survey.design[[k]],
                na.rm = TRUE
              )[which.prev],
              3) * 100
              
              
            }
            
          }
          
          names(prev) <- prev_vars
          prevt <-
            matrix(
              unlist(prev),
              ncol = n.col,
              byrow = TRUE,
              dimnames = list(prev_vars,
                              paste(
                                rep(levels(stratum.levels)[k], n.col), 1:n.col, sep = "_"
                              ))
            )
          
          ###PUT ALL TOGETHER
          prev.table <- cbind(prev.table, prevt)
          
        }
        
        if (stack) {
          prev.table <-
            as.data.frame(matrix(prev.table, stratum.n.levels, n.col, byrow = TRUE),
                          row.names = levels(data[, stratum]))
          names(prev.table) <-
            paste("Prev", 1:n.col, sep = "_")
        }
        
      }
      
      ## Stratification = FALSE
      else {
        survey.design.t <- svydesign(
          id = form_ids,
          strata = form_strata,
          weights = form_weights,
          data = data.new,
          nest = TRUE
        )
        prevt <- list()
        for (j in 1:length(prev_vars)) {
          if (all(is.na(data.new[, j]))) {
            prevt[[j]] = rep(NA, n.col)
          }
          else {
            prevt[[j]] <- round(svymean( ~ data.new[, j],
                                         design = survey.design.t,
                                         na.rm = TRUE)[which.prev],
                                3) * 100
          }
        }
        names(prevt) <- prev_vars
        prev.table <-
          matrix(
            unlist(prevt),
            ncol = n.col,
            byrow = TRUE,
            dimnames = list(prev_vars, paste(rep(
              "Total", n.col
            ), 1:n.col,
            sep = "_"))
          )
      }
    }
    
    ## Disaggregation != NULL
    else {
      data.new[, disagg] <-
        factor(data.new[, disagg]) # Convert disaggregating variable to factor
      
      ## Stratification = TRUE
      if (stratify == TRUE) {
        # Checking if stratum is a factor
        if (is.factor(data.new[, stratum]) == FALSE) {
          stratum.fac <- factor(data.new[, stratum])
        }
        else {
          stratum.fac <- data.new[, stratum]
        }
        stratum.levels <- sort(unique(stratum.fac))
        stratum.n.levels <- length(levels(stratum.levels))
        
        data.disagg <-
          split(data.new, data.new[, disagg]) # split list of disaggregated data
        n.disagg <-
          length(data.disagg) # number of levels of disaggregated data
        
        prev.table <- vector("list", n.disagg)
        
        for (i in 1:n.disagg) {
          data.stratum <-
            split(data.disagg[[i]], stratum.fac[data.disagg[[i]][, "index"]])
          
          survey.design <- list()
          
          for (k in 1:stratum.n.levels) {
            survey.design[[k]] <- svydesign(
              id = form_ids,
              strata = form_strata,
              weights = form_weights,
              data = data.stratum[[k]],
              nest = TRUE
            )
            
            prev <- list()
            for (j in 1:length(prev_vars)) {
              if (all(is.na(data.stratum[[k]][, j]))) {
                prev[[j]] = rep(NA, n.col)
              }
              else {
                prev[[j]] <- round(svymean(
                  ~ data.stratum[[k]][, j],
                  design = survey.design[[k]],
                  na.rm = TRUE
                )[which.prev],
                3) * 100
              }
            }
            names(prev) <- prev_vars
            prevt <-
              matrix(
                unlist(prev),
                ncol = n.col,
                byrow = TRUE,
                dimnames = list(prev_vars, paste(
                  rep(levels(stratum.levels)[k], n.col), 1:n.col,
                  sep = "_"
                ))
              )
            
            ###PUT ALL TOGETHER
            prev.table[[i]] <- cbind(prev.table[[i]], prevt)
            
            
          }
          
          if (stack) {
            prev.table[[i]] <-
              as.data.frame(
                matrix(prev.table[[i]], stratum.n.levels, n.col, byrow = TRUE),
                row.names = levels(data[, stratum])
              )
            colnames(prev.table[[i]]) <-
              paste("Prev", 1:n.col, sep = "_")
          }
          
        }
      }
      
      ## Stratification = FALSE
      else {
        data.disagg <-
          split(data.new, data[, disagg]) # split list of disaggregated data
        n.disagg <-
          length(data.disagg) # number of levels of disaggregated data
        
        prev.table <- as.list(rep(NA, n.disagg))
        
        for (i in 1:n.disagg) {
          survey.design.t <- svydesign(
            id = form_ids,
            strata = form_strata,
            weights = form_weights,
            data = data.disagg[[i]],
            nest = TRUE
          )
          prevt <- list()
          for (j in 1:length(prev_vars)) {
            if (all(is.na(data.disagg[[i]][, j]))) {
              prevt[[j]] = rep(NA, n.col)
            }
            else {
              prevt[[j]] <- round(svymean(
                ~ data.disagg[[i]][, j],
                design = survey.design.t,
                na.rm = TRUE
              )[which.prev],
              3) * 100
            }
          }
          names(prevt) <- prev_vars
          prev.table[[i]] <-
            matrix(
              unlist(prevt),
              ncol = n.col,
              byrow = TRUE,
              dimnames = list(prev_vars, paste(
                rep("Total", n.col), 1:n.col,
                sep = "_"
              ))
            )
          
          
        }
      }
      names(prev.table) <- levels(data[, disagg])
    }
    
    
    return(prev.table)
    
  }

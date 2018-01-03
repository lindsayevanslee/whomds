#' Calculate a prevalence table
#'
#' @param data a data.table of the survey data
#' @param id a character vector (length 1) with the name of the column with the individual IDs
#' @param weight a character vector (length 1) with the name of the column with the survey weights, default is NULL
#' @param strata.svy a character vector (length 1) with the name of the column with the survey strata, default is NULL
#' @param prev_vars a character vector with the names of the survey items for which prevalences should be calculated
#' @param lev a numeric vector giving the response options of prev_vars, default is 1:5
#' @param which.prev a numeric vector giving the index of the response options that you want prevalences printed for, default is equal to lev. For example if lev=0:1 and you only want prevalences for response option 1 (the second response option), then which.prev=2
#' @param stratum a character vector (length 1) with the name of the column to stratify by, default is NULL
#' @param disagg a character vector (length 1) with the name of the column by which to perform a second level of disaggregation (e.g. sex), default is NULL
#' @param stack a logical vector (length 1) indicating whether the prevalence table should be stacked, default is FALSE. This is most useful when prev_vars is length 1
#'
#' @return A table with the prevalences for each response option for the given prev_vars, stratified by stratum and disaggregated by disagg
#' @export
#'
#' @examples
prev_table <-
  function(data,
           id,
           weight = NULL,
           strata.svy = NULL,
           prev_vars,
           lev = 1:5,
           which.prev = lev,
           stratum = NULL,
           disagg = NULL,
           stack = FALSE) {

    #set options
    opts <- options(survey.lonely.psu = "adjust")
    on.exit(options(opts), add = TRUE)

    #create formulas
    form_id <- formula(paste0("~",id))

    if (is.null(weight)) {
      form_weight <- NULL
    } else {
      data[, weight] <- data[, weight] / mean(data[, weight])
      form_weight <- formula(paste0("~", weight))
    }

    if (is.null(strata.svy)) {
      form_strata.svy <- NULL
    } else {
      form_strata.svy <- formula(paste0("~", strata.svy))
    }

    #save number if columns for each stratum
    n.col <- length(which.prev)

    ### Converting all prev_vars to factors
    prev.vars <- data.frame(lapply(as.data.frame(data[, prev_vars]),
                                   function(X) {
                                     factor(X, levels = lev, ordered = TRUE)
                                   }))

    colnames(prev.vars) <- prev_vars

    #####

    data.new <-
      cbind(prev.vars, data[, weight], data[, id], data[, strata.svy],
            data[, disagg], 1:dim(data)[1], data[, stratum])
    colnames(data.new) <-
      c(prev_vars, weight, id, strata.svy, disagg, "index", stratum)

    ## Disaggregation = NULL
    if (is.null(disagg)) {
      ## Stratification != NULL
      if (!is.null(stratum)) {
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
          survey.design[[k]] <- survey::svydesign(
            id = form_id,
            strata = form_strata.svy,
            weights = form_weight,
            data = data.stratum[[k]],
            nest = TRUE
          )

          prev <- list()
          for (j in 1:length(prev_vars)) {
            if (all(is.na(data.stratum[[k]][, j]))) {
              prev[[j]] <- rep(NA, n.col)
            }
            else {
              prev[[j]] <- round(survey::svymean(
                ~ data.stratum[[k]][, j],
                design = survey.design[[k]],
                na.rm = TRUE
              )[which.prev],
              3) * 100
            }
          }
          names(prev) <- prev_vars
          prevt <- matrix(
            unlist(prev),
            ncol = n.col,
            byrow = TRUE,
            dimnames = list(prev_vars,
                            paste(
                              rep(levels(stratum.levels)[k], n.col),
                              1:n.col, sep = "_"
                            ))
          )

          ###PUT ALL TOGETHER
          prev.table <- cbind(prev.table, prevt)

        }

        if (stack) {
          prev.table <-
            as.data.frame(matrix(prev.table,
                                 stratum.n.levels, n.col, byrow = TRUE),
                          row.names = levels(data[, stratum]))
          names(prev.table) <- paste("Prev", 1:n.col, sep = "_")
        }

      }

      ## Stratification = NULL
      else {
        survey.design.t <- survey::svydesign(
          id = form_id,
          strata = form_strata.svy,
          weights = form_weight,
          data = data.new,
          nest = TRUE
        )
        prevt <- list()
        for (j in 1:length(prev_vars)) {
          if (all(is.na(data.new[, j]))) {
            prevt[[j]] <- rep(NA, n.col)
          }
          else {
            prevt[[j]] <- round(survey::svymean( ~ data.new[, j],
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

      ## Stratification != NULL
      if (!is.null(stratum)) {
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
            survey.design[[k]] <- survey::svydesign(
              id = form_id,
              strata = form_strata.svy,
              weights = form_weight,
              data = data.stratum[[k]],
              nest = TRUE
            )

            prev <- list()
            for (j in 1:length(prev_vars)) {
              if (all(is.na(data.stratum[[k]][, j]))) {
                prev[[j]] <- rep(NA, n.col)
              }
              else {
                prev[[j]] <- round(survey::svymean(
                  ~ data.stratum[[k]][, j],
                  design = survey.design[[k]],
                  na.rm = TRUE
                )[which.prev],
                3) * 100
              }
            }
            names(prev) <- prev_vars
            prevt <- matrix(
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
            colnames(prev.table[[i]]) <- paste("Prev", 1:n.col, sep = "_")
          }

        }
      }

      ## Stratification = NULL
      else {
        data.disagg <-
          split(data.new, data[, disagg]) # split list of disaggregated data
        n.disagg <-
          length(data.disagg) # number of levels of disaggregated data

        prev.table <- as.list(rep(NA, n.disagg))

        for (i in 1:n.disagg) {
          survey.design.t <- survey::svydesign(
            id = form_id,
            strata = form_strata.svy,
            weights = form_weight,
            data = data.disagg[[i]],
            nest = TRUE
          )
          prevt <- list()
          for (j in 1:length(prev_vars)) {
            if (all(is.na(data.disagg[[i]][, j]))) {
              prevt[[j]] <- rep(NA, n.col)
            }
            else {
              prevt[[j]] <- round(survey::svymean(
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

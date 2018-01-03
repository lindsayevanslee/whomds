##########################################################################################
#                           FUNCTION TO OBTAIN THE N and % OF RESPONSES                  #
# Description                                                                            #
# . Function: NPercentFunDisagg                                                          #
# . Arguments:                                                                           #
#   - data: data.frame                                                                   #
#   - prev_vars: vector with the name of the variables (questions) for which the         #
#     N and % will be calculated. The questions should contain only the responses        #     
#     (e.g.1-5) and NA values. Other codes should be recoded or removed                  #
#   - stratify = Logical. Default is FALSE, which means that no stratification will be   #
#     done                                                                               #
#   - stratum: name of the variable in which the statistics should be stratified         #
#     (e.g. "capacity_cat")                                                              #
#   - id: id variable to be used in the svydesign function, e.g. "VARUNIT_N"             #
#   - just.yes = Logical: variable that determines if only yes=1 values from binary      #
#     variables should be exported. Default is FALSE                                     #
#   - disagg: factor variable from data set that table is disaggregated by               #
# . Output: table with mean (SD), median, and range of responses                         #
##########################################################################################


NPercentFunDisagg <- function(data,prev_vars,stratify=FALSE,stratum=NULL,id,just.yes=FALSE,disagg=NULL,stack=FALSE) {
  
  ### Converting all prev_vars to ordered factors if they are not already (if conversion done here, then it takes levels as the values present in vector)
  prev.vars <- data.frame(lapply(data.frame(data[,prev_vars]), function (X) {
    
    if (is.ordered(X)) {
      return(X)
    } else {
      uniq <- unique(X[!is.na(X)])
      lev <- uniq[order(uniq)]
      return(factor(X,levels=lev,ordered=TRUE))}
    }))
  
  colnames(prev.vars) <- prev_vars

  data.new <- data.frame(cbind(prev.vars, data[,id], 1:dim(data)[1], data[,stratum], data[,disagg]))
  colnames(data.new) <- c(prev_vars, id, "index", stratum, disagg)
  
  
  ## Disaggregation = NULL
  if (is.null(disagg)){
    
    ## Stratification = TRUE
    if (stratify==TRUE) {
      
      # Checking if stratum is a factor
      if (is.factor(data.new[,stratum]) == FALSE){
        stratum.fac <- factor(data.new[,stratum])
      } else{
        stratum.fac <- data.new[,stratum]
      }
      stratum.levels <- sort(unique(stratum.fac))
      stratum.n.levels <- length(levels(stratum.levels))
      
      data.stratum <- split(data.new, list(stratum.fac))
      
      table.fin <- NULL
      tabs <- list()
      prop.tabs <- list()
      n <- list()
      pcent <- list()
      new.tab <- list()
      
      for (k in 1:stratum.n.levels) {
        
        tabs[[k]] <- lapply(prev_vars, function(x) {table(data.stratum[[k]][,x])})
        prop.tabs[[k]] <- lapply(prev_vars, function(x) {round(prop.table(table(data.stratum[[k]][,x]))*100,1)})
        
        tabs[[k]] <- lapply(tabs[[k]], function(x) {c(Category=" ",x)})
        prop.tabs[[k]] <- lapply(prop.tabs[[k]], function(x) {c(Category=" ",x)})
        
        n[[k]] <- unlist(tabs[[k]])
        pcent[[k]] <- unlist(prop.tabs[[k]])
        
        ## If only yes=1 responses desired
        if (just.yes) {
          n[[k]] <- n[[k]][-which(names(n[[k]])!="1")]
          pcent[[k]] <- pcent[[k]][-which(names(pcent[[k]])!="1")]
          
          names(n[[k]]) <- prev_vars
          names(pcent[[k]]) <- prev_vars
          
        }
        
        new.tab[[k]] <- cbind(N=n[[k]], Percent=pcent[[k]])
        colnames(new.tab[[k]]) <- c(paste(stratum.levels[k],"N",sep="_"),
                                    paste(stratum.levels[k],"%",sep="_"))
        
        table.fin <- cbind(table.fin,new.tab[[k]])
        
      }
     
      if (stack) {
        table.fin <- as.data.frame(matrix(table.fin,stratum.n.levels,2,byrow=TRUE),
                                   row.names=as.character(stratum.levels))
        colnames(table.fin) <- c("N","%")
      }
      
    }
      
    
    ## Stratification = FALSE
    else {
      tabs <- lapply(prev_vars, function(x) {table(data.new[,x])})
      prop.tabs <- lapply(prev_vars, function(x) {round(prop.table(table(data.new[,x]))*100,1)})
      
      tabs <- lapply(tabs, function(x) {c(Category=" ",x)})
      prop.tabs <- lapply(prop.tabs, function(x) {c(Category=" ",x)})
      
      n <- unlist(tabs)
      pcent <- unlist(prop.tabs)
      
      ## If only yes=1 responses desired
      if (just.yes) {
        n <- n[-which(names(n)!="1")]
        pcent <- pcent[-which(names(pcent)!="1")]
        
        names(n) <- prev_vars
        names(pcent) <- prev_vars
        
      }
      
      table.fin <- cbind(N=n,Percent=pcent)
      
    }
    
    
  return(table.fin)
    
  }
  
  ## Disaggregation != NULL
  else {
    
    data.new[,disagg] <- factor(data.new[,disagg]) # Convert disaggregating variable to factor
    
    ## Stratification = TRUE
    if (stratify==TRUE) {
      
      # Checking if stratum is a factor
      if (is.factor(data.new[,stratum]) == FALSE){
        stratum.fac <- factor(data.new[,stratum])
      } else{
        stratum.fac <- data.new[,stratum]
      }
      stratum.levels <- sort(unique(stratum.fac))
      stratum.n.levels <- length(levels(stratum.levels))
      
      data.disagg <- split(data.new, data.new[,disagg])
      n.disagg <- length(data.disagg)
      
      table.fin <- vector("list",n.disagg)
      
      for (i in 1:n.disagg){
       
        tabs <- list()
        prop.tabs <- list()
        n <- list()
        pcent <- list()
        new.tab <- list()
        
        data.stratum <- split(data.disagg[[i]], stratum.fac[data.disagg[[i]][,"index"]])
        
        for (k in 1:stratum.n.levels) {
          
          tabs[[k]] <- lapply(prev_vars, function(x) {table(data.stratum[[k]][,x])})
          prop.tabs[[k]] <- lapply(prev_vars, function(x) {round(prop.table(table(data.stratum[[k]][,x]))*100,1)})
          
          tabs[[k]] <- lapply(tabs[[k]], function(x) {c(Category=" ",x)})
          prop.tabs[[k]] <- lapply(prop.tabs[[k]], function(x) {c(Category=" ",x)})
          
          n[[k]] <- unlist(tabs[[k]])
          pcent[[k]] <- unlist(prop.tabs[[k]])
          
          ## If only yes=1 responses desired
          if (just.yes) {
            n[[k]] <- n[[k]][-which(names(n[[k]])!="1")]
            pcent[[k]] <- pcent[[k]][-which(names(pcent[[k]])!="1")]
            
            names(n[[k]]) <- prev_vars
            names(pcent[[k]]) <- prev_vars
            
          }
          
          new.tab[[k]] <- cbind(N=n[[k]], Percent=pcent[[k]])
          colnames(new.tab[[k]]) <- c(paste(stratum.levels[k],"N",sep="_"),
                                      paste(stratum.levels[k],"%",sep="_"))
          
          table.fin[[i]] <- cbind(table.fin[[i]],new.tab[[k]])
          
        }
        
       
        if (stack) {
          table.fin[[i]] <- as.data.frame(matrix(table.fin[[i]],stratum.n.levels,2,byrow=TRUE),
                                           row.names=levels(data[,stratum]))
          colnames(table.fin[[i]]) <- c("N","%")
        } 
         
      }
      
    }
    
    ## Stratification = FALSE
    else {
      
      data.disagg <- split(data.new, data[,disagg]) # split list of disaggregated data
      n.disagg <- length(data.disagg) # number of levels of disaggregated data
      
      table.fin <- vector("list",n.disagg)
      
      for (i in 1:n.disagg) {
      
        tabs <- lapply(prev_vars, function(x) {table(data.disagg[[i]][,x])})
        prop.tabs <- lapply(prev_vars, function(x) {round(prop.table(table(data.disagg[[i]][,x]))*100,1)})
        
        tabs <- lapply(tabs, function(x) {c(Category=" ",x)})
        prop.tabs <- lapply(prop.tabs, function(x) {c(Category=" ",x)})
        
        n <- unlist(tabs)
        pcent <- unlist(prop.tabs)
        
        ## If only yes=1 responses desired
        if (just.yes) {
          n <- n[-which(names(n)!="1")]
          pcent <- pcent[-which(names(pcent)!="1")]
          
          names(n) <- prev_vars
          names(pcent) <- prev_vars
          
        }
        
        table.fin[[i]] <- cbind(N=n,Percent=pcent)
      }
    }
    
    names(table.fin) <- levels(data[,disagg])
    return(table.fin)
    
  }
  
  
  
  
  
  
  
}
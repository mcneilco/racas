is_null_or_na <- function(value) {
  if(is.null(value)) return(TRUE)
  return(is.na(value))
}
na_to_null <- function(x) {
  if(is_null_or_na(x)) return(NULL)
  return(x)
}
length0_or_na_to_null <- function(x) {
  if(length(x) == 0) return(NULL)
  if(is_null_or_na(x)) return(NULL)
  return(x)
}
list_to_data.table <- function(l) {
  dt <- data.table(1)
  invisible(lapply(1:length(l) , function(x) {
    if(is.null(l[[x]])) return()
    if(length(l[[x]]) == 1) {
      dt[ , names(l)[[x]] := l[[x]]]
    } else {
      if(class(l[[x]]) == "data.frame") {
        dt[ , names(l)[[x]] := list(list(as.data.table(l[[x]])))]
      } else {
        dt[ , names(l)[[x]] := list(list(l[[x]]))]
      }
    }
  }
  ))
  dt[ , V1 := NULL]
  return(dt)
}
data.table_to_html_table <- function(dataTable, ...) {
  htmlTableString <- ""
  if(is.null(dataTable)) {return(htmlTableString)}
  if(nrow(dataTable) == 0) {
    return(htmlTableString)
  }
  htmlTableString <- print(xtable(dataTable, ...), 
                           type = "html",
                           ...)
  htmlTableString <- gsub("\\\n","",htmlTableString)
  return(htmlTableString)
}
capture_output <- function(obj, ...) {
  val <- capture.output({
    result <- withVisible(obj)
    if (result$visible)
      print(result$value)
  })
  nonEmpties <- which(val!="")
  val <- val[nonEmpties[1]:nonEmpties[length(nonEmpties)]]
  val <- gsub(" ", "&nbsp;", val)
  return(paste(val, ...))
}
flatten_list_in_data_table <- function(dt, selectColumn, parentAddFields = NULL, newParentNames = NULL) {
  dt <- copy(dt)
  if(length(selectColumn) > 1) {
    stop("Length of selectColumn must be 1")
  }
  if(!selectColumn %in% names(dt)) {
    stop(paste0("'",selectColumn, "' not found in names(dt)"))
  }
  if(!is.null(parentAddFields)) {
    if(!selectColumn %in% names(dt)) stop(paste0(sqliz(parentAddFields), " not found in names(dt)"))
    if(!is.null(newParentNames)) {
      if(length(parentAddFields) != length(newParentNames)) stop("length of newNames must equal length of parentNames")
    } else {
      newParentNames <- parentAddFields
    }
    dt[ , id_dt := 1:nrow(dt)]
    dt[ , returnLists := {
      addFields <- eval(parse(text=paste0('c(',paste0(parentAddFields,collapse = ","),")")))
      names(addFields) <- newParentNames
      vals <- get(selectColumn)
      returnVals <- lapply(vals, function(x, addFields) c(addFields,x), addFields)
      list(returnVals)
    }, by = id_dt]
    
  } else {
    dt[ , returnLists := selects]
  }
  selects <- Reduce(function(x,y) rbind(x,y,fill = TRUE), lapply(dt$returnLists, as.data.table))
}
flatten_list_to_data.table <- function(l) {
  dt <- Reduce(function(x,y) rbind(x,y,fill = TRUE), lapply(1:length(l), function(x) {
    dt <- as.data.table(l[[x]])
    dt[ , name := names(l)[[x]]]
    classes <- lapply(dt, class) 
    removeThese <- names(classes)[classes=="NULL"]
    if(length(removeThese) > 0) {
      dt[ , removeThese := NULL, with = FALSE]
    }
    return(dt)
  }
  )
  )
  return(dt)
}

list_to_data.table <- function(l) {
  dt <- data.table(1)
  invisible(lapply(1:length(l) , function(x) {
    if(is.null(l[[x]])) return()
    if(length(l[[x]]) == 1) {
      dt[ , names(l)[[x]] := l[[x]]]
    } else {
      if(class(l[[x]]) == "data.frame") {
        dt[ , names(l)[[x]] := list(list(as.data.table(l[[x]])))]
      } else {
        dt[ , names(l)[[x]] := list(list(l[[x]]))]
      }
    }
  }
  ))
  dt[ , V1 := NULL]
  return(dt)
}
coalesce <- function(...) {
  out <- Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}
parse_command_line_args <- function(args) {
  #args <- c("--tables", "api_curve_params", "api_dose_response")
  hh <- paste(unlist(args),collapse=' ')
  listoptions <- unlist(strsplit(hh,'--'))[-1]
  options.args <- sapply(listoptions,function(x){
    unlist(strsplit(x, ' '))[-1]
  }, simplify=FALSE)
  options.names <- sapply(listoptions,function(x){
    option <-  unlist(strsplit(x, ' '))[1]
  })
  names(options.args) <- unlist(options.names)
  return(options.args)  
}

combine.lists <- function(list1, list2) {
  
  list1.names <- names(list1)
  list2.names <- names(list2)
  
  new.list <- list1
  
  
  tmp <- match(list2.names, list1.names)
  w <- which(!is.na(tmp))
  
  if (length(w) > 0)
  {
    # take values from list2 in matching dimension names
    tmp <- tmp[!is.na(tmp)]
    new.list[[tmp]] <- list2[[w]]
    
    # append elements of 'list2' with unmatched names
    new.list <- c(new.list, list2[-w])
  }
  else
  {
    new.list <- c(new.list, list2)
  }
  
  new.list
} 
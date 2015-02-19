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
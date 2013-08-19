sqliz <- function(vector) {
  if (class(set)=="numeric") {
    sql <- paste(vector, collapse = ',')
    return(sql)
  }	else {
    sql <- paste0("'",paste(vector, collapse = "','"), "'")
    return(sql)
  }
}

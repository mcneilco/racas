sqliz <- function(set) {
  if (class(set)=="numeric") {
    tempstr <- as.data.frame(paste(set,",",sep=""),stringsAsFactors=FALSE)
    names(tempstr) <- "OBJECT"
    LastRow <- gsub(",","",tempstr$OBJECT[length(tempstr$OBJECT)])
    tempstr$OBJECT[length(tempstr$OBJECT)] <- LastRow
    sql <- paste(tempstr$OBJECT,collapse=" ")
    return(sql)
  }	else {
    tempstr <- as.data.frame(paste("'",set,"',",sep=""),stringsAsFactors=FALSE)
    names(tempstr) <- "OBJECT"
    LastRow <- gsub(",","",tempstr$OBJECT[length(tempstr$OBJECT)])
    tempstr$OBJECT[length(tempstr$OBJECT)] <- LastRow
    sql <- paste(tempstr$OBJECT,collapse=" ")
    return(sql)
  }
}

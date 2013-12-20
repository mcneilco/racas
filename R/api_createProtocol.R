#' Create acas protocol
#'
#' This function creates an acas protocol
#'
#' @param protocolName
#' @param shortDescription
#' @return recordedBy
#' @keywords register, create, protocol
#' @export
#' @examples
#' api_createProtocol(c("Brian Test Protocol2"), shortDescription = "Test Protocol", recordedBy = "bbolt")
api_createProtocol <- function(protocolName, shortDescription, recordedBy, stopOnAnyExist = TRUE) {
  if(is.null(protocolName)){
    stop("Protocol names cannot be null")
  }
  if(any(unlist(lapply(protocolName, function(x) x=="")))) {
    stop("Protocol names cannot equal \"\"")
  }
  if(any(duplicated(protocolName))) {
    stop("Protocol names must be unique")
  }
  if(is.null(recordedBy)){
    stop("Recorded by cannot be null")
  }
  if(recordedBy == ""){
    stop("Recorded by cannot equal \"\"")
  }
  
  protocolExistence <- checkExistence(protocolName, "protocolName")
  
  if(stopOnAnyExist) {
    if(any(unlist(protocolExistence))) {
      stop(paste0("Protocols already registered: '",paste(names(protocolExistence[unlist(protocolExistence)]),collapse = "', '")),"'")
    }
  }
  #Remove protocols that already exist
  protocolExistence[unlist(protocolExistence)] <- NULL
  protocolsToCreate <- names(protocolExistence)
  
  # Create the protocols

  createSimpleProtocol <- function(x) {
    options("scipen"=15)
    protocolLabels <- list()
    protocolLabels[[length(protocolLabels)+1]] <- createProtocolLabel(lsTransaction = lsTransaction, 
                                                                      recordedBy=recordedBy, 
                                                                      lsType="name", 
                                                                      lsKind="protocol name",
                                                                      labelText=x,
                                                                      preferred=TRUE)
    protocol <- createProtocol(lsTransaction = lsTransaction,
                   shortDescription=shortDescription,  
                   recordedBy=recordedBy, 
                   protocolLabels=protocolLabels
               )
    saveProtocol(protocol)
    return(protocol)
  }
  lsTransaction <- createLsTransaction()$id
  createdProtocols <- lapply(protocolsToCreate, createSimpleProtocol)
  return(createdProtocols)
}
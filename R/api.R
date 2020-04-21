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
    stopUser(paste0(racas::applicationSettings$client.protocol.label," names cannot be null"))
  }
  if(any(unlist(lapply(protocolName, function(x) x=="")))) {
    stopUser(paste0(racas::applicationSettings$client.protocol.label," names cannot equal \"\""))
  }
  if(any(duplicated(protocolName))) {
    stopUser(paste0(racas::applicationSettings$client.protocol.label," names must be unique"))
  }
  if(is.null(recordedBy)){
    stopUser("Recorded by cannot be null")
  }
  if(recordedBy == ""){
    stopUser("Recorded by cannot equal \"\"")
  }
  
  protocolExistence <- checkExistence(protocolName, "protocolName")
  
  if(stopOnAnyExist) {
    if(any(unlist(protocolExistence))) {
      stopUser(paste0(racas::applicationSettings$client.protocol.label,"s already registered: '",paste(names(protocolExistence[unlist(protocolExistence)]),collapse = "', '")),"'")
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

#' Check for protocol or experiment existence
#'
#' This function checks to see if a given protocol or experiment entity exists
#'
#' @param x entity label or code names of the same type
#' @param type entity type of x (protocolName, protocolCodeName, experimentName or experimentCodeName)
#' @return boolean list with names that match x
#' @keywords check, existence, exists
#' @export
#' @examples
#' checkExistence("ADME_Human Liver Microsome Stability", "protocolName")
#' checkExistence(c("ADME_Human Liver Microsome Stability","some other protocol name"), "protocolName")
#' checkExistence("PROT-00000001", "protocolCodeName")
#' checkExistence("EXPT-00000001", "experimentCodeName")
checkExistence <- function(x, type = c("protocolName", "experimentName", "protocolCodeName", "experimentCodeName")) {
  entities <- getEntity(x, type)
  entityExists <- lapply(entities, function(x) length(x)>0)
  return(entityExists)
}
#' Retrieves a list of entities (experiments or prototocols) by type
#'
#' This function retrieves a list of protocols or experiments by code name or label
#'
#' @param x entity names or code names of the same type 
#' @param type entity type of x (protocolName, protocolCodeName, experimentName or experimentCodeName)
#' @return a list of the entities returned from the server
#' @keywords get, entity
#' @export
#' @examples
#' getEntity("ADME_Human Liver Microsome Stability", "protocolName")
#' getEntity(c("ADME_Human Liver Microsome Stability","some other protocol name"), "protocolName")
#' getEntity("PROT-00000001", "protocolCodeName")
#' getEntity("EXPT-00000001", "experimentCodeName")
getEntity <- function(x, type = c("protocolName", "experimentName", "protocolCodeName", "experimentCodeName")) {
  if(length(type) > 1) {
    stopUser("Must specify one type")
  }
  type <- match.arg(type)
  servicePath <- switch(type,
                        protocolName = "protocols?FindByProtocolName&protocolName=",
                        protocolCodeName = "protocols?FindByCodeName&codeName=",
                        experimentName = "experiments?FindByExperimentName&experimentName=",
                        experimentCodeName = "experiments?FindByCodeName&codeName="
  )
  urls <- paste0(applicationSettings$client.service.persistence.fullpath, servicePath, lapply(x, RCurl::curlEscape, reserved = TRUE))
  entitiesJSON <- lapply(urls, getURL)
  entitiesList <- lapply(entitiesJSON, fromJSON)
  names(entitiesList) <- x
  return(entitiesList)
}

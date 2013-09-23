#' Check for entity existence
#'
#' This function checks to see if a given entity or list of entities exists
#'
#' @param x entity names or code names of the same type
#' @param type entity type of x (protocolName, experimentCodeName...etc.)
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
#' Retrieves a list of entities by type
#'
#' This function checks to see if a given entity exists
#'
#' @param x entity names or code names of the same type
#' @param type entity type of x (protocolName, experimentCodeName...etc.)
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
    stop("Must specify one type")
  }
  type <- match.arg(type)
  servicePath <- switch(type,
                        protocolName = "protocols?FindByProtocolName&protocolName=",
                        protocolCodeName = "protocols?FindByCodeName&codeName=",
                        experimentName = "experiments?FindByExperimentName&experimentName=",
                        experimentCodeName = "experiments?FindByCodeName&codeName="
  )
  urls <- paste0(applicationSettings$serverPath, servicePath, lapply(x, URLencode, reserved = TRUE))
  entitiesJSON <- lapply(urls, getURL)
  entitiesList <- lapply(entitiesJSON, fromJSON)
  names(entitiesList) <- x
  return(entitiesList)
}

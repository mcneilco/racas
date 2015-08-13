#' Gets configured entity type
#'
#' Get configured entity type
#' 
#' @param displayName character Optional display name to filter
#' @return list Configured entity types
#' @details Gets and optionally filters by display name the list of configured entity types in acas
#' @keywords entity, meta
#' @export
#' 
getConfiguredEntityTypes <- function(displayName) {
  if(missing(displayName)) displayName <- ""
  url <- URLencode(paste0(racas::applicationSettings$server.nodeapi.path, 
                               "/api/entitymeta/configuredEntityTypes/displayName/", displayName))
  configuredEntityTypes <- fromJSON(getURLcheckStatus(url))
  return(configuredEntityTypes)
}

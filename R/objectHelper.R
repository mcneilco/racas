#' Gets states from an entity
#' 
#' Allows you to select states of a specific lsTypeAndKind from an entity (protocol, experiment,
#' analysis group, treatment group, subject, container, or interaction)
#' @param entity A list that represents a single ACAS entity
#' @param typeAndKind A string value that follows the form "lsType_lsKind"
#' @return A list of lsStates
getStatesByTypeAndKind <- function(entity, typeAndKind) {
  return(entity$lsStates[lapply(entity$lsStates, function(x) x$"lsTypeAndKind")==typeAndKind])
}

#' Gets values from a state
#' 
#' Allows you to select states of a specific lsTypeAndKind from a state
#' @param lsState A list that represents a single lsState
#' @param typeAndKind A string value that follows the form "lsType_lsKind"
#' @return A list of lsValues
getValuesByTypeAndKind <- function(lsState, typeAndKind) {
  return(lsState$lsValues[lapply(lsState$lsValues, function(x) x$"lsTypeAndKind")==typeAndKind])
}

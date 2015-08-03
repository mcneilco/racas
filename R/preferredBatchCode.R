#' Gets preferred ids
#'
#' Entered batch codes (ids) are checked against a preferred id service
#' 
#' @param batchIds a character vector of batch codes
#' @param preferredIdService the url of the preferred id service. Defaults to \code{racas::applicationSettings$preferredBatchIdService}
#' @param testMode deprecated, was used for a test mode

#' @return a list of pairs of requested IDs and preferred IDs. On error, empty list
#' @details DEPRECATED: use \code{\link{getPreferredId2}} with \code{entityType="compound"} and \code{entityKind="batch name"}.
#' @keywords batchCode, preferred
#' @export
getPreferredId <- function(batchIds, preferredIdService = NULL, testMode=FALSE) {
  
  if (is.null(preferredIdService)) {
    preferredIdService <- paste0(racas::applicationSettings$server.nodeapi.path, 
                                racas::applicationSettings$client.service.preferred.batchid.path)
  }
  
  if (length(batchIds) > 500) {
    return(c(getPreferredIdInternal(batchIds[1:500], preferredIdService, testMode), 
             getPreferredId(batchIds[501:length(batchIds)], preferredIdService, testMode)))
  } else {
    return(getPreferredIdInternal(batchIds, preferredIdService))
  }
}

#' Gets preferred ids
#'
#' Internal code for \link{getPreferredId} (before chunking)
#' 
#' @param batchIds a character vector of batch codes
#' @param preferredIdService the url of the preferred id service. Defaults to \code{racas::applicationSettings$preferredBatchIdService}
#' @param testMode deprecated, was used for a test mode

#' @return a list of pairs of requested IDs and preferred IDs. On error, empty list
getPreferredIdInternal <- function (batchIds, preferredIdService = NULL, testMode=FALSE) {
  # Put the batchIds in the correct format
  requestIds <- list()
  if (testMode) {
    requestIds$testMode <- "true"
  }
  requestIds$requests = lapply(batchIds,function(input) {return(list(requestName=input))})
  
  
  # Get the preferred ids from the server
  response <- list(error=FALSE)
  tryCatch({
    response <- getURL(
      preferredIdService,
      customrequest='POST',
      httpheader=c('Content-Type'='application/json'),
      postfields=toJSON(requestIds))
  }, error = function(e) {
    stopUser(paste("Error in contacting the preferred ID service:", e$message))
  })
  if (substring(response,1,1)!="{") {
    stopUser(paste0("Error in contacting the preferred ID service: ", response))
  } else {
    tryCatch({
      response <- fromJSON(response)
    }, error = function(e) {
      stopUser(paste0("The loader was unable to parse the response it got from the preferred ID service: ", response))
    })
  }
  
  # Error handling
  if (grepl("^Error:",response[1])) {
    errorList <<- c(errorList, paste("The preferred ID service is having a problem:", response))
    return(list())
  } else if (response$error) {
    errorList <<- c(errorList, paste("The preferred ID service is having a problem:", response$errorMessages))
  }
  
  # Return the useful part
  return(response$results)
}

#' Gets preferred ids
#'
#' Entered entity codes (ids) are checked against a preferred id service.
#' 
#' @param batchIds a character vector of batch codes
#' @param entityType the type of the id's, e.g. "compound"
#' @param entityKind the kind of the id's, e.g. "batch name"
#' @param preferredIdService the url of the preferred id service. Defaults to
#'   \code{paste0(racas::applicationSettings$server.nodeapi.path,
#'   "/api/entitymeta/preferredCodes")}
#' @param testMode unused testing tool
#' @return a data.frame with names \code{c("Requested.Name", "Preferred.Code")}
#' @details Gets preferred id's for a range for inputs. For compound/batch name
#'   requests, it will use the relevant batch code check. For lsThings, it
#'   checks those entities for their preferred codes.
#' @keywords batchCode, preferred
#' @export
getPreferredId2 <- function (entityIds, entityType, entityKind, testMode=FALSE, preferredIdService = NULL) {
  # Put the entityIds in the correct format
  if (is.null(preferredIdService)) {
    preferredIdService <- paste0(racas::applicationSettings$server.nodeapi.path, 
                                 "/api/entitymeta/preferredCodes")
  }
  
  if (length(entityIds) > 500) {
    return(rbind(getPreferredId2(entityIds[1:500], entityType, entityKind, testMode, preferredIdService), 
                 getPreferredId2(entityIds[501:length(entityIds)], entityType, entityKind, testMode, preferredIdService)))
  } else {
    requestIds <- list()
    if (testMode) {
      requestIds$testMode <- "true"
    }
    
    requestIds$type = entityType
    requestIds$kind = entityKind
    requestIds$entityIdStringLines = paste(entityIds, collapse = "\n")
    
    # Get the preferred ids from the server
    response <- list(error=FALSE)
    response <- postURLcheckStatus(preferredIdService, toJSON(requestIds), requireJSON = TRUE)
    tryCatch({
      response <- fromJSON(response)
    }, error = function(e) {
      stopUser(paste0("The loader was unable to parse the response it got from the preferred ID service: ", response))
    })
    
    # Return the useful part
    return(read.csv(text=response$resultCSV, stringsAsFactors=FALSE))
  }
}


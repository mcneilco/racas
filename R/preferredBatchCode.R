#' Gets preferred ids
#'
#' Entered batch codes (ids) are checked against a preferred id service
#' 
#' @param batchIds a character vector of batch codes
#' @param preferredIdService the url of the preferred id service. Defaults to \code{racas::applicationSettings$preferredBatchIdService}
#' @param testMode deprecated, was used for a test mode

#' @return a list of pairs of requested IDs and preferred IDs. On error, empty list
#' @keywords batchCode, preferred
#' @export

getPreferredId <- function(batchIds, preferredIdService = NULL, testMode=FALSE) {
  
  if (is.null(preferredIdService)) {
    preferredIdService <- paste0("http://", racas::applicationSettings$client.host, ":", racas::applicationSettings$client.port, 
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
    stop(paste("Error in contacting the preferred ID service:", e$message))
  })
  if (substring(response,1,1)!="{") {
    stop("Error in contacting the preferred ID service: ", response)
  } else {
    tryCatch({
      response <- fromJSON(response)
    }, error = function(e) {
      stop("The loader was unable to parse the response it got from the preferred ID service: ", response)
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

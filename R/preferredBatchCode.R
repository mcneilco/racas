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
      postfields=rjson::toJSON(requestIds))
  }, error = function(e) {
    stopUser(paste("Error in contacting the preferred ID service:", e$message))
  })
  if (substring(response,1,1)!="{") {
    stopUser(paste0("Error in contacting the preferred ID service: ", response))
  } else {
    tryCatch({
      response <- rjson::fromJSON(response)
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
#' @param batchIds a character vector of codes
#' @param displayName the user display name of the codes (e.g. "Compound Batch ID")
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
getPreferredId2 <- function (entityIds, displayName, testMode=FALSE, preferredIdService = NULL) {
  # Put the entityIds in the correct format
  if (is.null(preferredIdService)) {
    preferredIdService <- paste0(racas::applicationSettings$server.nodeapi.path, 
                                 "/api/entitymeta/referenceCodes/csv")
  }
  
  if (length(entityIds) > 500) {
    return(rbind(getPreferredId2(entityIds[1:500], displayName, testMode, preferredIdService),
                 getPreferredId2(entityIds[501:length(entityIds)], displayName, testMode, preferredIdService)))
  } else {
    requestIds <- list()
    if (testMode) {
      requestIds$testMode <- "true"
    }
    
    requestIds$displayName = displayName
    requestIds$entityIdStringLines = paste(entityIds, collapse = "\n")
    
    # Get the preferred ids from the server
    response <- list(error=FALSE)
    response <- postURLcheckStatus(preferredIdService, rjson::toJSON(requestIds), requireJSON = TRUE)
    tryCatch({
      response <- rjson::fromJSON(response)
    }, error = function(e) {
      stopUser(paste0("The loader was unable to parse the response it got from the preferred ID service: ", response))
    })
    
    # Return the useful part
    return(read.csv(text=response$resultCSV, stringsAsFactors=FALSE))
  }
}

#' Gets parts of batch code
#' 
#' Takes a batch code and splits it on the last batch separator, as defined in 
#' \code{racas::applicationSettings$server.service.external.preferred.batchid.separator}.
#' getCompoundName will return everything except the last section, while
#' getBatchName will return everything after the last separator. For example, "CMPD-000001-01A"
#' is split into "CMPD-000001" and "01A" when the separator is "-".
#' @param batchCode vector of batch codes
#' @return character vector
getCompoundName <- function(batchCode) {
  # Split the batchCode to pull the compound name off the front, splitting by the last batchid.separator.
  # If nothing is left, returns the batchCode. Returns a character vector.
  batchSep <- racas::applicationSettings$server.service.external.preferred.batchid.separator
  splitBatch <- strsplit(batchCode, batchSep)
  # Remove last section to get compound name
  compoundName <- vapply(lapply(splitBatch, function(x) {
    head(x, n=length(x)-1)
  }), paste, "", collapse=batchSep)
  # If 
  compoundName[compoundName==""] <- batchCode[compoundName==""]
  return(compoundName)
}
#' @rdname getCompoundName
getBatchNumber <- function(batchCode) {
  batchSep <- racas::applicationSettings$server.service.external.preferred.batchid.separator
  splitBatch <- strsplit(batchCode, batchSep)
  batchNumber <- vapply(splitBatch, tail, character(1), n=1)
  batchNumber[batchNumber==batchCode] <- NA_character_
  return(batchNumber)
}

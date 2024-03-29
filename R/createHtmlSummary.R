#' Create an HTML summary
#' 
#' Takes a list of summary information and turns it into HTML that can be displayed in a web
#' interface.
#' 
#' @param hasError A boolean marking if the function returned an error
#' @param errorList A list of errors
#' @param hasWarning A boolean marking if the function had warnings
#' @param warningList A list of warnings
#' @param summaryInfo A list of information to return to the user (see details)
#' @param dryRun A boolean marking if information should be saved to the server
#' 
#' @details summaryInfo can have named elements "info": a list of information where names are used as titles,
#' and "viewerLink": a URL link to the program used to view the results
#' 
#' @return a string of HTML
#' 
#' @export
createHtmlSummary <- function(hasError,errorList,hasWarning,warningList,summaryInfo,dryRun) {
  require('brew')
  
  # Create a brew to load opening messages, errors, and warnings
  htmlOutputFormat <- "<p><%=startMessage%></p>
  <%=if(hasError) {htmlErrorList}%>
  <%=if(hasWarning&&dryRun) {htmlWarningList}%>"
  
  # If there is summmaryInfo, add it to the brew
  if(!is.null(summaryInfo)) {
    htmlOutputFormat <- paste0(htmlOutputFormat, "<h4>Summary</h4>")
  }
  if(!is.null(summaryInfo$info)) {
    htmlOutputFormat <- paste0(htmlOutputFormat, "<p>Information:</p>
                               <ul>
                               <li><%=paste(paste0(names(summaryInfo$info), ': ', vapply(summaryInfo$info, as.character, '')),collapse='</li><li>')%></li>
                               </ul>")
  }
  useSSL <- as.logical(applicationSettings$client.use.ssl)
  if (length(useSSL) != 1 || is.na(useSSL)) {
    useSSL <- FALSE
  }
  emailViewerLink <- paste0(ifelse(useSSL, "https://", "http://"), applicationSettings$client.host, ":", applicationSettings$client.port, summaryInfo$viewerLink)
  if(!is.null(summaryInfo$viewerLink)) {
    htmlOutputFormat <- paste0(htmlOutputFormat,"<div class=\"bv_openExptInQueryToolSection\"></div>",
"<p>*Note: there may be a delay before data is visible in ", racas::applicationSettings$client.service.result.viewer.displayName, "</p>")
  }
  
  # Create a header based on whether this is a dryRun and if there are warnings and errors
  if (dryRun) {
    if (hasError==FALSE) {
      if (hasWarning) {
        startMessage <- "Please review the warnings and summary before uploading."
      } else {
        startMessage <- "Please review the summary before uploading."
      }
    } else {
      startMessage <- "Please fix the following errors and use the 'Back' button at the bottom of this screen to upload a new version of the file."
    }
  } else {
    if (hasError) {
      startMessage <- "An error occured during uploading. If the messages below are unhelpful, you will need to contact your system administrator."
    } else {
      startMessage <- "Upload completed."
    }
  } 
  
  
  # Create a list of Errors
  htmlErrorList <- paste("<h4 style=\"color:red\">Errors:", length(errorList), "</h4>
                         <ul><li>", paste(errorList,collapse='</li><li>'), "</li></ul>")
  
  # Create a list of Warnings
  htmlWarningList <- paste0("<h4>Warnings: ", length(warningList), "</h4>
                            <p>Warnings provide information on issues found in the upload file. ",
                            "You can proceed with warnings; however, it is recommended that, if possible, ",
                            "you make the changes suggested by the warnings ",
                            "and upload a new version of the file by using the 'Back' button at the bottom of this screen.</p>
                            <ul><li>", paste(warningList,collapse='</li><li>'), "</li></ul>")
  
  return(paste(capture.output(brew(text=htmlOutputFormat)),collapse="\n"))
}

#' Save the HTML summary into the experiment
#' 
#' Saves a summary to the given experiment
#' 
#' @param experiment An experiment list of lists
#' @param hasError A boolean in the analysis had an error
#' @param htmlSummary A string that is html
#' @param lsTransaction An integer that is the transaction id (not used)
#' @param testMode a boolean used for testing (not used)
#' @param recordedBy current user (not used)
#' @return the htmlSummary
#' @export
saveAnalysisResults <- function(experiment, hasError, htmlSummary, recordedBy="none", dryRun=F, lsTransaction=NULL, testMode=FALSE) {
  # Saves (replace) the analysis html and status
  # Notes: experiment must have an "experiment metadata" state with values "analysis result html" and "analysis status"
  
  if (is.null(experiment)) {
    return (htmlSummary)
  }
  
  status <- if(hasError) {"failed"} else {"complete"}
  setExperimentStatus(status, experiment, recordedBy, dryRun, lsTransaction)
  
  setExperimentHtml(htmlSummary, experiment, recordedBy, dryRun, lsTransaction)
  return(htmlSummary)
}

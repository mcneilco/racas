#' Create an HTML summary
#' 
#' Takes a list of summary information and turns it into HTML that can be displayed in a web
#' interface.
#' 
#' @param hasError A boolean marking if the function returned an error
#' @param errorList A list of errors
#' @param hasWarning A boolean marking if the function had warnings
#' @param warningList A list of warnings
#' @param summaryInfo A list of information to return to the user
#' @param dryRun A boolean marking if information should be saved to the server
#' @export
createHtmlSummary <- function(hasError,errorList,hasWarning,warningList,summaryInfo,dryRun) {
  # Turns the output information into html
  # 
  # Args:
  #   hasError:             A boolean marking that there are errors
  #   errorList:            A list of errors
  #   hasWarning:           A boolean marking that there are warnings
  #   warningList:          A list of warnings
  #   summaryInfo:          A list of information to return to the user
  #   dryRun:               A boolean that marks if information should be saved to the server
  #
  # Returns:
  #  A character vector of html code
  
  require('brew')
  
  # Create a brew to load opening messages, errors, and warnings
  htmlOutputFormat <- "<p><%=startMessage%></p>
  <%=if(hasError) {htmlErrorList}%>
  <%=if(hasWarning&&dryRun) {htmlWarningList}%>"
  
  # If there is summmaryInfo, add it to the brew
  if(!is.null(summaryInfo)) {
    htmlOutputFormat <- paste0(htmlOutputFormat,
                               "<h4>Summary</h4>
                               <p>Information:</p>
                               <ul>
                               <li><%=paste(paste0(names(summaryInfo$info),': ',summaryInfo$info),collapse='</li><li>')%></li>
                               </ul>")
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

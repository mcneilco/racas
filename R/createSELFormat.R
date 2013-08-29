#' Creates a data.frame that contains the elements of the "Experiment Meta Data" section of SEL
#' 
#' The return structure represents the standard inputs of the "Experiment Meta Data" section of SEL
#' 
#' \itemize{
#'   \item format character used during the SEL process to determine the type of SEL load (default is "Generic")
#'   \item protocolName character that is the label_text of the protocol
#'   \item experimentName character that is the label_text of the experiment
#'   \item scientist character that is the username of the scientist who did the experiment
#'   \item notebook character that is the notebook where the experiment is described
#'   \item page character that is the page where the experiment is described
#'   \item assayDate character representing the date the experiment was run in the format YYYY-MM-DD
#'   \item project character that is the project for which the experiment was run
#' }
#' @return data.frame representing the most common inputs to the SEL experiment meta data section
#' @examples
#' experimentMetaData <- createSELExperimentMetaData(protocolName = "Cytotox Assay", 
#'                                                 experimentName = "2013123_cytotox_bbolt", 
#'                                                 assayDate = "2013-01-12")
#'                                                 
#' @docType data
createSELExperimentMetaData <- function(format = "Generic", protocolName = "", experimentName = "", scientist = "", notebook = "", page = "", assayDate = "", project = "") {
  experimentMetaData <- data.frame("Format" = format,
                                   "Protocol Name" = protocolName,
                                   "Experiment Name" = experimentName,
                                   "Scientist" = scientist,
                                   "Notebook" = notebook,
                                   "Page" = page,
                                   "Assay Date" = assayDate,
                                   "Project" = project,
                                   check.names=FALSE,
                                   stringsAsFactors = FALSE
  )
  return(experimentMetaData)
}
#' Creates a data.frame that contains the elements of the Calculated Results section of SEL
#' 
#' The return structure represents the standard inputs of the "Calculated Results" section of SEL
#' 
#' @param calculatedResultDataFrame a data.frame (or data.table) of calculated results
#' @param dataTypes optional character with length equal to ncol(calculatedResultDataFrame) with the data types for each column
#' @param ls_kinds optional character with length equal to ncol(calculatedResultDataFrame) with the ls_kinds for each column
#' @param unit_kinds optional character with length equal to ncol(calculatedResultDataFrame) with the unit_kinds for each column
#' @return data.frame representing the most common inputs to the SEL calculated results section
#' @docType data
#' 
#' @examples
#' 
#' #Basic Example
#' cars2 <- cars
#' cars2$Sample <- paste0("CORP",1:nrow(cars))
#' createSELCalculatedResults(cars2, corporateIDColumn = "Sample")
#' 
#' #This gives warning that "Corporte ID" not found
#' createSELCalculatedResults(cars)
#' 
#' #Additional Features
#' createSELCalculatedResults(cars2, corporateIDColumn = "Sample", dataTypes = c("Number","Text"))
#' createSELCalculatedResults(cars2, corporateIDColumn = "Sample", unit_kinds = c("miles per hour", "kilometers"))
#' createSELCalculatedResults(cars2, corporateIDColumn = "Sample", ls_kinds = c("IC50", "Max Efficacy"), unit_kinds = c("uM", "%"))
#' createSELCalculatedResults(cars2, corporateIDColumn = "Sample", ls_kinds = c("IC50 (still includes this)", "Max Efficacy"), unit_kinds = c("uM", "%"))
#' 
#' #Error handling
#' createSELCalculatedResults(cars2, corporateIDColumn = "Some Garbage")
#' createSELCalculatedResults(cars2, corporateIDColumn = "Sample", dataTypes = "Some Garbage")
createSELCalculatedResults <- function(calculatedResults, corporateIDColumn = "Corporate ID", dataTypes = NULL, ls_kinds = NULL, unit_kinds = NULL) {
  allowedResultClasses <- c("data.table", "data.frame")
  if(!class(calculatedResults) %in% allowedResultClasses) {
    stop(paste0("calculatedResults must be a ", paste(allowedResultClasses, collapse = " or ")))
  } else {
    calculatedResults <- as.data.frame(calculatedResults, check.names = FALSE)
    if(corporateIDColumn %in% names(calculatedResults)) {
      corpIDX <- grep(corporateIDColumn, names(calculatedResults))
      calcResults <- calculatedResults[,c(-corpIDX)]
      corporateIDCol <- data.frame("Datatype" = c("Corporate ID",calculatedResults[,corpIDX]), check.names = FALSE)
    } else {
      warning(paste0("\'",corporateIDColumn,"\' not found so \'Corporate ID\' column being added implicitely"))
      calcResults <- calculatedResults
      corporateIDCol <- data.frame("Datatype" = c("Corporate ID",rep("", nrow(calcResults))), check.names = FALSE)
    }
  }
  
  allowedDataTypes <- c("Number", "Text", "Date")
  defaultDataType <- "Number"
  if(!is.null(dataTypes)) {
    if(!all(dataTypes %in% allowedDataTypes)) {
      stop(paste0("dataTypes must be ", paste(allowedDataTypes, collapse = " or ")))
    } else {
      if(ncol(calcResults) != length(dataTypes)) {
        stop("Length of dataTypes must equal number of columns in calculatedResults excluding the corporateIDColumn column")
      }
    }
   } else {
    dataTypes <- rep(defaultDataType, ncol(calcResults))
  }
  
  if(!is.null(ls_kinds)) {
    if(ncol(calcResults) != length(ls_kinds)) {
      stop("Length of ls_kinds must equal number of columns in calculatedResults excluding the corporateIDColumn column")
    }
  } else {
    ls_kinds <- names(calcResults)
  }
  
  if(!is.null(unit_kinds)) {
    if(ncol(calcResults) != length(unit_kinds)) {
      stop("Length of unit_kinds must equal number of columns in calcResults excluding the corporateIDColumn column")
    }
  } else {
    unit_kinds <- NULL
  }
  
  if(is.null(unit_kinds)) {
    units <- NULL
  } else {
    units <- paste0(" (",unit_kinds,")")
  }
  kindsAndUnits <- paste0(ls_kinds, units)
  
  
  firstRow <- as.data.frame(t(kindsAndUnits), stringsAsFactors = FALSE)
  names(firstRow) <- dataTypes
  names(calcResults) <- dataTypes
  
  selFormattedCalculatedResults <- rbind(firstRow,
                                         calcResults
  )
  selFormattedCalculatedResults <- cbind(corporateIDCol,selFormattedCalculatedResults)
  
  return(selFormattedCalculatedResults)
}

#' Creates a data.frame that contains the elements of the Raw Results section of SEL
#' 
#' The return structure represents the standard inputs of the "Raw Results" section of SEL
#' 
#'
#' @docType data
#' 
#' @examples
#' 
#' Not implemented yet
createSELRawResults <- function(rawResults, curveIDColumn = "curve id", ls_kinds = NULL, unit_kinds = NULL) {
 return("NOT IMPLEMENTED YET")
}
#' Creates an SEL formatted file
#' 
#' Given data structures that represent the various pieces of an SEL file, this function will write or return SEL formatted files
#' 
#' @param selExperimentMetaData typically a data.frame created by \code{\link{createSELExperimentMetaData}}
#' @param selCalculatedResults optional data.frame of the format \code{\link{createSELCalculatedResults}}
#' @param selRawResults optional data.frame of the format \code{\link{createSELRawResults}}
#' @param outputFilePath optional output file path (see details for return if left NULL)
#' @param format "CSV", "XLS" or "XLSX" (not used if outputFilePath is NULL, see details)
#' @return character path of the written file or character of the CSV results
#' @details
#' 
#' If the outputFilePath is NULL then the function will write the CSV representation of the file to a character and return, otherwise it returns the path to the written file.
#' 
#' @examples
#' 
#' selExperimentMetaData <- createSELExperimentMetaData(protocolName = "Cytotox Assay", 
#'                                                 experimentName = "2013123_cytotox_bbolt", 
#'                                                 assayDate = "2013-01-12")
#'  
#' cars2 <- cars 
#' cars2$Sample <- paste0("CORP",1:nrow(cars))
#' selCalculatedResults <- createSELCalculatedResults(cars2, corporateIDColumn = "Sample")
#' 
#' #Outputs a character representation of a CSV SEL format
#' createSELFile(selExperimentMetaData, selCalculatedResults)
#' 
#' #Outputs a file in SEL format
#' myFile <- tempfile()
#' createSELFile(selExperimentMetaData, selCalculatedResults, outputFilePath = myFile)
#' read.csv(myFile)
createSELFile <- function(selExperimentMetaData, selCalculatedResults = NULL, selRawResults = NULL, outputFilePath = NULL, format = "CSV") {
  #TODO  implement format other than CSV
  
  #Pivot the selExperimentMetaData
  metaData <- convertSELExperimentMetaDataToSEL(selExperimentMetaData)
  
  #First determine how many columns are in this csv file
  ncolsOutput <- max(ncol(metaData),ncol(selCalculatedResults), ncol(selRawResults))
  
  #Create the output DF with as many columns as it will need
  if(ncol(metaData) < ncolsOutput) {
    outputDF <- cbind(metaData, as.data.frame(matrix(rep("", ncolsOutput - ncol(metaData)), nrow=1), stringsAsFactors = FALSE))
  } else {
    outputDF <- metaData
  }
  
  #add an extra line after experiment meta data section
  outputDF <- rbind(outputDF, rep("", ncol(outputDF)))
  
  #add calculated results if provided
  if(!is.null(selCalculatedResults)) {
    calcResultsHeader <- c("Calculated Results", rep("",ncol(outputDF) - 1))
    outputDF <- rbind(outputDF, calcResultsHeader)
    
    #Add any extra columns to the calculated results section needed in the output file
    if(ncol(selCalculatedResults) < ncol(outputDF)) {
      selCalculatedResults <- cbind(selCalculatedResults, as.data.frame(matrix(rep("", ncol(outputDF) - ncol(selCalculatedResults)), nrow=1)))
    }
    
    #add selCalculatedResults header to outputDF
    outputDF <- rbind(outputDF, names(selCalculatedResults))
    names(selCalculatedResults) <- names(outputDF)
    outputDF <- rbind(outputDF, selCalculatedResults)
    
    #add an extra line after calculated results section
    outputDF <- rbind(outputDF, rep("", ncol(outputDF)))
  }
  
  if(!is.null(selRawResults)) {
    rawResultsHeader <- c("Raw Results", rep("",ncol(outputDF) - 1))
    outputDF <- rbind(outputDF, rawResultsHeader)
    
    #Add any extra columns to the calculated results section needed in the output file
    if(ncol(selRawResults) < ncol(outputDF)) {
      selRawResults <- cbind(selRawResults, as.data.frame(matrix(rep("", ncol(outputDF) - ncol(selFormattedCalculatedResults)), nrow=1)))
    }
    
    #add selRawResults header to outputDF
    outputDF <- rbind(outputDF, names(selRawResults))
    names(selRawResults) <- names(outputDF)
    outputDF <- rbind(outputDF, selRawResults)
    
    #add an extra line after calculated results section
    outputDF <- rbind(outputDF, rep("", ncol(outputDF)))
    
  }
  
  #If the outputfile is not null then write to the given path, if it is null then return the results as a string
  if(!is.null(outputFilePath)) {
    write.table(outputDF, file = outputFilePath, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE, na="")
    return(outputFilePath)
  } else {
    csvResults <- paste0(capture.output(write.csv(outputDF, row.names = FALSE, quote = FALSE)),collapse = "\n")
    return(csvResults)
  }
}
#' Converts an Experiment Meta Data data.frame to a pivoted version
#' 
#' Pivotes a data.frame like that returned by \code{\link{createSELExperimentMetaData}} to a pivoted data.frame
#' 
#' @param experimentMetaData typically a data.frame created by \code{\link{createSELExperimentMetaData}}
#' @return data.frame pivoted version of the experimentMetaData
#' 
#' @examples
#' 
#' experimentMetaData <- createSELExperimentMetaData(protocolName = "Cytotox Assay", 
#'                                                 experimentName = "2013123_cytotox_bbolt", 
#'                                                 assayDate = "2013-01-12")
#'                                                 
#' convertSELExperimentMetaDataToSEL(experimentMetaData)
#' 
#' 
convertSELExperimentMetaDataToSEL <- function(experimentMetaData) {
  transposedExptMetaData <-   data.frame("V1" = t(experimentMetaData), check.names = FALSE)
  selExperimentMetaDataSection <-   data.frame("Property" = c("Experiment Meta Data",row.names(transposedExptMetaData)) ,
                                               "Value" = c("",as.character(transposedExptMetaData[,1])), 
                                               check.names = FALSE,
                                               stringsAsFactors = FALSE)
  return(selExperimentMetaDataSection)
}

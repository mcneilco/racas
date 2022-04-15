#' Reading of Excel or CSV
#' 
#' Read in xls, xlsx, csv, or txt (tab delimited) files and output a data frame of the information
#' 
#' @param filePath The path the the file to read
#' @param sheet Sheet number of an Excel sheet
#' @param header Boolean to treat first row as column names
#' 
#' @return a data.frame of the file
#' 
#' @details Right now, this turns dates into "A_date_was_in_Excel_Date_format" 
#' and does not treat headers as column names.
#' In fileRead.R
#' Hidden sheets in xls and xlsx files are ignored.

readDelim <- function(filePath, delim=",", testNLines = 500, ...) {
    # Read in a delimited file
    # Use delim regex to count number of columns as read.delim only reads the first 5 rows.
    fileEncoding <- getFileEncoding(filePath)
    fileData <- readLines(filePath, encoding=fileEncoding)
    # https://stackoverflow.com/questions/18893390/splitting-on-comma-outside-quotes
    # Regex explaination from stack overflow:
    # ,           // Split on comma
    # (?=         // Followed by
    #   (?:      // Start a non-capture group
    #     [^"]*  // 0 or more non-quote characters
    #     "      // 1 quote
    #     [^"]*  // 0 or more non-quote characters
    #     "      // 1 quote
    #   )*       // 0 or more repetition of non-capture group (multiple of 2 quotes will be even)
    #   [^"]*    // Finally 0 or more non-quotes
    #   $        // Till the end  (This is necessary, else every comma will satisfy the condition)
    # )
    splitRegex <- paste0(delim,"(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
    if(testNLines > 0 && length(fileData) >= testNLines) {
       linesToTestForNumColumns <- fileData[1:testNLines]
    } else {
        linesToTestForNumColumns <- fileData
    }
    # Loop through the lines, split using the regex, count the number of columns, return the max number of columns
    nCol <- max(unlist(lapply(linesToTestForNumColumns, function(x) length(tstrsplit(x, split=splitRegex, perl = TRUE)))))
    # Use read.csv, specify the number of columns by passing in a list of column names using the default naming convention of V+{colIndex}
    output <- read.delim(text = fileData, sep = delim, na.strings = "", stringsAsFactors=FALSE, fileEncoding=fileEncoding, col.names=paste0("V", 1:nCol), ...)
    return(output)
}

readExcelOrCsv <- function(filePath, sheet = 1, header = FALSE) {
  
  if (is.na(filePath)) {
    stopUser("Need Excel file path as input")
  }
  if (!file.exists(filePath)) {
    stopUser("Cannot find input file")
  }
  
  if (grepl("\\.xlsx?$",filePath)) {
    output <- tryCatch({
      wb <- XLConnect::loadWorkbook(filePath)
      sheetToRead <- which(unlist(lapply(XLConnect::getSheets(wb), XLConnect::isSheetVisible, object = wb)))[sheet]
      return(XLConnect::readWorksheet(wb, sheet = sheetToRead, header = header, dateTimeFormat="%Y-%m-%d"))
    }, error = function(e) {
      stopUser("Cannot read input excel file: ", e$message)
    })
  } else {
    # Get delim
    if (grepl("\\.csv$",filePath)) {
      delim <- ","
    } else if (grepl("\\.txt$",filePath)) {
      delim <- "\t"
    } else {
      stopUser("The input file must have extension .xls, .xlsx, .csv, or .txt")
    }
    output <- tryCatch({
      return(readDelim(filePath, delim = delim, header = header))
    }, warning = function(e) {
      # We haven't caught additional warnings so far
      # So we only stop if we get a warning that is known to cause issues
      if(e$message == "EOF within quoted string") {
        stop(e$message)
      } else {
        return(output)
      }
    }, error = function(e) {
      stopUser(paste0("Cannot read input csv file: ", e$message))
    })
  }
  return(output)
}

#' Check file encoding
#' 
#' Checks to see the encoding of the input file. 
#' 
#' @param filePath The file that needs to be checked, relative to your working directory
#' 
#' @return The file encoding for the input file
#' 
#' Two example UTF-16LE file can be found in inst/tests/data/:
#' @return UTF-16LE-test.txt
#' @return UTF-16LE-test2.txt
getFileEncoding <- function(filePath) {
  # "\xff\xfe" is the byte order mark for "UTF-16LE"
  # Referenced from the answer by 'roippi' in this question thread:
  # http://stackoverflow.com/questions/23729151/reading-text-file-into-variable-subsequent-print-returns-escape-characters
  fileEncoding <- ifelse(suppressWarnings(grepl("\xff\xfe",readLines(filePath, n=1), perl = TRUE)), 
                         "UTF-16LE", 
                         "")
  return(fileEncoding)
}

#'Get dataframe sections
#'
#'Gets a section of a data.frame that is divided by NA's or ""
#'
#'@param genericDataFileDataFrame A data frame of lines 
#'@param lookFor A string identifier to user as regext for the line before the start of the seciton
#'@param transpose a boolean to set if the data should be transposed
#'@param required logical Should the function throw an error if the field does not exist?
#'@return A dataframe of the of section in the generic excel file
#'
#'Intended for data.frames that have been read directly from a csv or xls and have sections with names

getSection <- function(genericDataFileDataFrame, lookFor, transpose = FALSE, required = TRUE) {
  # Get the first line matching the section
  listMatch <- sapply(genericDataFileDataFrame,grep,pattern = lookFor,ignore.case = TRUE, perl = TRUE)
  firstInstanceInEachColumn <- suppressWarnings(unlist(lapply(listMatch, min)))
  startSection <- firstInstanceInEachColumn[is.finite(firstInstanceInEachColumn)][1]
  if(is.na(startSection) && lookFor =="Raw Results") {
    return(NULL)
  }
  if(is.na(startSection)) {
    if(required) {
      stopUser(paste0("The spreadsheet appears to be missing an important section header. The loader needs '",lookFor,"' to be somewhere in the spreadsheet.",sep=""))
    } else {
      return(NULL)
    }
  }
  
  if((startSection+2)>length(genericDataFileDataFrame[[1]])) {
    stopUser(paste0("There must be at least two rows filled in after '", lookFor, 
                "'. Either there is extra data that you need to fill in, or you may wish to remove '", 
                lookFor, "' entirely."))
  }
  
  # Get the indexes of columns in the section, using the longest of either of the first two rows
  sectionHeaderRow <- genericDataFileDataFrame[startSection + 1,]
  secondRow <- genericDataFileDataFrame[startSection + 2,]
  sectionHeaderColumns <- grepl(pattern="\\S", sapply(sectionHeaderRow,as.character))
  secondHeaderColumns <- grepl(pattern="\\S", sapply(secondRow,as.character))
  if (all(!c(sectionHeaderColumns, secondHeaderColumns))) {
    stopUser(paste0("There must be at least two rows filled in after '", lookFor, "'."))
  }
  if (any(!secondHeaderColumns & !sectionHeaderColumns)) {
    dataColumnIndexes <- 1:(min(which(!secondHeaderColumns & !sectionHeaderColumns)) - 1)
  } else {
    dataColumnIndexes <- 1:length(sectionHeaderRow)
  }
  
  # Get the last line matching the section
  sectionColumn <- genericDataFileDataFrame[,names(startSection)]
  sectionColumnSubset <- subset(sectionColumn, 1:length(sectionColumn) > startSection)
  sectionLength <- which(is.na(sectionColumnSubset) | sectionColumnSubset %in% "")[1]
  if(is.na(sectionLength)) {
    sectionLength <- length(sectionColumnSubset) + 1
  }
  endSection <- startSection + sectionLength
  
  #Use the start and end variables to grab the data frame
  startSectionColumnNumber <- match(names(startSection),names(genericDataFileDataFrame))
  foundData <- subset(x = genericDataFileDataFrame, subset = 1:nrow(genericDataFileDataFrame) > startSection & 1:nrow(genericDataFileDataFrame) < endSection, select = dataColumnIndexes)
  
  # Transpose the data frame if option is set
  if(transpose == TRUE) {
    row.names(foundData) <- foundData[,1]
    foundData <- subset(foundData,select = 2:length(foundData))
    foundData <- as.data.frame(t(foundData), stringsAsFactors=FALSE)
  }
  
  foundData <- as.data.frame(lapply(foundData, gdata::trim), optional=TRUE, stringsAsFactors=FALSE)
  
  return(foundData)
}

#' Meta data validation
#'
#' Valides the meta data using services from configuration
#
#'@param metaData     	A "data.frame" of two columns containing the Meta data for the experiment
#'@param expectedDataFormat	A "data.frame" of three columns and the same number of rows as metaData 
#'describing the Meta data data frame and its expected classes. Columns:
#'\describe{
#'\item{headers}{tring values of the experiment meta data to extract}
#'\item{class}{String value of the expected class of the given value}
#'\item{isNullable}{Boolean containing whether the field can be empty}
#'}
#'@param errorEnv An environment to send error messages to
#'@return A data.frame containing the validated meta data
#'
#'@details Example expectedDataFormat can be found in generic_data_parser.R
validateSharedMetaData <- function(metaData, expectedDataFormat = NULL, errorEnv = NULL) {
  # Check if extra data was picked up that should not be
  if (length(metaData[[1]]) > 1) {
    extraData <- c(as.character(metaData[[1]][2:length(metaData[[1]])]),
                   as.character(metaData[[2]][2:length(metaData[[2]])]))
    extraData <- extraData[extraData != ""]
    addError(paste0("Extra data were found next to the Experiment Meta Data ",
                                      "and should be removed: '",
                                      paste(extraData, collapse="', '"), "'"), errorEnv = errorEnv)
    metaData <- metaData[1, ]
  }
  
  # Extract the expected headers from the input variable
  expectedHeaders <- expectedDataFormat$headers
  
  # Validate that there are no missing required columns, add errors for any expected fields that are missing
  missingColumns <- expectedHeaders[is.na(match(toupper(expectedHeaders),toupper(names(metaData)))) 
                                    & !(expectedDataFormat$isNullable)]
  for(m in missingColumns) {
    addError(paste("The loader could not find required Experiment Meta Data row:", m))
  }
  
  # Validate that the matched columns are of the same data type and non-nullable fields are not null
  # return modified metaData with results of the validation of each field
  matchedColumns <- metaData[, !is.na(match(toupper(names(metaData)), toupper(expectedHeaders)))]
  validatedMetaData <- metaData
  for(m in 1:length(matchedColumns)) {
    # Get the name of the column
    column <- names(matchedColumns)[m]
    
    # Find if it is Nullable
    nullable <- expectedDataFormat$isNullable[expectedDataFormat$headers == column]
    
    
    expectedDataType <- as.character(expectedDataFormat$class[expectedDataFormat$headers == column])
    receivedValue <- matchedColumns[1,m]
    if(!nullable && (is.null(receivedValue) | is.na(receivedValue) | receivedValue==""  | receivedValue=="")) {
      addError(paste0("The loader could not find an entry for '", column, "' in the Experiment Meta Data"), errorEnv = errorEnv)
    }
    
    validationFunction <- switch(expectedDataType, 
                                 "dateValue" = validateDate, 
                                 "Number" = validateNumeric, 
                                 "stringValue" = validateCharacter,  
                                 stopUser(paste("Internal Error: unrecognized class required by the loader:", expectedDataType))
    )
    validatedData <- validationFunction(receivedValue)
    validatedMetaData[, column] <- validatedData
  }
  
  # Add warnings for additional columns sent that are not expected
  additionalColumns <- names(metaData)[is.na(match(names(metaData),expectedHeaders))]
  if (length(additionalColumns) > 0) {
    if (length(additionalColumns) == 1) {
      warnUser(paste0("The loader found an extra Experiment Meta Data row that will be ignored: '", 
                     additionalColumns, 
                     "'. Please remove this row."))
    } else {
      warnUser(paste0("The loader found extra Experiment Meta Data rows that will be ignored: '", 
                     paste(additionalColumns,collapse="' ,'"), 
                     "'. Please remove these rows."))
    }
  }
  
  return(metaData)
}

#'Melt our data sets
#'
#'melts our specific format
#'
#'@param wideData a data.frame with columns of the same valueKind
#'@param splitColumn the column name in wideData to split groups on
#'@param splitFunction a function to use in secondary splitting of rows
#'@param stateGroups definitions for how to group states (see genericDataParserConfiguration.R), passed to splitFunction
#'@param resultTypes a data.frame with columns:
#'\describe{
#'\item{DataColumn}{the string that will match column names of wideData}
#'\item{Kind}{the valueKind of each row}
#'\item{Units}{the unit of that row}
#'\item{Conc}{the tested concentration of that row}
#'\item{ConcUnits}{the tested concenration unit of that row}
#'}
#'
#'@details resultTypes should not have a factor in DataColumn- order breaks
meltWideData <- function(wideData, resultTypes, stateGroups=list(), splitColumn=NULL, splitFunction=NULL) {
  if(is.factor(resultTypes$DataColumn)) {
    stopUser("Column DataColumn in resultTypes should not be a factor")
  }
  
  if (!all(resultTypes$DataColumn %in% names(wideData))) {
    stopUser("All resultTypes$DataColumn must be included in wideData")
  }
  
  # Add a temporary rowID to keep track of how rows match up
  wideData$rowID <- seq(1,length(wideData[[1]]))
  
  if (is.null(splitColumn)) {
    wideData$splitColumnID <- NA
  } else {
    wideData$splitColumnID <- as.numeric(as.factor(do.call(paste0, args=as.list(wideData[splitColumn]))))
  }
  
  if (is.null(splitFunction)) {
    wideData$splitFunctionID <- NA
  } else {
    wideData$splitFunctionID <- splitFunction(wideData, inputFormat, stateGroups, resultTypes)
  }

  # Remove blank columns
  blankSpaces <- lapply(as.list(wideData),function(x) return (x != ""))
  emptyColumns <- unlist(lapply(blankSpaces, sum) == 0)
  resultTypes <- resultTypes[!(resultTypes$DataColumn %in% names(wideData)[emptyColumns]), ]
  
  #Convert the wideData to long format
  longResults <- reshape(wideData, idvar=c("id"), ids=row.names(wideData), v.names="UnparsedValue",
                         times=resultTypes$DataColumn, timevar="resultTypeAndUnit",
                         varying=list(resultTypes$DataColumn), direction="long", drop = names(wideData)[emptyColumns])
  
  # Add the result types information to the long format
  resultTypeRows <- match(longResults$"resultTypeAndUnit", resultTypes$DataColumn)
  
  longResults$valueUnit <- resultTypes$Units[resultTypeRows]
  longResults$concentration <- resultTypes$Conc[resultTypeRows]
  longResults$concentrationUnit <- resultTypes$concUnits[resultTypeRows]
  longResults$valueType <- resultTypes$dataClass[resultTypeRows]
  longResults$publicData <- !resultTypes$hidden[resultTypeRows]
  longResults$time <- resultTypes$time[resultTypeRows]
  longResults$timeUnit <- resultTypes$timeUnit[resultTypeRows]
  longResults$valueKind <- resultTypes$Kind[resultTypeRows]
  
  longResults$"UnparsedValue" <- gdata::trim(as.character(longResults$"UnparsedValue"))
  
  # Parse numeric data from the unparsed values
  matches <- is.na(suppressWarnings(as.numeric(gsub("^(>|<)(.*)", "\\2", gsub(",","",longResults$"UnparsedValue")))))
  longResults$numericValue <- longResults$"UnparsedValue"
  longResults$numericValue[matches] <- ""
  
  # Parse string values from the unparsed values
  longResults$stringValue <- as.character(longResults$"UnparsedValue")
  longResults$stringValue[!matches & longResults$valueType != "stringValue"] <- ""
  
  longResults$clobValue <- as.character(longResults$"UnparsedValue")
  longResults$clobValue[!longResults$valueType=="clobValue"] <- NA
  longResults$stringValue[longResults$valueType=="clobValue"] <- ""
  
  longResults$fileValue <- as.character(longResults$"UnparsedValue")
  longResults$fileValue[!longResults$valueType=="fileValue"] <- NA
  longResults$stringValue[longResults$valueType=="fileValue"] <- ""
  
  # Parse Operators from the unparsed value
  matchExpression <- ">|<"
  longResults$valueOperator <- longResults$numericValue
  matches <- gregexpr(matchExpression,longResults$numericValue)
  regmatches(longResults$valueOperator, matches, invert = TRUE) <- ""
  
  # Turn result values to numeric values
  longResults$numericValue <-  as.numeric(gsub(",", "", gsub(matchExpression, "", longResults$numericValue)))
  
  # For the results marked as "stringValue":
  #   Set the stringValue to the original value
  #   Clear the other categories
  longResults$numericValue[which(longResults$valueType=="stringValue")] <- rep(NA, sum(longResults$valueType=="stringValue", na.rm = TRUE))
  longResults$valueOperator[which(longResults$valueType=="stringValue")] <- rep(NA, sum(longResults$valueType=="stringValue", na.rm = TRUE))
  
  # For the results marked as "dateValue":
  #   Apply the function validateDate to each entry
  longResults$dateValue <- rep(NA, length(longResults$rowID))
  if (length(which(longResults$valueType=="dateValue")) > 0) {
    dateTranslation <- lapply(unique(longResults$UnparsedValue[which(longResults$valueType=="dateValue")]), validateDate)
    names(dateTranslation) <- unique(longResults$UnparsedValue[which(longResults$valueType=="dateValue")])
    longResults$dateValue[which(
      longResults$valueType=="dateValue"
      & !is.na(longResults$UnparsedValue)
      & longResults$UnparsedValue != "")] <- unlist(dateTranslation[longResults$UnparsedValue[which(longResults$valueType=="dateValue" & 
                                                                                                      !is.na(longResults$UnparsedValue))]])
  }
  longResults$numericValue[which(longResults$valueType=="dateValue")] <- rep(NA, sum(longResults$valueType=="dateValue", na.rm=TRUE))
  longResults$valueOperator[which(longResults$valueType=="dateValue")] <- rep(NA, sum(longResults$valueType=="dateValue", na.rm=TRUE))
  longResults$stringValue[which(longResults$valueType=="dateValue")] <- rep(NA, sum(longResults$valueType=="dateValue", na.rm=TRUE))
  
  # Turn empty string into NA
  longResults[longResults==" " | longResults=="" | is.na(longResults)] <- NA
  
  # Remove empty rows
  longResults <- longResults[!(is.na(longResults$numericValue) 
                               & is.na(longResults$stringValue) 
                               & is.na(longResults$valueOperator)
                               & is.na(longResults$dateValue)
                               & is.na(longResults$clobValue)
                               & is.na(longResults$fileValue)), ]
  return(longResults)
}

#'@rdname validate
#'@param expectedFormat a \link{format.POSIXct} that dates should be formatted as
#'@param secondaryFormat a \link{format.POSIXct} that will not throw warnings, and is tested second
#'@return \code{validateDate} a string date in the format expectedFormat
validateDate <- function(inputValue, expectedFormat = "%Y-%m-%d", secondaryFormat = "%m/%d/%Y", errorEnv = NULL) {
  
  returnDate <- ""
  
  if (is.na(inputValue) | inputValue == "") {return (NA)}
  
  # Function to attempt to coerce the date into a given format
  coerceToDate <- function(format, inputValue) {
    # Coerces a string to a given format
    #
    # Args:
    #  format: A character string representing the desired date format. (see ?format.POSIXct)
    #	inputValue: A string representing a date
    # Returns:
    #	A coerced date object or an NA if unable to coerce properly
    return(as.Date(as.character(inputValue), format))
  }
  isInFormat <- function(format, inputValue) {
    # Coerces a string to a given format, and then evaluates whether it is reasonable or not
    #
    # Args:
    #	format: A character string representing the desired date format. (see ?format.POSIXct)
    #	inputValue: A string representing a date
    # Returns:
    #	A boolean as to whether the date is correctly coercible to the given format
    
    # Coerce the date
    coercedDate <- coerceToDate(format, inputValue)
    if(!is.na(coercedDate)) {
      # If the value was coerced then evaluate how many years into the future or in the paste it is
      numYearsFromToday <- as.numeric(format(coercedDate, "%Y")) - as.numeric(format(Sys.Date(), "%Y"))
      if(numYearsFromToday > -50 && numYearsFromToday < 1) {
        # If the date is less than 50 years in the paste or less than 1 year in the future, then it is somewhat reasonable
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Check if can be coerced to the expected format
  if(!isInFormat(expectedFormat, inputValue )) {
    
    # Let the secondary format pass through
    if(isInFormat(secondaryFormat, inputValue)) {
      return(as.character(coerceToDate(secondaryFormat, inputValue)))
    }
    
    #First try substituting out the seperators in the inputValue for those in the expected format
    expectedSeperator <- ifelse(grepl("-",expectedFormat),"-", "/")
    inputValueWExpectedSeperator <- gsub("-|/",expectedSeperator,inputValue)
    
    #Test again with new seperators
    if(!isInFormat(expectedFormat, inputValueWExpectedSeperator)) {
      #This means the value is still not in the expected format, now check for other common formats to see if any of them are reasonable
      commonFormats <- c("%Y-%m-%d","%y-%M-%d","%d-%m-%y","%m-%d-%y","%m-%d-%Y","%b-%d-%Y","%b-%d-%Y")
      formatsAbleToCoerce <- commonFormats[unlist(lapply(commonFormats,isInFormat, inputValue = inputValueWExpectedSeperator))]
      if(length(formatsAbleToCoerce) > 0) {
        # If any of the formats were coercible then we will attempt to pick the best one by getting the one value closest to today
        possibleDates <- do.call("c",lapply(formatsAbleToCoerce, coerceToDate, inputValueWExpectedSeperator))
        possibleDatesInExpectedFormat <- as.Date(format(possibleDates, expectedFormat))
        daysFromToday <- abs(as.Date(format(Sys.Date(), expectedFormat)) - possibleDates)
        minDaysFromToday <- min(daysFromToday)
        bestMatchingDate <- possibleDatesInExpectedFormat[daysFromToday == minDaysFromToday][1]
        
        # Add to the warnings that we coerced the date to a "Best Match"
        warnUser(paste0("A date is not in the proper format. Found: \"",inputValue,"\" This was interpreted as \"",bestMatchingDate, 
                       "\". Please enter dates as YYYY-MM-DD."))
        returnDate <- bestMatchingDate
      } else {
        # If we couldn't parse the data into any of the formats, then we add this to the erorrs and return no date
        addError(paste0("The loader was unable to change the date '", inputValue, 
                        "' to the proper format. Please change it to the format YYYY-MM-DD."),
                 errorEnv = errorEnv)
      }
    } else {
      # If the change in the seperators fixed the issue, then we add this to the warnings and return the coerced date
      warnUser(paste0("A date is not in the proper format. Found: \"",inputValue,"\" This was interpreted as \"",
                     inputValueWExpectedSeperator, 
                     "\". Please enter dates as YYYY-MM-DD."))
      returnDate <- coerceToDate(expectedFormat, inputValueWExpectedSeperator)
    }
  } else {
    # If the date was coercible to the given format with no changes, then good, just return what they gave us as a date
    returnDate <- coerceToDate(expectedFormat, inputValue)
  }
  # Return the date
  return(as.character(returnDate))
}

#'@rdname validate
#'@return \code{validateCharacter} a string
#'@details NA will be allowed through, use other functions to check for NA
validateCharacter <- function(inputValue, errorEnv = NULL) {
  
  # Checks if the entry is NULL
  if (is.null(inputValue)) {
    addError(paste("An entry was expected to be a set of characters but the entry was: NULL"), errorEnv)
    return (NULL)
  }
  
  if(is.na(inputValue)) {
    return(inputValue)
  }
  
  #Checks if the input is similar enough as a character to be interpreted as one
  if (as.character(inputValue)!=inputValue) {
    # If it cannot be coerced to character, throw an error
    if (is.na(as.character(inputValue))) {
      addError(paste("An entry was expected to be a set of characters but the entry was:", inputValue), errorEnv)
    }
    warnUser(paste("An entry was expected to be a set of characters but the entry was:", inputValue))
  }
  # Returns the input as a character
  return(as.character(inputValue))
}
#' Check that strings can be coerced to others
#' 
#' Tests whether a string can be interpreted as given type
#' 
#' @param inputValue value that should able to become the type, usually a string
#' @param errorEnv environment where errorList is stored
#' @name validate
#' @return \code{validateNumeric} a numeric
validateNumeric <- function(inputValue, errorEnv = NULL) {
  
  isCoercibleToNumeric <- !is.na(suppressWarnings(as.numeric(gsub(",", "", as.character(inputValue)))))
  if(!isCoercibleToNumeric) {
    addError(paste0("An entry was expected to be a number but was: '", inputValue, "'. Please enter a number instead."), errorEnv)
  }
  return(suppressWarnings(as.numeric(gsub(",", "", as.character(inputValue)))))
}
#' File Saving (deprecated)
#' 
#' Moves a file to the location for files and saves a reference to that location
#' in the experiment
#' 
#' @param fileStartLocation Path to location to find the source file, relative
#'   from working directory
#' @param experiment experiment object to save file to
#' @param recordedBy username of person saving
#' @param lsTransaction integer of transaction
#' @param fileServiceType "blueimp" or "custom"
#' @param fileService url path to custom file service (will be passed to
#'   customSourceFileMove, which should be defined in customFunctions)
#'   
#' @details Mostly deprecated in favor of
#'   \code{\link{saveAcasFileToExperiment}}, where the fileStartLocation is
#'   relative from privateUploads. In fileRead.R.
#'   
#' @return New file location (or code)
moveFileToExperimentFolder <- function(fileStartLocation, experiment, recordedBy, lsTransaction, 
                                       fileServiceType = racas::applicationSettings$server.service.external.file.type, 
                                       fileService = racas::applicationSettings$server.service.external.file.service.url,
                                       deleteOldFile = TRUE, customSourceFileMove = NULL) {
  
  fileName <- basename(fileStartLocation)
  
  if(fileServiceType == "blueimp") {
    experimentCodeName <- experiment$codeName
    
    experimentFolderLocation <- getUploadedFilePath("experiments")
    dir.create(experimentFolderLocation, showWarnings = FALSE)
    
    fullFolderLocation <- file.path(experimentFolderLocation, experimentCodeName)
    dir.create(fullFolderLocation, showWarnings = FALSE)
    
    targetPath <- file.path("experiments", experimentCodeName, fileName)
  }
  
  serverFileLocation <- moveFileToFileServer(fileStartLocation, targetPath, 
                                             fileServiceType, fileService, experiment, recordedBy, customSourceFileMove = customSourceFileMove)
  
  updateValueByTypeAndKind(serverFileLocation, "experiment", experiment$id, 
                           "metadata", "raw results locations", "fileValue", "source file")
  
  return(serverFileLocation)
}

#' Returns file path for uploaded files 
#'
#' @param inputString File Name
#' @return File path
#' @keywords config, configuration, filePath, fileToParse
#' @export

getUploadedFilePath <- function(inputString) {
  return(file.path(racas::applicationSettings$server.file.server.path, inputString))  
}

#' Get text file contents as character
#'
#' @param file character path to file
#' @return character contents of file
#' @keywords read, text, character, file
#' @export
#' 

get_text_file_contents <- function(file_path) {
  if(file.access(file_path, 4) == 0) {
    text <- readChar(file_path, file.info(file_path)$size)
  } else {
    warning(paste0("no read access to file '",file_path,"'"))
    text <- NULL
  }
  return(text)
}

#' Move file to file server
#'
#' @param fileServiceType "blueimp" or "custom"
#' @param fileStartLocation current location of file, relative from working directory
#' @param targetPath path to new file location, not currently used when fileServiceType == "custom"
#' @param fileService path to file service, not used when fileServiceType == "blueimp"
#' @param experiment experiment object, not used when fileServiceType == "blueimp"
#' @param recordedBy logged in username, not used when fileServiceType == "blueimp"
#' @param customSourceFileMove function to move file to custom server
#' @return character file code or path relative from privateUploads
#' @keywords custom, file, blueimp
#' @export
#' 
moveFileToFileServer <- function(fileStartLocation, targetPath=NULL, 
                                 fileServiceType = racas::applicationSettings$server.service.external.file.type, 
                                 fileService=racas::applicationSettings$server.service.external.file.service.url, 
                                 experiment=NULL, recordedBy=NULL, customSourceFileMove=NULL) {
  # moves a file to file server
  if (fileServiceType == "blueimp") {
    # Move the file, cannot use file.rename across a mounted drive
    file.copy(from=fileStartLocation, to=getUploadedFilePath(targetPath), overwrite = TRUE)
    file.remove(fileStartLocation)
    return(targetPath)
  } else if (fileServiceType == "custom") {
    if(!is.function(customSourceFileMove)) {
      stop(paste0("customSourceFileMove has not been defined in customFunctions.R"))
    }
    fileName <- basename(fileStartLocation)
    return(customSourceFileMove(fileStartLocation, fileName, fileService, experiment, recordedBy))
  } else {
    stopUser("Configuration error: Invalid file service type")
  }
}

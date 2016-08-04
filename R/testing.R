
loadTestData <- function(applicationSettings = racas::applicationSettings, ...) {
  originalWD <- getwd()
  setwd(file.path(Sys.getenv("ACAS_HOME")))
  source(file.path("public/src/modules/GenericDataParser/src/server/generic_data_parser.R"))

  docsDirectory <- system.file("docs", package = "racas")
  docsDirectoryFileList <- list.files(docsDirectory, full.names = TRUE)
  testDataFiles <- docsDirectoryFileList[grepl("SELTESTDATA_", basename(docsDirectoryFileList))]
  answer <- list()
  for(testFile in testDataFiles) {
    if (grepl("\\.xlsx?$",testFile)) {
      tryCatch({
        genericDataFileDataFrame <- read.xls(testFile, header = FALSE, blank.lines.skip = FALSE)
        if (grepl("\\.xlsx$",testFile)) {
          genericDataFileDataFrame <- as.data.frame(sapply(genericDataFileDataFrame,gsub,pattern="&gt;",replacement=">"))
          genericDataFileDataFrame <- as.data.frame(sapply(genericDataFileDataFrame,gsub,pattern="&lt;",replacement="<"))
        }
      }, error = function(e) {
        stopUser("Cannot read input excel file")
      })
    } else if (grepl("\\.csv$",testFile)){
      tryCatch({
        genericDataFileDataFrame <- read.csv(testFile, header = FALSE)
      }, error = function(e) {
        stopUser("Cannot read input csv file")
      })
    } else {
      stopUser("The input file must have extension .xls, .xlsx, or .csv")
    }
    calculatedResults <- getSection(genericDataFileDataFrame, lookFor = "Calculated Results")
    calculatedResults[,1] <- as.character(calculatedResults[,1])
    corpIDSInFile <- calculatedResults[3:nrow(calculatedResults),1]
    calculatedResults[3:nrow(calculatedResults),1] <- getCorporateIDs(n = length(corpIDSInFile), random = FALSE, fillMissing = TRUE)
    
    genericDataFileDataFrame <- sapply(genericDataFileDataFrame, as.character)
    calculatedResults <- sapply(calculatedResults, as.character)
    calculatedResultsStart <- grep("Calculated Results", genericDataFileDataFrame) + 1
    genericDataFileDataFrame[calculatedResultsStart:(nrow(calculatedResults)+calculatedResultsStart-1) ,] <- calculatedResults
    
    selFile <- tempfile(fileext=".csv", tmpdir = file.path(racas::applicationSettings$server.file.server.path))
    write.table(genericDataFileDataFrame, file = selFile, col.names = FALSE, row.names = FALSE, na = "", sep = "," )
    
    # Collect the information from the request
    request <- list()
    request$fileToParse <- selFile
    request$dryRunMode <- FALSE
    request$testMode <- FALSE
    request$reportFile <- NULL
    request$user <- "jmcneil"
    request$recordedby <- "bbolt"

    answer <- list(answer, parseGenericData(request))
  }
  setwd(originalWD)
  return(answer)
}


getCorporateIDs <- function(table = "compound.lot", column = "corp_name", applicationSettings = racas::applicationSettings, service = NULL, n = 100, random = TRUE, fillMissing = FALSE) {
  dbType <- getDBType()
  if(is.null(service)){
    if(random) {
      qu <- switch(dbType,
                   "Oracle" = paste0("SELECT ", table, ".", column, " FROM (SELECT ", table, ".", column, " FROM ", table, " ORDER BY DBMS_RANDOM.VALUE) WHERE ROWNUM < ", n),
                   "Postgres" =  paste0("SELECT ", table, ".", column, " FROM ", table, " ORDER BY RANDOM() LIMIT ",n),
                   "MySQL" = paste0("SELECT ", table, ".", column, " FROM ", table, " ORDER BY RAND() LIMIT ",n)
      )
    } else {
      qu <- switch(dbType,
                   "Oracle" = paste0("SELECT ", table, ".", column, " FROM ", table, " WHERE ROWNUM < ", n, " ORDER BY ", table, ".", column),
                   "Postgres" =  paste0("SELECT ", table, ".", column, " FROM ", table, " ORDER BY ", table, ".", column, " LIMIT ",n),
                   "MySQL" = paste0("SELECT ", table, ".", column, " FROM ", table, " ORDER BY ", table, ".", column, " LIMIT ",n)
      )
    }
    answers <- query(qu)
    
  } else {
    #TODO implement service call
  }
  names(answers) <- "IDS"
  
  if(fillMissing) {
    answers <- data.frame("ID" = rep(answers$ID, length.out = n))
  }
  
  return(as.character(answers$ID))
}

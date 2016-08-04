# Summary Statistics for the ACAS database
# Author: Jennifer Rogers

# generateSummaryStatistics
# Runs functions necessary to generate the summary
# statistics in an HTML file, then generates the file. Also
# writes a CSV file that can be downloaded in a link at
# the bottom of the HTML page.
# Note that the corresponding .rmd file needs to be
#      located in the rmd folder of racas
#
# Input: numWeeks, to be used by several functions
# Output: an HTML file showing the statistics
# Possible error cases: Generally, if api tables are
#        missing or have missing columns
#        Will also err if any of the functions with "Over time"
#        in their name change their return type from NULL
#        when there is no data to graph

generateSummaryStatistics <- function(numWeeks = 4) {
  # Get data frames to make plots
  dbType <- getDBType()
  weeklyStatisticsData <- weeklyStatistics(dbType)
  usageTotals <- as.data.frame(t(colSums(weeklyStatisticsData[ , c('protocol', 'experiment', 'analysis_group_value', 'subject_value'), with = FALSE])))
  
  usageTotals$user <- query("SELECT count(distinct (d.recorded_by)) as \"user\"
                        FROM protocol p
                                      JOIN experiment d on p.id=d.protocol_id
                                      WHERE d.ignored            = '0'
                                      AND d.deleted            = '0'
                                      AND p.ignored            = '0'
                                      AND p.deleted            = '0'")[1,]
  numWeeks <- 4
  history <- experimentHistory(numWeeks = numWeeks)
#   history <- experimentHistoryChart(numWeeks)
#   progress <- detailedExperimentChart()
#   protocols <- protocolsOverTime()
#   experiments <- experimentsOverTime()
#   analysis <- analysisOverTime()
#   subjects <- subjectsOverTime()
#   values <- dataOverTime()
#   numExperiments <- numExperimentsChart()
  #recentUser <- mostRecent(4)    this function is not used, but may be useful in the future
  
  # Set up a factor to arrange the users in order of 
  # the number of experiments they have loaded
#   if (!is.null(history)) {
#     history$recorded_by <- factor(history$recorded_by, levels = unique(history$recorded_by))
#     history$isOld <- factor(history$isOld, levels = c(FALSE, TRUE))
#   }
#   if (!is.null(progress)) {
#     progress$recorded_by <- factor(progress$recorded_by, levels = unique(numExperiments$recorded_by))
#     progress$status <- factor(progress$status, levels = c("Finalized", "Unfinalized"))
#   }

  rmdHome <- system.file("rmd", "summaryStatisticsHome.rmd", package="racas")
  htmlSummary <- knit2html_bug_fix(input = rmdHome, 
                                  options = c("base64_images", "mathjax"),
                                  template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                  stylesheet = system.file("rmd", "summaryStats.css", package="racas"))
  
  rmdGraphs <- system.file("rmd", "summaryStatisticsGraphs.rmd", package="racas")
  htmlGraphs <- knit2html_bug_fix(input = rmdGraphs, 
                                   options = c("base64_images", "mathjax"),
                                   template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                   stylesheet = system.file("rmd", "summaryStats.css", package="racas"))
  
  # Check that the folder exists
  summaryStatisticsFolder <- file.path(racas::applicationSettings$appHome, 'privateUploads', 'summaryStatistics')
  if (!file.exists(summaryStatisticsFolder)) {
    dir.create(summaryStatisticsFolder)
  }
  
  csvPath <- file.path(summaryStatisticsFolder, 'summaryStatistics.csv')
  htmlPath <- file.path(summaryStatisticsFolder, 'summaryStatistics.html')
  htmlGraphPath <- file.path(summaryStatisticsFolder, 'summaryStatisticsGraphs.html')
  
  # Get the data and write to CSV
  summaryTable <- weeklyStatisticsData[ , c('date', 'protocol', 'experiment', 'analysis_group_value', 'subject_value'), with = FALSE]
  setnames(summaryTable, c("WEEK", "PROCOLS_RECORDED", "EXPERIMENTS_RECORDED", "CALCULATED_RESULTS_RECORDED", "RAW_RESULTS_RECORDED"))
  write.csv(summaryTable, csvPath, row.names = FALSE)
  
  writeLines(htmlSummary, con = htmlPath)
  writeLines(htmlGraphs, con = htmlGraphPath)

  return(list(csvFilePath = csvPath,
              htmlFilePath = htmlPath,
              htmlGraphs = htmlGraphPath))
}


# usageStatistics
# Creates a vector of summary statistics for the database:
#    - Total number of experiments
#    - Total number of protocols
#    - Total number of users (not counting "nouser")
# 
# Input: none
# Output: Returns a vector containing the statistics
# Limitations: Includes the user NA, which may or may not be desired
#             
# Possible error cases: api_experiment or api_protocol does not exist
#                      

weeklyStatistics <- function(dbType) {
  queries <- list(
    subject_value = "SELECT EXTRACT(YEAR FROM d.recorded_date) as YEAR, EXTRACT(WEEK FROM d.recorded_date) AS WEEK, count(d.id) as subject_value
                       FROM protocol p
                       JOIN experiment e on p.id=e.protocol_id
                       JOIN experiment_analysisgroup eag ON e.id=eag.experiment_id
                       JOIN analysis_group ag ON eag.analysis_group_id = ag.id
                       JOIN analysisgroup_treatmentgroup agtg ON agtg.analysis_group_id = ag.id
                       JOIN treatment_group tg ON tg.id = agtg.treatment_group_id
                       JOIN treatmentgroup_subject tgs ON tgs.treatment_group_id=agtg.treatment_group_id
                       JOIN subject s ON s.id = tgs.subject_id
                       JOIN subject_state ss ON ss.subject_id = s.id
                       JOIN subject_value d ON d.subject_state_id = ss.id AND d.ls_kind <> 'tested concentration' AND d.ls_kind <> 'batch code' AND d.ls_kind <> 'time'
                       WHERE ag.ignored         = '0'
                       AND e.ignored            = '0'
                       AND e.deleted            = '0'
                       AND p.ignored            = '0'
                       AND p.deleted            = '0'
                       AND d.recorded_date >= TRUNC(sysdate, 'WW')
                       GROUP BY EXTRACT(YEAR FROM d.recorded_date), EXTRACT(WEEK FROM d.recorded_date)
                       order by 1,2 desc",
    analysis_group_value = "SELECT EXTRACT(YEAR FROM d.recorded_date) AS YEAR, EXTRACT(WEEK FROM d.recorded_date) as WEEK, count(d.id) as analysis_group_value
                                  FROM protocol p
                                  JOIN experiment e on p.id=e.protocol_id
                                  JOIN experiment_analysisgroup eag ON e.id=eag.experiment_id
                                  JOIN analysis_group ag ON eag.analysis_group_id = ag.id
                                  JOIN analysis_group_state ags ON ags.analysis_group_id = ag.id
                                  JOIN analysis_group_value d ON d.analysis_state_id = ags.id AND d.ls_kind NOT IN ('batch code','time')
                                  WHERE ag.ignored         = '0'
                                  AND ags.ignored          = '0'
                                  AND d.ignored          = '0'
                                  AND e.ignored            = '0'
                                  AND e.deleted            = '0'
                                  AND p.ignored            = '0'
                                  AND p.deleted            = '0'
                                  AND d.recorded_date >= TRUNC(sysdate, 'WW')
                                  GROUP BY EXTRACT(YEAR FROM d.recorded_date), EXTRACT(WEEK FROM d.recorded_date)
                                  order by 1,2 desc
                                  ",
    experiment = "SELECT EXTRACT(YEAR FROM d.recorded_date) AS YEAR, EXTRACT(WEEK FROM d.recorded_date) as WEEK, count(d.id) as experiment
                        FROM protocol p
                        JOIN experiment d on p.id=d.protocol_id
                        WHERE d.ignored            = '0'
                        AND d.deleted            = '0'
                        AND p.ignored            = '0'
                        AND p.deleted            = '0'
                        AND d.recorded_date >= TRUNC(sysdate, 'WW')
                        GROUP BY EXTRACT(YEAR FROM d.recorded_date), EXTRACT(WEEK FROM d.recorded_date)
                        order by 1,2 desc",
   protocol = "SELECT EXTRACT(YEAR FROM d.recorded_date) AS YEAR, EXTRACT(WEEK FROM d.recorded_date) as WEEK, count(d.id) as protocol
                      FROM protocol d
                      WHERE d.ignored            = '0'
                      AND d.deleted            = '0'
                      AND d.recorded_date >= TRUNC(sysdate, 'WW')
                      GROUP BY EXTRACT(YEAR FROM d.recorded_date), EXTRACT(WEEK FROM d.recorded_date)
                      order by 1,2 desc"
    )
  if(dbType == "Oracle") {
    queries <- lapply(queries, function(x) gsub("EXTRACT\\(WEEK FROM d.recorded_date\\)", "to_char(d.recorded_date - 7/24,'WW')", x))
  }
#   if(!update) {
  if(TRUE) {
      queries <- lapply(queries, function(x) gsub("AND d.recorded_date >= TRUNC\\(sysdate, 'WW'\\)\n", "", x))
  }
  #queries <- queries[-c(1:2)]
  answers <- lapply(queries, function(x) {
    answer <- query(x)
    answer <- as.data.table(answer)
    
    if(nrow(answer) == 0) return(NULL)
    setnames(answer, tolower(names(answer)))
    answer[ , c('week', 'year') := list(as.character(week), as.character(year))]
    setkey(answer, "year", "week")
    return(answer)
  })
  expectedColumns <- c("subject_value", "analysis_group_value", "experiment", "protocol")
  isNULLColumns <- sapply(answers, is.null)
  answers <- answers[!isNULLColumns]
  hasColumn <-  c("subject_value", "analysis_group_value", "experiment", "protocol") %in% names(isNULLColumns)[!isNULLColumns]
  hasColumns <- expectedColumns[hasColumn]
  missingColumns <- expectedColumns[!hasColumn]
  if(length(hasColumns) == 0) {
    answers <- as.data.table(matrix(0, length(expectedColumns),  nrow = 1))
    setnames(answers, expectedColumns)
    answers[  , c("year", "week") :=  list(format(Sys.time(), foramt = "%Y"), format(Sys.time(), foramt = "%W"))]
  } else {
    answers <- Reduce(function(x,y) {
      ans <- merge(x,y, all = TRUE)
      setkey(ans, 'week', 'year')
      return(ans)
    }
    ,answers)
    if(length(missingColumns) > 0) {
      answers[ , missingColumns := 0, with = FALSE]
    }
  }
  answers[ , date := as.Date(paste(paste0(year,"-",week,"-1")),"%Y-%U-%u")]
  setkey(answers,"date", "year", "week")
  regularSequence <- data.table(date = seq(answers[1]$date, answers[nrow(answers)]$date, by='1 week'))
  regularSequence[ , c('year', 'week') := list(format(date, "%Y"), strftime(date,format="%W")) ]
  setkey(regularSequence, "date", "year", "week")
  answers <- answers[regularSequence, allow.cartesian = TRUE]
  answers[is.na(answers)] <- 0
#   min <- answers[1, c("year", "week"), with = FALSE]
#   max <- answers[nrow(answers), c("year", "week"), with = FALSE]
  #answers[ , c("year","week") := NULL]
  #answers[ , date:= NULL]
#   timeSeries <- ts(answers,frequency = 52, start = c(min$year, as.numeric(min$week)), end = c(max$year, as.numeric(max$week)))

#   plot(timeSeries, nc = 1)
#   
#   numProtocols <- query("select count(id) from protocol where deleted = '0' and ignored = '0'")[1,1]
#   numExperiments <- query("select count(id) from exeriment  where deleted = '0' and ignored = '0'")[1,1]
#   numUsers <- query("select count(distinct ev.code_value) from experiment_value ev join experiment_state es on ev.experiment_state_id=es.id join experiment e on es.experiment_id=e.id where ev.ls_kind = 'scientist' and ev.deleted='0' and ev.ignored = '0' and es.deleted = '0' and es.ignored = '0' and e.deleted = '0' and e.ignored = '0'")[1,1]
#   numSubjects <- query("select count(id) from subject where subject.deleted = '0'")[1,1]
#   numAnalysisGroups <- query("select count(agv.id) from api_analysis_group_results")[1,1]
  return(answers)
}


# experimentHistoryChart 
# Returns data to create a bar chart that displays the number of experiments for each user,
#    broken down by whether the records are older or newer than a user-specified
#    number of weeks
# 
# Input: 
#    numWeeks - A double or int, greater than zero, which partitions the experiments.
#               It partitions them into records that are newer than
#               numWeeks, and experiments that are older than numWeeks.
#               numWeeks defaults to 4.
# Output: Returns the data frame necessary to graph the number of old and
#         new experiments per user
#         NULL if there is no data
# Limitations:
#   Gives nonsensical legends if numWeeks is not positive
#   Returns nothing if there is no data in api_experiment
# Possible error cases: api_experiment does not exist, or does not contain
#   columns "recorded_by" and "recorded_date"
experimentHistory <- function(numWeeks) {
  experiments <-  as.data.table(query("SELECT d.id, d.recorded_date, MAX( CASE ev.ls_kind WHEN 'experiment status' THEN ev.code_value ELSE null END ) AS status, d.recorded_by
                                      FROM protocol p
                                      JOIN experiment d on p.id=d.protocol_id
                                      JOIN experiment_state es on es.experiment_id = d.id
                                      JOIN experiment_value ev on ev.experiment_state_id = es.id
                                      WHERE d.ignored            = '0'
                                      AND d.deleted            = '0'
                                      AND p.ignored            = '0'
                                      AND p.deleted            = '0'
                                      GROUP BY d.id, d.recorded_date, d.recorded_by
                                      "))
  if(nrow(experiments) == 0) {
    return(NULL)
  }
  setnames(experiments,tolower(names(experiments)))
  
  #Get a time/date format that was numWeeks ago
  cutoffDate <- Sys.time() - as.difftime(numWeeks, units = "weeks")
  
  experiments[ , week := as.numeric(format(experiments$recorded_date, "%U"))]
  experiments[ , year := as.numeric(format(experiments$recorded_date, "%Y"))]
  experiments[ , userCount := .N, by = recorded_by]
  experiments[ , isOld := TRUE]
  experiments[recorded_date >= cutoffDate, isOld := FALSE]
  
  experiments[ , finalizationStatus := "Unfinalized"]
  experiments[ status %in% c("approved", "rejected", "deleted"), finalizationStatus := "Finalized"]

  setorder(experiments, -userCount)
  experiments$recorded_by <- factor(experiments$recorded_by, levels = unique(experiments$recorded_by))
  experiments$isOld <- factor(experiments$isOld, levels = c(FALSE, TRUE))
  
  
  return(experiments)
}
experimentHistoryChart <- function(numWeeks = 4) {
  userFrame <- query("select recorded_by, recorded_date from api_experiment")
  
  if(NROW(userFrame) == 0) 
    return(NULL)
  
  names(userFrame) <- tolower(names(userFrame))
  
  #Get a time/date format that was numWeeks ago
  cutoffDate <- Sys.time() - as.difftime(numWeeks, units = "weeks")
  
  #Create a column to separate old experiments from new experiments
  userFrame <- transform(userFrame, isOld = ifelse(recorded_date < cutoffDate, TRUE, FALSE))
  
  # Change all missing entries to "None"
  nouserList <- which(userFrame$recorded_by == 'nouser')
  userFrame$recorded_by[nouserList] <- 'None'
  
  # Change all missing entries to "Other"
  naList <- which(is.na(userFrame$recorded_by))
  userFrame$recorded_by[naList] <- 'Other'

  return(userFrame)
}


# detailedExperimentChart
# Returns data to plot the number of experiments (finalized
# and total) for each user
# 
# Input: none
# Output: a data frame that can be used to plot the number of
#         finalized and unfinalized experiments per user
#         NULL if there is no data
# Limitations: Returns nothing if there is no data in api_experiment
# Possible error cases: api_experiment does not exist, or does not contain
#    the columns "status" or "recorded_by"

detailedExperimentChart <- function() {
  userFrame <- query("select recorded_by, status from api_experiment")
  
  if(NROW(userFrame) == 0) 
    return(NULL)
  
  names(userFrame) <- tolower(names(userFrame))
  
  # Change all statuses to either 'Finalized' or 'Unfinalized'  
  unfinalizedList <- which(userFrame$status != 'Finalized')
  userFrame$status[unfinalizedList] <- 'Unfinalized'
  
  # Change all missing entries to "Other"
  naList <- which(is.na(userFrame$recorded_by))
  userFrame$recorded_by[naList] <- 'Other'
  
  # Change all unrecorded entries to "None"
  nouserList <- which(userFrame$recorded_by == 'nouser')
  userFrame$recorded_by[nouserList] <- 'None'
  
  return(userFrame)
}

# protocolsOverTime
# Returns data to plot the total number of protocols over time
# 
# Input: none
# Output: The data table needed to plot the cumulative protocols
#         over time
#         NULL if there is no data

protocolsOverTime <- function() {
  groupFrame <- query("select distinct(protocol_id), recorded_date 
                      from api_protocol")
  if(NROW(groupFrame) == 0) 
    return(NULL)
  
  names(groupFrame) <- tolower(names(groupFrame))
  groupAndDate <- data.table(groupFrame)
  
  setkey(groupAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- groupAndDate[, NROW(protocol_id), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}

# experimentsOverTime
# Returns data to plot the total number of experiments over time
# 
# Input: none
# Output: The data table needed to plot the cumulative experiments
#         over time
#         NULL if there is no data

experimentsOverTime <- function() {
  groupFrame <- query("select distinct(id), recorded_date 
                      from api_experiment")
  if(NROW(groupFrame) == 0) 
    return(NULL)
  
  names(groupFrame) <- tolower(names(groupFrame))
  groupAndDate <- data.table(groupFrame)
  
  setkey(groupAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- groupAndDate[, NROW(id), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}

# analysisOverTime
# Returns data to plot the total number of analysis groups over time
# 
# Input: none
# Output: The data table needed to plot the cumulative analysis
#         groups over time
#         NULL if there is no data
# Possible error cases:  api_analysis_group_results does not 
#         contain the ag_id or recorded_date fields

analysisOverTime <- function() {
  groupFrame <- query("select distinct(ag_id), recorded_date 
                                   from api_analysis_group_results")
  if(NROW(groupFrame) == 0) 
    return(NULL)
  
  names(groupFrame) <- tolower(names(groupFrame))
  groupAndDate <- data.table(groupFrame)
  
  setkey(groupAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- groupAndDate[, NROW(ag_id), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}


# subjectsOverTime
# Returns data to plot the total number of subjects over time
# 
# Input: none
# Output: Returns the data table needed to plot a cumulative
#         graph of subjects over time
#         NULL if there is no data
# Possible error cases: subject does not exist, or does not contain
#    id and recorded_date fields

subjectsOverTime <- function() {
  subjectFrame <- query("select distinct(subject_code_name), recorded_date 
                        from api_subject_results")
  
  if(NROW(subjectFrame) == 0) 
    return(NULL)
  
  names(subjectFrame) <- tolower(names(subjectFrame))
  subjectAndDate <- data.table(subjectFrame)
  
  setkey(subjectAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- subjectAndDate[, NROW(subject_code_name), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}


# dataOverTime
# Returns data to plot the total number of values in subjects over time
# 
# Input: none
# Output: Returns the data table necessary to plot the cumulative number
#         of values in subjects, as a function of time
#         NULL if there is no data
# Possible error cases: api_subject_results table does not exist, or does
#    not contain sv_id and recorded_date fields

dataOverTime <- function() {
  dataFrame <- query("select sv_id, recorded_date from api_subject_results")
  
  if(NROW(dataFrame) == 0) 
    return(NULL)
  
  names(dataFrame) <- tolower(names(dataFrame))
  dataAndDate <- data.table(dataFrame)
  
  setkey(dataAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- dataAndDate[, NROW(sv_id), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}


# mostRecent
# Returns the user with the most new experiments in the last numWeeks
# 
# Input: numWeeks, to determine what counts as a "new" experiment
# Output: Returns a one-element vector containing (a string of) the name 
#         of the user with the most experiments recorded in the past numWeeks
#         NULL if there is no data
# Limitations: If there is a tie, it is broken in favor of the username
#              farther in the alphabet
# Possible error cases: api_experiment may not exist
#

mostRecent <- function(numWeeks = 4) {
  userFrame <- query("select recorded_by, recorded_date from api_experiment 
                     where recorded_by != 'nouser'")
  
  if(NROW(userFrame) == 0) 
    return(NULL)
  
  names(userFrame) <- tolower(names(userFrame))
  
  #Get a time/date format that was numWeeks ago
  cutoffDate <- Sys.time() - as.difftime(numWeeks, units = "weeks")
  
  userFrame <- transform(userFrame, age = ifelse(recorded_date < cutoffDate, "old", "new"))
  
  # This data table has users, date recorded, and age
  userTable <- data.table(userFrame)
  setkey(userTable, age, recorded_by)
  
  # Find the total number of 'new' and 'old' entries per user
  summaryTable <- userTable[, NROW(recorded_date), by = "age,recorded_by"]
  newTable <- summaryTable["new",]
  
  # V1 is the summarized column; this sorts in ascending order
  setkey(newTable, "V1")
  
  bestEntry <- last(newTable)
  if(is.na(bestEntry$recorded_by))  # There must have only been old entries
    return("None")
  else
    return(bestEntry$recorded_by) 
}


# Returns a table of users and their number of experiments
# 
# Input: none
# Output: a data table with a column of users and a column with
#         the number of experiments they have recorded
# Limitations: Includes the user "NA", which may or may not be desired
# Possible error cases: the appropriate tables don't exist

numExperimentsChart <- function() {
  userFrame <- query("select recorded_by, recorded_date from api_experiment")
  
  if(NROW(userFrame) == 0) 
    return(NULL)
  
  names(userFrame) <- tolower(names(userFrame))
  
  # Change all missing entries to "Other"
  naList <- which(is.na(userFrame$recorded_by))
  userFrame$recorded_by[naList] <- 'Other'
  
  # Change all unrecorded entries to "None"
  nouserList <- which(userFrame$recorded_by == 'nouser')
  userFrame$recorded_by[nouserList] <- 'None'
  
  userTable <- data.table(userFrame)
  setkey(userTable, recorded_date)
  sumTable <- userTable[, NROW(recorded_date), by = recorded_by]
  
  # Order from most experiments to least experiments
  setkey(sumTable, V1)
  sumTable <- sumTable[order(-V1)]
  
  return (sumTable)
}


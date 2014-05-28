# Summary Statistics for the ACAS database
# Author: Jennifer Rogers

# generateHTML
# Runs functions necessary to generate the summary
# statistics in an HTML file, then generates the file
#
# Input: numWeeks, to be used by several functions
# Output: an HTML file showing the statistics
# Possible error cases: Generally, if api tables are
# missing or have missing columns

generateHTML <- function(numWeeks = 4) {
  
  #TODO: Handle cases when there is no data appropriately
  
  # Get data frames to make plots
  history <- experimentHistoryChart(4)
  subjects <- subjectsOverTime()
  recentUser <- mostRecent(4)
  analysis <- analysisOverTime()
  numExpProtUsers <- usageStatistics()
  values <- dataOverTime()

  rmd <- system.file("rmd", "summaryStatistics.rmd", package="racas")
  htmlSummary <- knit2html.bugFix(input = rmd, 
                                  options = c("base64_images", "mathjax"),
                                  template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                  stylesheet = system.file("rmd", "racas_container.css", package="racas"))
  writeLines(htmlSummary, con = '~/Desktop/output.html')
  return(htmlSummary)
}


# experimentHistoryChart 
# Returns data to create a bar chart that displays the number of experiments for each user,
#    broken down by whether the records are older or newer than a user-specified
#    number of weeks
# 
# Input: 
#    numWeeks - A double, greater than zero, which partitions the experiments.
#               It partitions them into records that are newer than
#               numWeeks, and experiments that are older than numWeeks.
#               numWeeks defaults to 4.
# Output: Returns the data frame necessary to graph the number of old and
#         new experiments per user
# Limitations:
#   Gives nonsensical legends if numWeeks is not positive
#   Returns nothing if there is no data in api_experiment
# Possible error cases: api_experiment does not exist, or does not contain
#   columns "recorded_by" and "recorded_date"

experimentHistoryChart <- function(numWeeks = 4) {
  userFrame <- query("select recorded_by, recorded_date from api_experiment")
  names(userFrame) <- tolower(names(userFrame))
  
  if(NROW(userFrame) == 0) 
    return()
  
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
  
  #TODO: Try to make a dodged plot with 0's
  return(userFrame)
}


# subjectsOverTime
# Returns data to plot the total number of subjects over time
# 
# Input: none
# Output: Returns the data frame needed to plot a cumulative
#         graph of subjects over time
# Possible error cases: subject does not exist, or does not contain
#    id and recorded_date fields

subjectsOverTime <- function() {
  subjectFrame <- query("select distinct(subject_code_name), recorded_date 
                                     from api_subject_results")
  names(subjectFrame) <- tolower(names(subjectFrame))
  subjectAndDate <- data.table(subjectFrame)
  
  if(NROW(subjectAndDate) == 0) 
    return(c("None"))
  
  setkey(subjectAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- subjectAndDate[, NROW(subject_code_name), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}


# mostRecent
# Returns the user with the most new experiments in the last numWeeks
# 
# Input: numWeeks, to determine what counts as a "new" experiment
# Output: Returns a one-element vector containing (a string of) the name 
#         of the user with the most experiments recorded in the past numWeeks
# Limitations: If there is a tie, it is broken in favor of the username
#              farther in the alphabet
#              Returns nothing if there are no named users in api_experiment
# Possible error cases: api_experiment may not exist
#

mostRecent <- function(numWeeks = 4) {
  userFrame <- query("select recorded_by, recorded_date from api_experiment 
                     where recorded_by != 'nouser'")
  
  if(NROW(userFrame) == 0) 
    return()
  
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


# analysisOverTime
# Returns data to plot the total number of analysis groups over time
# 
# Input: none
# Output: The data frame needed to plot the cumulative analysis
#         groups over time
# Possible error cases:  analysis_group does not exist

analysisOverTime <- function() {
  groupFrame <- query("select distinct(ag_id), recorded_date 
                                   from api_analysis_group_results")
  names(groupFrame) <- tolower(names(groupFrame))
  groupAndDate <- data.table(groupFrame)
  
  if(NROW(groupAndDate) == 0) 
    return(c("None"))
  
  setkey(groupAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- groupAndDate[, NROW(id), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}


# usageStatistics
# Creates a vector of summary statistics for the database:
#    - Total number of experiments
#    - Total number of protocols
#    - Total number of users
# 
# Input: none
# Output: Returns a vector containing the statistics
# Limitations: Includes the users "nouser" and NA, which may or may not be desired
#             
# Possible error cases: api_experiment or api_protocol does not exist
#                      

usageStatistics <- function() {
  numExperiments <- query("select count(distinct id) from api_experiment")[1,1]
  numProtocols <- query("select count(distinct protocol_id) from api_protocol")[1,1]
  numUsers <- query("select count(distinct recorded_by) from api_experiment")[1,1]
  
  return(c(numExperiments, numProtocols, numUsers))
}


# dataOverTime
# Returns data to plot the total number of values in subjects over time
# 
# Input: none
# Output: Returns the data frame necessary to plot the cumulative number
#         of values in subjects, as a function of time
# Possible error cases: api_subject_results table does not exist, or does
#    not contain sv_id and recorded_date fields

dataOverTime <- function() {
  dataAndDate <- data.table(query("select sv_id, recorded_date 
                                  from api_subject_results"))
  
  if(NROW(dataAndDate) == 0) 
    return(c("None"))
  
  setkey(dataAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- dataAndDate[, NROW(sv_id), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  return(dateTable)
}
# Plots the total number of values in subjects over time
# 
# Input: none
# Output: Displays and returns a line graph
# Possible error cases: subject_value table does not exist, or does
#    not contain id and recorded_date fields

dataOverTime <- function() {
  dataAndDate <- data.table(query("select sv_id, recorded_date 
                                  from api_subject_results"))
  
  if(NROW(dataAndDate) == 0) 
    return(c("None"))
  
  setkey(dataAndDate, recorded_date)
  
  # We get a two-column table, with the date and the total number of groups
  dateTable <- dataAndDate[, NROW(sv_id), by = recorded_date]
  dateTable <- within(dateTable, cumulativeSum <- cumsum(V1))
  
  rmd <- system.file("rmd", "summaryStatistics.rmd", package="racas")
  htmlSummary <- knit2html.bugFix(input = rmd, 
                                  options = c("base64_images", "mathjax"),
                                  template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                  stylesheet = system.file("rmd", "racas_container.css", package="racas"))
  writeLines(htmlSummary, con = '~/Desktop/output.html')
  return(htmlSummary)
}
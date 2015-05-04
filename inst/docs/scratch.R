
library(racas)
library(data.table)
library(gdata)
library(drc)
library(xtable)

#file <- system.file("docs", "doseResponseRequest.json", package = "racas")
#file <- "inst/docs/doseResponseRequest.json"
#fitSettingsJSON <- readChar(file, file.info(file)$size)
#curveids <- as.character(query("select curveid from api_curve_params")[[1]])

#file <- system.file("docs", "simpleBulkDoseResponseFitRequest.json", package = "racas")
file <- "inst/docs/example-simple-fitsettings-ll4.json"
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleBulkDoseResponseFitRequest <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
fitSettingsJSON <- simple_to_advanced_fit_settings(simpleBulkDoseResponseFitRequest)
curveids <- as.character(query("select curveid from api_curve_params")[[1]])
fitData <- getFitData.curve(curveids)
system.time(response <- dose_response_session(fitSettingsJSON, curveids = curveids))
parsedResponse <- fromJSON(response)
session <- parsedResponse$sessionID
loadSession(session)


file <- "inst/docs/example-simple-fitsettings-ll4.json"
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleBulkDoseResponseFitRequest <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
fitSettings <- simple_to_advanced_fit_settings(simpleBulkDoseResponseFitRequest)
experimentCode <- "EXPT-00000049"
fitData <- get_fit_data_experiment_code(experimentCode)
fitData <- dose_response(fitSettings, fitData)
curveid <- fitData[1]$curveid
response <- dose_response_session(fitSettings, curveids = curveid)


#Experiment Code to curveCuratorThumbs




#DNET sampling
source("inst/docs/scratchPrivate.R")
system.time(fitData <- getRandomDNETCurves(100))
fitDataBackup <- fitData

fitData <- fitDataBackup
file <- "inst/docs/simpleBulkDoseResponseFitRequest.json"
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleBulkDoseResponseFitRequest <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
fitSettingsJSON <- toJSON(simpleToAdvancedBulkFitRequest(simpleBulkDoseResponseFitRequest))
fitSettings <- fromJSON(fitSettingsJSON)
system.time(response <- dose_response_session(fitSettings, fitData = fitData))
parsedResponse <- fromJSON(response)
session <- parsedResponse$sessionID
loadSession(session)
fitData[ , DNETCategory := getDNETCategory(results.parameterRules, inactive, fitConverged, insufficientRange), by = curveid]
fitData[,actualDNETCategory:=rbindlist(fitData$parameters)$resultcomment]
blah <- fitData[, c("curveid","category","actualDNETCategory"), with = FALSE][category!=actualDNETCategory, ]

for(i in blah$curveid) {
  cat(paste0("New Category: ", fitData[curveid==i,]$category,"\n"))
  cat(paste0("Old Category: ", fitData[curveid==i,]$actualDNETCategory,"\n"))
  plot(fitData[curveid==i,]$model[[1]])
  readline("next:")
}



#pointData <- fitData[1]$points[[1]][flagChanged==TRUE,]
#pointData <- fitDataBefore[8]$points[[1]]
#pointData <- rbindlist(fitData$points)[!is.na(flag),]
pointData <- rbindlist(fitData$points)[flagChanged==TRUE,]
changed <- merge(rbindlist(fitDataBefore$points),pointData, by = "response_sv_id")[, c("flag.x", "flag.y"), with = FALSE]
update_point_flags(pointData, "bbolt")

#Knockout random sample/unkn
updateFlags <- rbindlist(fitData$points)
randomRows <- updateFlags[sample(nrow(updateFlags), 100), ]
#randomRows <- updateFlags
randomRows <- randomRows[, c("curveid","flag","response_sv_id"), with = FALSE]
randomRows[sample(nrow(randomRows), 50), flag := "user"]
randomRows[flag != "user", flag := as.character(NA)]
#randomRows[, flag := as.character(NA)]
setnames(randomRows, "response_sv_id", "id")
file <- system.file("docs", "doseResponseRequest.json", package = "racas")
fitSettingsJSON <- readChar(file, file.info(file)$size)
fitSettingsJSON <- fromJSON(fitSettingsJSON)
fitSettingsJSON$updateFlags <- randomRows
#fitSettingsJSON$updateFlags <- randomRows[ , ifelse(is.na(flag), list(list(list(curveid=curveid,flag=NULL,id=id))), list(list(list(curveid=curveid,flag=flag,id=id)))) ,by = id]$V1
writeLines(toJSON(fitSettingsJSON), con = "inst/docs/doseResponseRequest.json")
#writeLines(gsub("\"NA\"","null",toJSON(fitSettingsJSON)), con = "inst/docs/doseResponseRequest.json")
#response <- dose_response_session(fitSettingsJSON, sessionID = fromJSON(response)$sessionID)


##Profiling
system.time(blah <- profr(blah <- get_point_stats(fitData[1]$points[[1]]), interval=.0001))
plot(blah)

#4 Parameter Dose Response from fitData
data("exampleFitData", package = "racas")
file <- system.file("docs", "doseResponseRequest.json", package = "racas")
fitSettingsJSON <- readChar(file, file.info(file)$size)
fitSettings <- fromJSON(fitSettingsJSON)
response <- dose_response_session(fitSettings, fitData = fitData)
parsedResponse <- fromJSON(response)
session <- parsedResponse$sessionID
loadSession(session)
fitData[1]$reportedParameters[[1]]



#2 Parameter Michaelis Menton
load("/Users/bbolt/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/A14981AA-144F-45B7-AD7F-9918D21D3305/fitData.rda")
fitData[, fixedParameters := list(list())]
fitData[, points := list(list(points[[1]][,flag := FALSE])), by = curveid]
file <- system.file("docs", "doseResponseRequest-kd.json", package = "racas")
fitSettingsJSON <- readChar(file, file.info(file)$size)
fitSettings <- fromJSON(fitSettingsJSON)
response <- dose_response_session(fitSettings, fitData = fitData)
parsedResponse <- fromJSON(response)
session <- parsedResponse$sessionID
loadSession(session)
fitData[1]$reportedParameters[[1]]

#Sam's Examples
data("analysisGroupValues")
data("pointData")

##Profiling
Rprof()
response <- dose_response_session(fitSettingsJSON, curveids = curveids)
Rprof(NULL)
prof <- parse_rprof("Rprof.out")
plot(prof)







fitData[fitConverged == TRUE, {
                                fittedParams <- fittedParameters[[1]]
                                #names(fittedParams) <- paste0("fitted_",names(fittedParams))
                                plotCurve(points[[1]], 
                                          as.data.frame(c(currentTime = as.numeric(format(Sys.time(), "%s%S3")),curveid = curveid, name = curveid,fittedParams)), 
                                          fitFunction = LL4, 
                                          paramNames = c("slope", "min", "max", "ec50"), 
                                          logDose = TRUE, 
                                          drawIntercept = "ec50", 
                                          showLegend = TRUE, 
                                          outFile = paste0(curveid,".png"), 
                                          xmin = NA, ymin = NA, ymax = NA)}, by = curveid]




options(digits.secs = 4)
as.character(format(Sys.time(), "%OS3"))
currentTime <- paste0(as.character(format(Sys.time(), "%s")),strsplit(as.character(format(Sys.time(), "%OS3")),"\\.")[[1]][2])
as.numeric(format(Sys.time(), "%Y"))






me <- print(xtable(as.data.frame(fitData[1]$reportedParameters[[1]]), display = rep("E", 6), caption = fitData[1]$category), 
            caption.placement = "top",
            type = "html", 
            include.rownames = FALSE, 
            comment = FALSE, 
            timestamp = FALSE, 
            rotate.rownames = TRUE, 
            file = "~/Desktop/test.html",
            html.table.attributes = "") 
# 
# 
# times <- 20
# fitDat <- fitData
# for(i in 1:times) {
#   fits <- copy(fitData)
#   fits <- fits[ ,curveid := paste0(curveid,i)]
#   fitDat <- rbind(fitDat, fits)
# }
# fitData <- fitDat
# fitData <- fitData[1:30]
# 
# #Notes
# #Rendering Hint -> Fit Model
# 
# #Fit
# #stdErr, tValue or pValue options
# 
# 
# 
# myfailSettings.parameter <- list(list(parameter = "max", type = "stdErr", value = 3, operator = ">"),list(parameter = "kd", type = "pValue", value = 0.6, operator = ">"))
# myfailSettings.interpolatedValue <- list(kdThreshold = list(parameter = "kd", reference = "dose.max", type = "logAboveReference", value = 0.5, operator = ">"),list(parameter = "max", type = "threshold", value = 30, operator = ">"))
# myfailSettings.inactive <- list(threshold = 5, activeDoses = 2, mockControls = FALSE)
# myfixedParameters <- list(max = NA, kd = NA)
# fitData[ , c("failSettings.parameter", "failSettings.interpolatedValue", "failSettings.inactive", "fixedParameters") := list(list(myfailSettings.parameter),
#                                                                                                                              list(myfailSettings.interpolatedValue),
#                                                                                                                              list(myfailSettings.inactive),
#                                                                                                                              list(myfixedParameters))]
# system.time(fitData <- dose_response_fit(fitData))
# fitData[grepl("max.fail.interpolate",failResults.interpolatedValues), c("model.sync","fixedParameters") := list(model.sync = FALSE,
#                                                                                                                 list(myfixedParameters = list(max = 10, kd = NA)))
#         ]
# fitData <- dose_response_fit(fitData)
# 
# fitData[fitConverged == FALSE, c("model.sync","renderingHint", "fixedParameters") := list(model.sync = FALSE,
#                                                                                           renderingHint = "3 parameter Michaelis Menten",
#                                                                                           list(myfixedParameters = list(NA, NA, NA)))
#         ]
# system.time(fitData <- dose_response_fit(fitData))
# 
# 
# 
# fitData[fitConverged == TRUE, {png(file = paste0(curveid,".png"))
#                                plot(model[[1]], main = paste0(category,"\nkd: ",
#                                                               round(fittedParameters[[1]]$kd,2),
#                                                               " max: ",round(fittedParameters[[1]]$max,2),
#                                                               "\nmax stderr: ", round(goodnessOfFit.parameters[[1]]$max.stdErr,2),
#                                                               " kd pValue: ", round(goodnessOfFit.parameters[[1]]$kd.pValue,2)), ylim = c(0, 40))
#                                dev.off()}, by = curveid]
# 
# fitData[fitConverged == TRUE, plotData(points[[1]], as.data.frame(c(curveid = curveid, name = curveid,fittedParameters[[1]])), MM2, paramNames = c("kd","max"), logDose = TRUE, drawIntercept = "kd", showLegend = TRUE, outFile = paste0(curveid,".png"), xmin = 1, ymin = 0, ymax = 40), by = curveid]


lettersToRow <- function(wellRefs) {
  letts <- strsplit(wellRefs,"", perl = TRUE)
  row <- which(toupper(letters) == let) + 26*(pos-1)
}



finalData <- unique(fitdata[, c("exptno", "curveid"), with = FALSE])
setnames(finalData, c("curveid", "tested_lot"))
finalData[,renderingHint := "2 parameter Michaelis Menten"]
points <- split(fitdata, fitdata$exptno)
points <- data.table(curveid = names(points), points = points)
finalData <- merge(finalData, points, by = "curveid")


#KD
experimentCode <- "EXPT-00000141"
fitData <- get_fit_data(experimentCode)
file <- system.file("docs", "example-simple-fitsettings-mm2.json", package = "racas")
simpleSettingsJSON <- readChar(file, file.info(file)$size)
simpleSettings <- fromJSON(simpleSettingsJSON)
fitSettings <- simple_to_advanced_fit_settings(simpleSettings, modelHint = "MM.2")
#ERROR
fitData <- dose_response(fitSettings, fitData)
myMessenger$capture_output("response <- api_doseResponse_fitData_to_curveDetail(fitData, saved = FALSE, sessionID = doseResponse$sessionID)", userError = "Error converting Fit to a Response", continueOnError = FALSE)
for(i in fitData$curveid) {
  cat(paste0("New Category: ", fitData[curveid==i,]$category,"\n"))
  plot(fitData[curveid==i,]$model[[1]])
  readline("next:")
}

  fitData[fitConverged == TRUE, {
    fittedParams <- fittedParameters[[1]]
    #names(fittedParams) <- paste0("fitted_",names(fittedParams))
    plotCurve(points[[1]][ , curveid := curveid], 
              as.data.frame(c(currentTime = as.numeric(format(Sys.time(), "%s%S3")),curveid = curveid, name = curveid,fittedParams)), 
              fitFunction = LL4, 
              paramNames = c("slope", "min", "max", "ec50"), 
              logDose = TRUE, 
              drawIntercept = "ec50", 
              showLegend = TRUE, 
               outFile = paste0(curveid,".png"), 
              xmin = NA, ymin = NA, ymax = NA)
#     cat(paste0("New Category: ", category,"\n"))

#       readline("next:")

    }, by = curveid]


#
file <- system.file("tests","data", "doseResponse","default-ec50-fitSettings.json", package = "racas")
fitSettings <- fromJSON(readChar(file, file.info(file)$size))
load(system.file("tests","data", "doseResponse","example-ec50-fitData.rda", package = "racas"))
fitData <- fitData[1]
#Need to copy fitData so we are working with our own copy (data.table does objects by reference)
fitData <- copy(fitData)

#Extract the fit variables from json
myFixedParameters <- fitSettings$fixedParameters
myParameterRules <- fitSettings$parameterRules
myInactiveRule <- fitSettings$inactiveRule
myInverseAgonistMode <- ifelse(is.null(fitSettings$inverseAgonistMode), TRUE, fitSettings$inverseAgonistMode)
myBiphasicRule <- fitSettings$biphasicRule
myUserFlag <- ifelse(is.null(fitSettings$user_flag), "NA", fitSettings$user_flag)
fitData[ , fixedParameters := list(list(myFixedParameters))]
fitData[ , parameterRules := list(list(myParameterRules))]
fitData[ , inactiveRule := list(list(myInactiveRule))]
fitData[ , inverseAgonistMode := myInverseAgonistMode]
fitData[ , biphasicRule := list(list(myBiphasicRule))]

#Update all of the flags to those that are in the fitSettings json
updateFlags <- as.data.table(fitSettings$updateFlags)
if(nrow(updateFlags) > 0 ) {
  #updateFlags[flag=="NA", flag := as.character(NA)]
  setkey(updateFlags,"response_sv_id" )
  #First fix issues with updateFlags (they may come in with "NA" instead of NA and logical istead of character)
  updateFlags[ flag_user == "NA", flag_user := as.character(NA)]
  updateFlags[ flag_on.load == "NA", flag_on.load := as.character(NA)]
  updateFlags[ flag_algorithm == "NA", flag_algorithm := as.character(NA)]
  updateFlags[ , flag_user := as.character(flag_user)]
  updateFlags[ , flag_on.load := as.character(flag_on.load)]
  
  #pts <- fitData[1]$points[[1]]
  fitData[, points := list(list(update_point_flags(points[[1]], updateFlags))) , by = curveid]
}

#Fit the data
fitData <- dose_response_fit(fitData)




#Newer stuff:
#PLOT FITDATA curves individually
fitData[fitConverged == TRUE, {
  fittedParams <- fittedParameters[[1]]
  #names(fittedParams) <- paste0("fitted_",names(fittedParams))
  plotCurve(points[[1]][ , curveid := curveid], 
            as.data.frame(c(currentTime = as.numeric(format(Sys.time(), "%s%S3")),curveid = curveid, name = curveid,fittedParams)), 
            fitFunction = LL4, 
            paramNames = c("slope", "min", "max", "ec50"), 
            logDose = TRUE, 
            drawIntercept = "ec50", 
            showLegend = TRUE, 
            outFile = paste0(curveid,".png"), 
            xmin = NA, ymin = NA, ymax = NA)
        cat(paste0("New Category: ", category,"\n"))
        readline("next:")
  
}, by = curveid]

#KI
#http://REMOVED/r-services-api/curve/render?curveIds=523049
#http://REMOVED/curveFit/index.php?path=TEST0003631
library(data.table)
library(drc)
library(racas)
load("kiFitData.rda")
points <- fitData[which(rbindlist(fitData$parameters)$lot=="CMPD001557105::1")]$points[[1]]


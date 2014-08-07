getDNETCurveData <- function(fitresultids, applicationSettings, globalConnect = TRUE) {
  #311180,311181  
  parametersLong <-   query(paste0("SELECT f.fitinstanceid,
                                   p.fitresultparameterid,
                                   f.fitmodelid,
                                   fr.fitresultid as curveid,
                                   f.transformationid,
                                   fr.objdno,
                                   p.fitvalue,
                                   ffp.letter,
                                   k.lot
                                   FROM fitinstance f,
                                   fitresultparameter p,
                                   fitresult fr,
                                   fitfunctionparameter ffp,
                                   fitmodelparameter fmp,
                                   kbatch k
                                   WHERE fr.fitresultid      =p.fitresultid
                                   AND f.fitinstanceid       =fr.fitinstanceid
                                   AND ffp.fitfuncparameterid=fmp.fitfuncparameterid
                                   AND p.fitmodelparameterid =fmp.fitmodelparameterid
                                   AND k.kbatchid = fr.objdno
                                   AND fr.fitresultid       IN (",sqliz(fitresultids),")"), applicationSettings = applicationSettings, globalConnect = TRUE)
  names(parametersLong) <- tolower(names(parametersLong))
  row.names(parametersLong) <- parametersLong$fitresultparameterid
  parametersLong <- parametersLong[,c("curveid","fitinstanceid","transformationid","objdno","fitvalue","letter", "lot")]
  wideFormat <- reshape(parametersLong,
                        timevar="letter",
                        idvar=c("curveid","objdno","fitinstanceid","transformationid","lot"),direction="wide")
  
  newNames <- c("fittedmin","fittedmax", "fittedlog10ki", "fittedslope")
  oldNames <- c("fitvalue.A","fitvalue.B","fitvalue.C","fitvalue.D")
  names(wideFormat)[6:9] <- newNames[match(names(wideFormat)[6:9],oldNames)]
  parameters <- wideFormat
  
  kiFitParams <- query(paste0("select resultcomment, fitresultid as curveid from facmpdfitparameters where fitresultid in (",sqliz(fitresultids),")"), applicationSettings = applicationSettings, globalConnect = globalConnect)
  names(kiFitParams) <- tolower(names(kiFitParams))
  parameters <- merge(parameters,kiFitParams, by.x = "curveid", by.y = "curveid" )
  
  
  for(r in 1:nrow(parameters)) {
    transformationid <- parameters$transformationid[r]
    objdno <- parameters$objdno[r]
    fitresultid <- parameters$curveid[r]
    resultidPoints <- query(paste0("SELECT R.RRTID AS response_sv_id,
                                   R.CONCENTRATION                                                                        AS dose,
                                   transformations.gettransformationvaluebyresult(T.TRANSFORMATIONID, R.NORMALIZEDRESULT) AS response
                                   FROM TRANSFORMATION T
                                   JOIN TSETREAD TR
                                   ON TR.TSETREADID = T.TSETREADID
                                   JOIN RRT R
                                   ON R.TSETREADID = T.TSETREADID
                                   JOIN TSETINSTANCE TI
                                   ON TR.TSETID             = TI.EXPERIMENTSETID
                                   WHERE T.TRANSFORMATIONID = ",transformationid,"
                                   AND R.objdno             = ",objdno,"
                                   AND R.FLAGID            != 1
                                   ORDER BY dose,
                                   response"), applicationSettings = applicationSettings, globalConnect = globalConnect)
    names(resultidPoints) <- tolower(names(resultidPoints))
    resultidPoints$curveid <- fitresultid
    resultidPoints$flag_user <- as.character(NA)
    resultidPoints$flag_algorithm <- as.character(NA)
    resultidPoints$flag_on.load <- as.character(NA)
    resultidPoints$flag_temp <- as.character(NA)
    resultidPoints$doseUnits <- "uM"
    resultidPoints$responseUnits <- "Eff"
    if(r==1) {
      points <- resultidPoints
    } else {
      points <- rbind(points, resultidPoints)
    }
  }
  
  return(list(points = points, parameters = parameters))
}
formatDNETFitData <- function(fitData) {
  fitData$points <- cbind(fitData$points, flagChanged = FALSE)
  fitData <- data.table(curveid = unique(as.character(fitData$points$curveid,fitData$parameters$curveid))[order(unique(as.character(fitData$points$curveid,fitData$parameters$curveid)))], 
                        modelHint = "LL.4", 
                        points = split(as.data.table(fitData$points), fitData$points$curveid),
                        parameters = split(as.data.table(fitData$parameters), fitData$parameters$curveid),
                        key = "curveid")
  myParameterRules <- list(goodnessOfFits = list(), limits = list())
  myInactiveRule <- list()
  myFixedParameters <- list()
  fitData[ , c("parameterRules", "inactiveRule", "fixedParameters") := list(list(myParameterRules),
                                                                            list(myInactiveRule),
                                                                            list(myFixedParameters))]
  fitData[ , model.synced := FALSE]
}
getRandomDNETCurves <- function(howManyYouWant) {
  dnetLocalSettings <- racas::applicationSettings
  dnetLocalSettings$server.database.username <- "kalypsysadmin"
  dnetLocalSettings$server.database.password <- "***REMOVED***"
  dnetLocalSettings$server.database.host <- "***REMOVED***"
  dnetLocalSettings$server.database.port <- 1521
  dnetLocalSettings$server.database.name <- "ORADEV"
  dnetLocalSettings$server.database.r.package <- "ROracle"
  dnetLocalSettings$server.database.r.driver="Oracle()"
  curveIds <- query(paste0("SELECT * FROM   ( SELECT FITRESULTID FROM facmpdfitparameters ORDER BY DBMS_RANDOM.RANDOM) WHERE  rownum <= ",howManyYouWant), applicationSettings = dnetLocalSettings)
  fitData <- getDNETCurveData(curveIds[,1], applicationSettings = dnetLocalSettings, globalConnect = TRUE)
  dbDisconnect(conn)
  formatDNETFitData(fitData)
  return(fitData)
}

library(racas)
library(ROracle)
library(data.table)
load("/Users/bbolt/Documents/dns/newCurveFitRegression/dns_regression_curveids.rda")
dnetLocalSettings <- racas::applicationSettings
dnetLocalSettings$server.database.username <- "kalypsysadmin"
dnetLocalSettings$server.database.password <- "***REMOVED***"
dnetLocalSettings$server.database.host <- "***REMOVED***"
dnetLocalSettings$server.database.port <- 1521
dnetLocalSettings$server.database.name <- "ORADEV"
dnetLocalSettings$server.database.r.package <- "ROracle"
dnetLocalSettings$server.database.r.driver="Oracle()"

fitData <- getDNETCurveData(fitResultIDs, applicationSettings = dnetLocalSettings)
fitData <- formatDNETFitData(fitData)

file <- "inst/docs/example-simple-fitsettings-ll4.json"
file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas" )
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
simpleFitSettings$inactiveThreshold <- 20
fitSettings <- simple_to_advanced_fit_settings(simpleFitSettings)
fitSettings$inverseAgonistMode <- FALSE

fitted <- dose_response_session(fitSettings = fitSettings, fitData = fitData)[[1]]
oldRegressionResults <- read.csv("/Users/bbolt/Documents/dns/newCurveFitRegression/old_regression_results.txt", sep = "\t")
matched <- match(rbindlist(fitted$parameters)$lot, oldRegressionResults$sample)
dnetCurveClasses <- oldRegressionResults$curveDescription[matched[!is.na(matched)]]
fitted[!is.na(matched), DNETCategory := as.character(dnetCurveClasses)]
#fitted[,DNETCategory:=rbindlist(fitted$parameters)$resultcomment]
blah <- fitted[, c("curveid","category","DNETCategory"), with = FALSE][category!=DNETCategory, ]

for(i in blah$curveid) {
  cat(paste0("New Category: ", fitted[curveid==i,]$category,"\n"))
  cat(paste0("Old Category: ", fitted[curveid==i,]$DNETCategory,"\n"))
  plot(fitted[curveid==i,]$model[[1]])
  readline("next:")
}


fitResultIDs <- c(8778, 8788, 8818, 9629, 8854, 8997, 9403, 9516, 8806, 8827, 8836, 8845, 8863, 8988)
fitData <- getDNETCurveData(fitResultIDs, applicationSettings = dnetLocalSettings)
fitData <- formatDNETFitData(fitData)

file <- "inst/docs/example-simple-fitsettings-ll4.json"
file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas" )
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
simpleFitSettings$inactiveThreshold <- 20
fitSettings <- simple_to_advanced_fit_settings(simpleFitSettings)
fitSettings$inverseAgonistMode <- FALSE
fitted <- dose_response_session(fitSettings = fitSettings, fitData = fitData)[[1]]

for(i in fitted$curveid) {
  cat(paste0("Category: ", fitted[curveid==i,]$category,"\n"))
  plot(fitted[curveid==i,]$model[[1]])
  readline("next:")
}


###Biphasics
file <- system.file("docs", "example-simple-fitsettings-ll4.json", package = "racas" )
simpleBulkDoseResponseFitRequestJSON <- readChar(file, file.info(file)$size)
simpleFitSettings <- fromJSON(simpleBulkDoseResponseFitRequestJSON)
simpleFitSettings$inactiveThreshold <- 20
fitSettings <- simple_to_advanced_fit_settings(simpleFitSettings)
fitSettings$inverseAgonistMode <- FALSE

biphasics <- c(9629, 8836, 8806, 8788, 8778)
fitData <- getDNETCurveData(biphasics, applicationSettings = dnetLocalSettings)
fitData <- formatDNETFitData(fitData)

fitData[ , points := {
  pts <- points[[1]]
  doses <- sort(unique(pts$dose), decreasing = TRUE)[1:4]
  pts[dose == doses[1], response := response - 50]
  pts[dose == doses[2], response := response - 40]
  pts[dose == doses[3], response := response - 20]
  pts[dose == doses[4], response := response - 10]
  list(list(pts))
}, by = curveid]

fitData <- dose_response_session(fitSettings = fitSettings, fitData = fitData)[[1]]
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




#Blah

blah <- fitData[ , any(!is.na(points[[1]]$flag_algorithm)), by = curveid][V1 == TRUE, ]

for(i in 1:nrow(blah)) {
  crv <- blah[i]$curveid
  pts <- fitData[curveid== crv, ]$points[[1]]
  plot(pts$dose,pts$response, log = "x")
  plot(fitData[curveid== crv, ]$model[[1]])
  readline("next:")
  dev.off()
}

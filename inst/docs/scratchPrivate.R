getRandomDNETCurves <- function(howManyYouWant) {
  dnetLocalSettings <- racas::applicationSettings
  dnetLocalSettings$server.database.username <- "kalypsysadmin"
  dnetLocalSettings$server.database.password <- "***REMOVED***"
  
  getDNETCurveData <- function(fitresultids, applicationSettings = dnetLocalSettings, globalConnect = TRUE) {
    #311180,311181
    parametersLong <- 	query(paste0("SELECT f.fitinstanceid,
                                  p.fitresultparameterid,
                                  f.fitmodelid,
                                  fr.fitresultid as curveid,
                                  f.transformationid,
                                  fr.objdno,
                                  p.fitvalue,
                                  ffp.letter
                                  FROM fitinstance f,
                                  fitresultparameter p,
                                  fitresult fr,
                                  fitfunctionparameter ffp,
                                  fitmodelparameter fmp
                                  WHERE fr.fitresultid      =p.fitresultid
                                  AND f.fitinstanceid       =fr.fitinstanceid
                                  AND ffp.fitfuncparameterid=fmp.fitfuncparameterid
                                  AND p.fitmodelparameterid =fmp.fitmodelparameterid
                                  AND fr.fitresultid       IN (",sqliz(fitresultids),")"), applicationSettings = applicationSettings, globalConnect = TRUE)
    names(parametersLong) <- tolower(names(parametersLong))
    row.names(parametersLong) <- parametersLong$fitresultparameterid
    parametersLong <- parametersLong[,c("curveid","fitinstanceid","transformationid","objdno","fitvalue","letter")]
    wideFormat <- reshape(parametersLong,
                          timevar="letter",
                          idvar=c("curveid","objdno","fitinstanceid","transformationid"),direction="wide")
    
    newNames <- c("fittedmin","fittedmax", "fittedlog10ki", "fittedslope")
    oldNames <- c("fitvalue.A","fitvalue.B","fitvalue.C","fitvalue.D")
    names(wideFormat)[5:8] <- newNames[match(names(wideFormat)[5:8],oldNames)]
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
      resultidPoints$flag <- NA
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
  curveIds <- query(paste0("SELECT * FROM   ( SELECT FITRESULTID FROM facmpdfitparameters ORDER BY DBMS_RANDOM.RANDOM) WHERE  rownum < ",howManyYouWant), applicationSettings = dnetLocalSettings)
  fitData <- getDNETCurveData(curveIds[,1], applicationSettings = dnetLocalSettings, globalConnect = TRUE)
  dbDisconnect(conn)
  fitData$points <- cbind(fitData$points, flagChanged = FALSE)
  fitData <- data.table(curveid = unique(as.character(fitData$points$curveid,fitData$parameters$curveid))[order(unique(as.character(fitData$points$curveid,fitData$parameters$curveid)))], 
                        renderingHint = "4 parameter D-R", 
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
  fitData
}

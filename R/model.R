


























#' Extract the parameters of a given set of curveids (of the same type of rendering hint)
#'
#' Given a list of curveids, this function queries the acas databse specified in the variable \code{\link{applicationSettings}} and
#' returns a list of length=2 with the points and parameters
#'
#' @param curveids a list of curveids of the same rendering hint E.G. "4 parameter D-R"
#' @param ... expressions fed to underlying function E.G. globalConnect (see \code{\link{query}} documentation )
#' @return A list of length 2 containing the points and parameters of the curveids
#' @keywords curveids, parameters, curveData
#' @export
#' @examples
#' 
#' getCurveData(c("126933_AG-00000615", "126933_AG-00000123"))
#' getCurveData(c("126933_AG-00000615", "126933_AG-00000123"), globalConnect=TRUE)

getCurveData <- function(curveids, ...) {
  parameters <- getCurveIDAnalsysiGroupResults(curveids)
  
  renderingHintParameters <- getParametersByRenderingHint(parametersDataFrame = parameters, curveids = curveids)
  
  #There are cases where getParametersByRenderingHint return curveids (See PK), in this case we call getParametersByRenderingHint again with those curveids 3_AG-00000040
  if(class(renderingHintParameters)=="list") {
    points <- getPoints(curveids, renderingHint = renderingHintParameters$renderingHint, ...)
    points$oldcurveid <- points$curveid
    points$curveid <- paste0(points$curveid,"_s_id_",points$s_id)
    renderingHintParameters <- renderingHintParameters$parameters
    renderingHintParameters <- merge(renderingHintParameters, unique(data.frame(name = points$name,s_id = points$s_id, curveid=points$oldcurveid)))
    renderingHintParameters$curveid <- paste0(renderingHintParameters$curveid,"_s_id_",renderingHintParameters$s_id)
    renderingHintParameters$name <- renderingHintParameters$name
  } else {
    points <- getPoints(curveids, ...)
  }
  
  if(is.null(renderingHintParameters) && nrow(points)==0) {
    warning(paste0("No Points or Parameters found for curveids: ", curveids))
  }
  
  return (list(
    points = points,
    parameters = renderingHintParameters
  ))
}
getPoints <- function(curveids, renderingHint = as.character(NA), ...) {
  
  
  drQU <- paste("SELECT curveid, dose, doseunits, response, responseunits, flag, response_ss_id, s_id, tg_id, ag_id from api_dose_response where curveid in (",sqliz(curveids),")",sep="")
  poQU <- paste("SELECT *
                FROM
                (SELECT s.id    AS S_ID,
                el.label_text AS experiment_name,
                'Animal-'
                || cl.label_text AS name,
                agv.string_value AS curveid,
                MAX(
                CASE
                WHEN sv.ls_kind = 'time'
                THEN sv.numeric_value
                ELSE NULL
                END)   AS dose,
                'Time' AS dosetype,
                MAX(
                CASE
                WHEN sv.ls_kind = 'time'
                THEN sv.unit_kind
                ELSE NULL
                END) AS doseunits,
                MAX(
                CASE
                WHEN sv.ls_kind = 'PO - PK_Concentration'
                THEN sv.numeric_value
                ELSE NULL
                END)   AS response,
                'Conc' AS responsetype,
                MAX(
                CASE
                WHEN sv.ls_kind = 'PO - PK_Concentration'
                THEN sv.unit_kind
                ELSE NULL
                END) AS responseunits,
                MAX(
                CASE sv.ls_kind
                WHEN 'flag'
                THEN sv.string_value
                ELSE NULL
                END) AS Flag,
                MAX(
                CASE sv.ls_kind
                WHEN 'PO - PK_Concentration'
                THEN sv.subject_state_id
                ELSE NULL
                END) AS response_ss_id,
                MAX(
                CASE sv.ls_kind
                WHEN 'PO - PK_Concentration'
                THEN s.treatment_group_id
                ELSE NULL
                END)       AS tg_id,
                MAX(ag.id) AS ag_id
                FROM analysis_GROUP ag
                JOIN analysis_GROUP_state ags
                ON ags.analysis_GROUP_id = ag.id
                JOIN analysis_GROUP_value agv
                ON agv.analysis_state_id = ags.id
                JOIN treatment_GROUP tg
                ON ag.id=tg.analysis_GROUP_id
                JOIN experiment e
                ON ag.experiment_id=e.id
                JOIN experiment_label el
                ON e.id=el.experiment_id
                JOIN subject s
                ON tg.id=s.treatment_GROUP_id
                JOIN subject_state ss
                ON ss.subject_id = s.id
                JOIN subject_value sv
                ON sv.subject_state_id = ss.id
                JOIN itx_subject_container itxsc
                ON s.id = itxsc.subject_id
                JOIN container c
                ON c.id=itxsc.container_id
                JOIN container_label cl
                ON cl.container_id=c.id
                WHERE agv.ls_kind = 'PO pk curve id'
                AND sv.ls_kind       IN ('time', 'PO - PK_Concentration')
                AND agv.string_value IN ( ",sqliz(curveids)," )
                GROUP BY s.id,
                ss.id,
                agv.string_value,
                cl.label_text,
                el.label_text
                )
                WHERE response IS NOT NULL
                ORDER BY tg_id ASC
                ")
  ivQU <- paste("SELECT *
                FROM
                (SELECT s.id    AS S_ID,
                el.label_text AS experiment_name,
                'Animal-'
                || cl.label_text AS name,
                agv.string_value AS curveid,
                MAX(
                CASE
                WHEN sv.ls_kind = 'time'
                THEN sv.numeric_value
                ELSE NULL
                END)   AS dose,
                'Time' AS dosetype,
                MAX(
                CASE
                WHEN sv.ls_kind = 'time'
                THEN sv.unit_kind
                ELSE NULL
                END) AS doseunits,
                MAX(
                CASE
                WHEN sv.ls_kind = 'IV - PK_Concentration'
                THEN sv.numeric_value
                ELSE NULL
                END)   AS response,
                'Conc' AS responsetype,
                MAX(
                CASE
                WHEN sv.ls_kind = 'IV - PK_Concentration'
                THEN sv.unit_kind
                ELSE NULL
                END) AS responseunits,
                MAX(
                CASE sv.ls_kind
                WHEN 'flag'
                THEN sv.string_value
                ELSE NULL
                END) AS Flag,
                MAX(
                CASE sv.ls_kind
                WHEN 'IV - PK_Concentration'
                THEN sv.subject_state_id
                ELSE NULL
                END) AS response_ss_id,
                MAX(
                CASE sv.ls_kind
                WHEN 'IV - PK_Concentration'
                THEN s.treatment_group_id
                ELSE NULL
                END)       AS tg_id,
                MAX(ag.id) AS ag_id
                FROM analysis_GROUP ag
                JOIN analysis_GROUP_state ags
                ON ags.analysis_GROUP_id = ag.id
                JOIN analysis_GROUP_value agv
                ON agv.analysis_state_id = ags.id
                JOIN treatment_GROUP tg
                ON ag.id=tg.analysis_GROUP_id
                JOIN experiment e
                ON ag.experiment_id=e.id
                JOIN experiment_label el
                ON e.id=el.experiment_id
                JOIN subject s
                ON tg.id=s.treatment_GROUP_id
                JOIN subject_state ss
                ON ss.subject_id = s.id
                JOIN subject_value sv
                ON sv.subject_state_id = ss.id
                JOIN itx_subject_container itxsc
                ON s.id = itxsc.subject_id
                JOIN container c
                ON c.id=itxsc.container_id
                JOIN container_label cl
                ON cl.container_id=c.id
                WHERE agv.ls_kind = 'IV pk curve id'
                AND sv.ls_kind       IN ('time', 'IV - PK_Concentration')
                AND agv.string_value IN ( ",sqliz(curveids)," )
                GROUP BY s.id,
                ss.id,
                agv.string_value,
                cl.label_text,
                el.label_text
                )
                WHERE response IS NOT NULL
                ORDER BY tg_id ASC
                ")
  poIVQU <- paste("SELECT a.*, a.Route || '-' || b.Dose as name
                  FROM (
                  select max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration' ) then tg.id else null end) as s_id,
                  api_agsvb.string_value as curveid,
                  cl.label_text as animal,
                  e.label_text as experiment_name,
                  max(CASE WHEN tv.ls_kind in ('time') then tv.numeric_value else null end) as dose,
                  'Time' as dosetype,
                  max(CASE WHEN tv.ls_kind in ('time') then tv.unit_kind else null end) as doseunits,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.numeric_value else null end) as response,
                  'Conc' as responsetype,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.unit_kind else null end) as responseunits,
                  max(CASE tv.ls_kind WHEN 'flag' then tv.string_value else null end) as Flag,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.treatment_state_id else null end) as response_ss_id,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration' ) then tg.id else null end) as tg_id,
                  max(api_agsvb.AG_ID) AS ag_id,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.uncertainty else null end) as standardDeviation,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration') then 'PO' WHEN tv.ls_kind in ('IV - PK_Concentration') then 'IV' else null end) as Route
                  FROM api_analysis_group_results api_agsvb JOIN treatment_GROUP tg on api_agsvb.ag_id=tg.analysis_GROUP_id
                  JOIN api_experiment e on api_agsvb.experiment_id=e.id
                  JOIN treatment_group_state ts ON ts.treatment_group_id = tg.id
                  JOIN treatment_group_value tv ON tv.treatment_state_id = ts.id
                  JOIN subject s on tg.id=s.treatment_GROUP_id
                  JOIN subject_state ss ON ss.subject_id = s.id
                  JOIN subject_value sv ON sv.subject_state_id = ss.id
                  JOIN itx_subject_container itxsc on s.id = itxsc.subject_id
                  JOIN container c on c.id=itxsc.container_id
                  JOIN container_label cl on cl.container_id=c.id
                  WHERE api_agsvb.ls_kind like 'PO IV pk curve id'
                  AND tv.ls_kind in ('time', 'PO - PK_Concentration', 'IV - PK_Concentration')
                  AND api_agsvb.string_value in (",sqliz(curveids),")
                  GROUP by tg.id, ts.id, api_agsvb.string_value, cl.label_text, e.label_text
                  ) a
                  LEFT OUTER JOIN (
                  SELECT tv.numeric_value || tv.unit_kind as Dose,
                  tg.id AS s_id
                  FROM api_analysis_group_results api_agsvb
                  JOIN treatment_GROUP tg
                  ON api_agsvb.ag_id=tg.analysis_GROUP_id
                  JOIN treatment_group_state ts
                  ON ts.treatment_group_id = tg.id
                  JOIN treatment_group_value tv
                  ON tv.treatment_state_id = ts.id
                  WHERE api_agsvb.ls_kind LIKE 'PO IV pk curve id'
                  AND tv.ls_kind             IN ('Dose')
                  AND api_agsvb.string_value IN (",sqliz(curveids),")
                  ) b
                  ON a.s_id = b.s_id
                  order by tg_id asc"
  )
  
  qu <- switch(renderingHint,
               "PO IV pk curve id" = poIVQU,
               "PO pk curve id" = poQU,
               "IV pk curve id" = ivQU
  )
  if(is.null(qu)) {
    qu <- drQU
  }
  
  points <- query(qu)
  names(points) <- tolower(names(points))
  if(nrow(points)==0) {
    stop("Got 0 rows of points")
  }
  points <- switch(renderingHint,
                   "PO IV pk curve id" = {
                     data.frame(  curveid = as.factor(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.factor(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseType = as.factor(points$dosetype), 
                                  doseUnits = as.factor(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseType = as.factor(points$responsetype),
                                  responseUnits = as.factor(points$responseunits),
                                  standardDeviation = as.numeric(points$standarddeviation),
                                  flag = as.factor(points$flag),
                                  response_ss_id = as.numeric(points$response_ss_id),
                                  s_id = as.numeric(points$s_id),
                                  tg_id = as.numeric(points$tg_id),
                                  ag_id = as.numeric(points$ag_id)
                     )
                   },
                   "IV pk curve id" = {
                     data.frame(  curveid = as.factor(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.factor(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseType = as.factor(points$dosetype), 
                                  doseUnits = as.factor(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseType = as.factor(points$responsetype),
                                  responseUnits = as.factor(points$responseunits),
                                  flag = as.factor(points$flag),
                                  response_ss_id = as.numeric(points$response_ss_id),
                                  s_id = as.numeric(points$s_id),
                                  tg_id = as.numeric(points$tg_id),
                                  ag_id = as.numeric(points$ag_id)
                     )
                   },
                   "PO pk curve id" = {
                     data.frame(  curveid = as.factor(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.factor(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseType = as.factor(points$dosetype), 
                                  doseUnits = as.factor(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseType = as.factor(points$responsetype),
                                  responseUnits = as.factor(points$responseunits),
                                  flag = as.factor(points$flag),
                                  response_ss_id = as.numeric(points$response_ss_id),
                                  s_id = as.numeric(points$s_id),
                                  tg_id = as.numeric(points$tg_id),
                                  ag_id = as.numeric(points$ag_id)
                     )
                   },
                   data.frame(  curveid = as.factor(points$curveid),
                                name = as.factor(points$curveid),
                                dose = as.numeric(points$dose), 
                                doseUnits = as.factor(points$doseunits), 
                                response = as.numeric(points$response),
                                responseUnits = as.factor(points$responseunits),
                                flag = as.factor(points$flag),
                                response_ss_id = as.numeric(points$response_ss_id),
                                s_id = as.numeric(points$s_id),
                                tg_id = as.numeric(points$tg_id),
                                ag_id = as.numeric(points$ag_id)
                   )
  )
  points
  if(nrow(points) > 0) {
    points$flag <- factor(points$flag, levels = c(levels(points$flag), TRUE, FALSE))
    points$flag <- !is.na(points$flag)
    points$id <- 1:nrow(points)
  }
  return(points)
}
getCurveIDAnalsysiGroupResults <- function(curveids, ...) {
  parameters <- query(paste("SELECT TESTED_LOT,
                            AG_ID,
                            AGV_ID,
                            LS_KIND,
                            OPERATOR_KIND,
                            NUMERIC_VALUE,
                            STRING_VALUE,
                            UNIT_KIND,
                            RECORDED_DATE
                            FROM p_api_analysis_group_results
                            WHERE ag_id IN
                            (SELECT ag_id
                            FROM p_api_analysis_group_results
                            WHERE LS_KIND like '%curve id'
                            AND string_value in (",sqliz(curveids),"))
                            ")
                      , ...)
  names(parameters) <- tolower(names(parameters))
  return(parameters)
}
getParametersByRenderingHint <- function(parametersDataFrame, curveids) {
  longFormat <- parametersDataFrame
  row.names(longFormat) <- parametersDataFrame$agv_id
  longFormat <- longFormat[,c("ag_id","tested_lot","ls_kind","numeric_value","string_value","unit_kind", "operator_kind")]
  wideFormat <- reshape(longFormat,
                        timevar="ls_kind",
                        idvar=c("ag_id","tested_lot"),direction="wide")
  
  renderingHint <- wideFormat$"string_value.Rendering Hint"[[1]]
  #If the rendering hint is null, we will use the ls_kind
  #BB - adding a case for "PK IV PO Single Dose" because I am only getting one rendering hint from the GDP for the 3 curves uploaded
  if(!is.null(renderingHint)) {
    if(renderingHint=="PK IV PO Single Dose") {
      renderingHint <- parametersDataFrame$ls_kind[which(parametersDataFrame$string_value %in% curveids)][1]
    }
  }
  if(is.null(renderingHint)) {
    dat <- paste0(capture.output(str(parametersDataFrame)), collapse = "\n")
    stop(paste0("Could not find ls_kind 'Rendering Hint' for curve id, unable to determine correct curve parameters\n",dat, collapse = "\n"))
  }
  parameters <- switch(renderingHint,
                       "4 parameter D-R" = getLL4ParametersFromWideFormat(wideFormat),
                       "Ki D-R" = getKiParametersFromWideFormat(wideFormat),
                       "PO IV pk curve id" = getPKParametersFromWideFormat(wideFormat, renderingHint),
                       "PO pk curve id" = getPKParametersFromWideFormat(wideFormat, renderingHint),
                       "IV pk curve id" = getPKParametersFromWideFormat(wideFormat, renderingHint)
  )
  return(parameters)
}

getLL4ParametersFromWideFormat <- function(wideFormat) {
  wideName = c("tested_lot", "string_value.curve id", "string_value.Rendering Hint", "numeric_value.Min","numeric_value.Fitted Min",
               "numeric_value.Max", "numeric_value.Fitted Max", "numeric_value.Hill slope", "numeric_value.Fitted Hill slope", 
               "numeric_value.EC50", "numeric_value.Fitted EC50", "operator_kind.EC50")
  newName = c("tested_lot", "curveid", "renderingHint", "min", "fittedmin",
              "max", "fittedmax", "hill", "fittedhillslope",
              "ec50", "fittedec50", "operator")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  return(extractParametersFromWideFormat(valuesToGet, wideFormat))
}

getPKParametersFromWideFormat <- function(wideFormat, renderingHint) {
  parameters <- list(renderingHint = renderingHint)
  wideName = c("tested_lot", paste0("string_value.",renderingHint))
  newName = c("tested_lot", "curveid")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  parameters$parameters <- extractParametersFromWideFormat(valuesToGet, wideFormat)
  return(parameters)
}

getPOIVPKParametersFromLongFormat <- function(longFormat) {
  curveIDList <- c('PO pk curve id','IV pk curve id')
  parameters <- list(curveids = longFormat$string_value[longFormat$ls_kind %in% curveIDList])
  parameters$parameters <- subset(longFormat, ls_kind %in% curveIDList, select = c("ag_id", "tested_lot", "string_value") )
  names(parameters$parameters) <- c("ag_id", "tested_lot", "curveid")
  return(parameters)  
}

getPOIVPKParametersFromWideFormat <- function(wideFormat) {
  parameters <- list(curveids = c(longFormat$"string_value.PO pk curve id",wideFormat$"string_value.IV pk curve id"))
  wideName = c("tested_lot", "string_value.PO IV pk curve id", "string_value.PO pk curve id", "string_value.IV pk curve id")
  newName = c("tested_lot", "curveid", "poPKCurveID", "ivPKCurveID")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  parameters$parameters <- extractParametersFromWideFormat(valuesToGet, wideFormat)
  return(parameters)  
}

extractParametersFromWideFormat <- function(valuesToGet, wideFormat) {
  parameters <- data.frame(ag_id = as.factor(wideFormat$ag_id))
  for(i in 1:nrow(valuesToGet)) {
    colName <- as.character(valuesToGet$wideName[i])
    newName <- as.character(valuesToGet$newName[i])
    if(colName %in% colnames(wideFormat)) {
      split <- strsplit(colName,"\\.")[[1]]
      if(length(split)==1) {
        parameters <- cbind(parameters,  wideFormat[,split])
      } else {
        parameters <- cbind(parameters, switch(split[1],
                                               "numeric_value" = as.numeric(wideFormat[,colName]),
                                               as.factor(wideFormat[,colName]))
        )        
      } 
    } else {
      parameters <- cbind(parameters, switch(split[1],
                                             "numeric_value" = as.numeric(rep(NA, times = nrow(parameters))),
                                             factor(rep(NA, times = nrow(parameters))))
      )
    }
    names(parameters)[ncol(parameters)] <- newName
  }
  return(parameters)
}

#' Curve plotting function
#'
#' This function takes in a set of data points, curve parameters, and an equation and plots the data
#'
#' @param curveData a data frame with the points with column names curveid, dose, response, flag
#' @param params the set of parameters used to enumerate the curve
#' @param outFile file to plot image to, if not specified then the function plots to graphic device
#' @param ymin specify the ymin axes location
#' @param ymax specify the ymax axes location
#' @param xmin specify the xmin axes location
#' @param xmax specify the xmax axes location
#' @param logDose specify if x axis is in log space
#' @param logResponse specify if y axis is in log space
#' @param height height of the plot in pixels
#' @param width width of the plot in pixels
#' @param showGrid adds a grid to the plot
#' @param showLegend shows a legend with curve ids on the right hand side of the plot
#' @param showAxes turns axes on or off
#' @param plotMeans will plot the mean Y given a given X
#' @param drawStdDevs will plot the Standard Deviations for each dose response combination
#' @param connectPoints will draw a line between the means of each mean X - Y
#' @param drawCurve turn the curve drawing on and off
#' @param drawCurve turn the curve drawing on and off
#' 
#' @return If  outFile is specified, then the function prints an image to the out file, if outFile is not specified, then then an image is plotted to a graphics device
#' @keywords plot, render, curve
#' @export
#' @examples
#' LL4 <- 'min + (max - min)/((1 + exp(-hill * (log(x/ec50))))^1)'
#' data(curveData)
#' params <- curveData$parameters
#' curveData <- curveData$points
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE)
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE)
#' PlotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = TRUE)
#' 
#' #Ki Data (using raw data)
#' data(kiData)
#' params <- kiData$parameters
#' points <- kiData$points
#' paramNames <- c("Top", "Bottom", "HotNM", "HotKDNM", "Log10Ki")
#' KiFCT <- 'Bottom + (Top-Bottom)/(1+10^(x-log10((10^Log10Ki)*(1+HotNM/HotKDNM))))'
#' PlotCurve(points, params, KiFCT, paramNames, drawIntercept= "Log10Ki", outFile = NA, ymin = NA, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#'
#' #PK Curves
#' #PO
#' data(poPKCurveData)
#' params <- poPKCurveData$parameters
#' curveData <- poPKCurveData$points
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #IV
#' data(ivPKCurveData)
#' params <- ivPKCurveData$parameters
#' curveData <- ivPKCurveData$points
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #IV Overlay
#' data(overlayIVPKCurveData)
#' params <- overlayIVPKCurveData$parameters
#' curveData <- overlayIVPKCurveData$points
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' 
#' #PO IV
#' data(poIVPKCurveData)
#' params <- poIVPKCurveData$parameters
#' curveData <- poIVPKCurveData$points
#' PlotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, addShapes = TRUE, drawStdDevs = TRUE)
#' 

PlotCurve <-  function(curveData, params, fitFunction, paramNames = c("ec50", "min", "max", "hill"), drawIntercept = "ec50", outFile = NA, ymin = NA, logDose = FALSE, logResponse = FALSE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, drawCurve = TRUE, connectPoints = FALSE, plotMeans = FALSE, drawStdDevs = FALSE, addShapes = FALSE, labelAxes = FALSE, ...) {
  
  #Check if paramNames match params column headers
  if(!is.na(paramNames) && drawCurve == TRUE) {
    if(any(is.na(match(paramNames, names(params))))) {
      stop("paramNames not found in names of params")
    }
  } else {
    drawCurve <- FALSE
    drawIntercept <- NA
  }
  
  #Assign Colors
  plotColors <- rep(c("black","red","orange", "blue", "green","purple", "cyan"),100, replace = TRUE)
  #plotColors <- rep(c("0x8DD3C7", "0xFFFFB3", "0xBEBADA", "0xFB8072", "0x80B1D3", "0xFDB462", "0xB3DE69", "0xFCCDE5", "0xD9D9D9", "0xBC80BD", "0xCCEBC5", "0xFFED6F"), 100, replace = TRUE)
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
            rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  plotColorsAlpha <- add.alpha(plotColors, alpha=0.3)
  params$color <- plotColors[1:nrow(params)]
  curveData$color <- plotColors[match(curveData$curveid,params$curveid)] 
  curveData$coloralpha <- plotColorsAlpha[match(curveData$curveid,params$curveid)] 
  
  #Add shapres
  if(addShapes) {
    pchs <- 1:24
    pchs <- rep(pchs[-c(4)],100, replace = TRUE)
    params$pch <- pchs[1:nrow(params)]
    curveData$pch <- params$pch[match(curveData$curveid,params$curveid)]
  }
  
  #Determine axes ranges
  maxDose <- max(curveData$dose)
  minDose <- min(curveData$dose)
  maxResponse <- max(curveData$response)
  minResponse <- min(curveData$response)
  responseRange <- abs(maxResponse-minResponse)
  doseRange <- abs(maxDose-minDose)
  if(is.na(ymin)) {
    if(logResponse) {
      ymin <- 10^(log10(min(curveData$response[curveData$response>0])) - 0.5)
    } else {
      ymin <- (minResponse - 0.01*responseRange)
    }
  }
  if(is.na(ymax)) {
    if(logResponse) {
      ymax <- 10^(log10(maxResponse) + 0.5)
    } else {
      ymax <- (maxResponse + 0.01*responseRange)
    }
  }
  if(is.na(xmax)) {
    if(logDose) {
      xmax <- 10^(log10(maxDose) + 0.5)
    } else {
      xmax <- maxDose + abs(0.01 * doseRange)
    }  
  }
  if(is.na(xmin)) {
    if(logDose) {
      xmin <- 10^(log10(min(curveData$dose[curveData$response>0])) - 0.5)
    } else {
      xmin <- minDose - abs(0.01 * doseRange)
    }
  }
  #If plotting log data then xrange/yrange vals cannot be negative
  if(logDose) {
    if(!is.na(xmin)) {
      if(xmin <= 0) {
        xmin = 0.001
      }
    }
    if(!is.na(xmax) && !is.na(xmin)) {
      if(xmax <= xmin) {
        xmin = NA
        xmax = NA
      }
    }
  }
  if(logResponse) {
    if(!is.na(ymin)) {
      if(ymin <= 0) {
        ymin = 0.001
      }
    }
    if(!is.na(xmax) && !is.na(ymin)) {
      if(ymax <= ymin) {
        ymin = NA
        ymax = NA
      }
    }
  }
  
  xrn <- c(xmin, xmax)
  yrn <- c(ymin, ymax)
  
  ##Seperate Flagged and good points for plotting different point shapes..etc.
  flaggedPoints <- subset(curveData, curveData$flag)
  goodPoints <- subset(curveData, !curveData$flag)
  
  ##Calculate Means and SDs
  sds <- aggregate(goodPoints$response,list(dose=goodPoints$dose,curveid=goodPoints$curveid, color = goodPoints$color), sd)
  names(sds)[ncol(sds)] <- "sd"
  means <- aggregate(goodPoints$response,list(dose=goodPoints$dose,curveid=goodPoints$curveid, color = goodPoints$color), mean)
  names(means)[ncol(means)] <- "mean"
  
  ###Begin Drawing the Plot
  if(!is.na(outFile)) {
    png(file = outFile, height = height, width = width)
  }
  
  #Axes and Labels require extra margins
  #TODO: make this a bit nicer, right now there is probably too much padding in the margins when label is on
  defaultMargins=c(0.1,1,0.3,0.8)
  #par(mar=c(2.1,3,0.1,0.1)) #Set margin to east to fit legend
  margins <- defaultMargins
  if(labelAxes) {
    margins[c(1,2)] <- defaultMargins[c(1,2)] + 4
  } else {
    if(showAxes) {
      margins[c(1,2)] <- defaultMargins[c(1,2)] + 2
    }
  }
  par(mar = margins)
  
  #Determine which axes will require log scale plotting
  plotLog <- paste0(ifelse(logDose, "x", ""),ifelse(logResponse, "y", ""))
  #First Plot Good Points so we that can see the flagged points if they are overlayed
  doGoodPoints <- function(yrn) {
    if(!plotMeans) {
      #TODO: what if plotMeans but also plotPoints? deal with that later
      plot(goodPoints$dose, goodPoints$response, log = plotLog, col = goodPoints$color, pch = goodPoints$pch, xlim = xrn, ylim = yrn, xaxt = "n", family = "sans", axes = FALSE, ylab = "", xlab = "")
    } else {
      plot(means$dose, means$mean, log = plotLog, col = means$color, xlim = xrn, ylim = yrn, xaxt = "n", family = "sans", axes = FALSE, ylab = "", xlab = "")
    }
    if(drawStdDevs) {
      plotCI(x=goodPoints$dose,y=goodPoints$response, uiw=goodPoints$standardDeviation, col = goodPoints$color, add=TRUE,err="y",pch=NA)
    }
  }
  #Draw Legend if specified
  doGoodPoints(yrn = yrn)
  if(showLegend) {
    #par(xpd=TRUE) # allows legends to be printed outside plot area
    #legendYPosition <- 10 ^ par("usr")[2]
    #legendXPosition <- par("usr")[4]
    if(is.null(params$name)) {
      legendText <- params$curveid
    } else {
      legendText <- params$name
    }
    legendTextColor <- params$color
    legendPCH <- params$pch
    legendLineWidth <- 1
    leg <- legend("topright",legend = legendText, col = legendTextColor, lty = legendLineWidth, pch = legendPCH, cex=0.7, box.lwd = 0)
    doGoodPoints(yrn = c(yrn[1], yrn[2] + leg$rect$h))
    leg <- legend("topright",legend = legendText, col = legendTextColor, lty = legendLineWidth, pch = legendPCH, cex=0.7, box.lwd = 0)
  }
  if(connectPoints) {
    cids <- unique(means$curveid)
    for(c in 1:length(cids)) {
      cid <- cids[c]
      lineData <- subset(means, means$curveid == cid)
      lines(x = lineData$dose, y = lineData$mean, col = lineData$color, pch = 4, lty = 'dotted')
    }
  }
  
  #If grid, then add grid
  if(showGrid) {
    grid(lwd = 1.7)
  }
  #Now Plot Flagged Points
  points(x = flaggedPoints$dose, y = flaggedPoints$response, col = flaggedPoints$coloralpha, pch = 4)
  
  #Draw Error Bars and Means
  #plotCI(x=means$dose,y=means$MEAN,uiw=sds$SD,add=TRUE,err="y",pch="-")
  getDrawValues <- function(params) {
    reportedValueColumns <- match(paramNames, names(params))
    reportedValueColumns <- reportedValueColumns[!is.na(reportedValueColumns)]
    reportedValues <- params[,reportedValueColumns]
    reportedValues <- reportedValues[sapply(reportedValues, function(x) !any(is.na(x)))] 
    
    tmp <- data.frame(matrix(nrow=1, ncol=length(paramNames))) 
    names(tmp) <- paramNames
    tmp[1,match(names(reportedValues), paramNames)] <- reportedValues
    
    fittedColumnNames <- paste0("fitted",paramNames)
    fittedValueColumns <- match(fittedColumnNames,names(params))
    fittedValueColumns <- fittedValueColumns[!is.na(fittedValueColumns)]
    
    if(length(fittedValueColumns) > 0) {
      fittedValues <-  params[,fittedValueColumns]
      fittedValues <- fittedValues[sapply(fittedValues, function(x) !any(is.na(x)))] 
      tmp[1,match(names(fittedValues),fittedColumnNames)] <- fittedValues
    }
    return(tmp)
  }
  #Curve Drawing Function
  drawCurveID <- function(cid) {
    drawValues <- getDrawValues(params = params[cid,])
    curveID <- params$curveid[cid]
    curveParams <- subset(params, params$curveid == curveID)
    for(i in 1:ncol(drawValues)) {
      assign(names(drawValues)[i], drawValues[,i])
    }
    fct <- eval(parse(text=paste0('function(x) ', fitFunction)))
    curve(fct, from = xrn[1], to = xrn[2], add = TRUE, col = curveParams$color)  
  }
  #Actually Draw Curves
  if(drawCurve) {
    null <- lapply(1:length(params$curveid),drawCurveID)
  }
  ##DO axes and Grid
  box()
  if(showAxes) {
    if(logDose) {
      xTickRange <- par("xaxp")[1:2]
      log10Range <- log10(abs(xTickRange[2]/xTickRange[1]))+1
      major.ticks <- unlist(lapply(1:log10Range,ten <- function(x) {xTickRange[1]*10^(x-1)}))
      axis(1,at=major.ticks,labels=formatC(major.ticks),tcl=par("tcl")*1.8)
      intervals <- c(major.ticks/10,major.ticks[-1],major.ticks*10)
      minor.ticks <- 1:9 * rep(intervals / 10, each = 9)
      axis(1, at= minor.ticks, tcl = -0.5, labels = FALSE, tcl=par("tcl")*0.7) 
    } else {
      axis(1)
    }
    if(logResponse) {
      yTickRange <- par("yaxp")[1:2]
      log10Range <- log10(abs(yTickRange[2]/yTickRange[1]))+1
      major.ticks <- unlist(lapply(1:log10Range,ten <- function(x) {yTickRange[1]*10^(x-1)}))
      axis(2,at=major.ticks,labels=formatC(major.ticks),tcl=par("tcl")*1.8,,las=1)
      intervals <- c(major.ticks/10,major.ticks[-1],major.ticks*10)
      minor.ticks <- 1:9 * rep(intervals / 10, each = 9)
      axis(2, at= minor.ticks, tcl = -0.5, labels = FALSE, tcl=par("tcl")*0.7) 
    } else {
      axis(2)
    }
  }
  ##If only one curve then draw ac50 lines
  #Get coordinates to draw lines through curve at AC50
  #Vertical
  if(!is.na(drawIntercept)) {
    if(nrow(params) == 1) {
      drawValues <- getDrawValues(params = params[1,])
      for(i in 1:ncol(drawValues)) {
        assign(names(drawValues)[i], drawValues[,i])
      }
      fct <- eval(parse(text=paste0('function(x) ', fitFunction)))
      curveIntercept <- fct(params[,drawIntercept])
      ylin <- c()
      ylin$x <- c(get(drawIntercept), get(drawIntercept))
      ylin$y <- c(par("usr")[3],curveIntercept)
      #Horizontal
      xlin <- c()
      if(logDose) {
        xlin$x <- c(0.0000000000000001,get(drawIntercept))
      } else {
        xlin$x <- c(par("usr")[1],get(drawIntercept))
      }
      xlin$y <- c(curveIntercept,curveIntercept)
      #Draw AC50 Lines
      lines(ylin,lwd=0.7,col="red")
      lines(xlin,lwd=0.7,col="red")
    }
  }
  if(labelAxes) {
    xlabel <- paste0(curveData$doseType[1], " (",curveData$doseUnits[1],")")
    ylabel <- paste0(curveData$responseType[1], " (",curveData$responseUnits[1],")")
    title(xlab = xlabel, ylab = ylabel)
  }
  if(!is.na(outFile)) {
    dev.off()
  }
}

LL4 <- 'min + (max - min)/((1 + exp(-hill * (log(x/ec50))))^1)'
OneSiteKi <- 'min + (max-min)/(1+10^(x-log10((10^Log10Ki)*(1+ligandConc/kd))))'

kiNames <- c("Top", "Bottom", "logKi")
oneSiteKi <- function(kd, ligandConc, fixed = c(NA, NA, NA), names = c("c", "d", "e")) {
  
  ki.fct3 <- param[,2] + (param[,1]-param[,2])/(1+10^(x-log10(10^param[,3]*(1+ligandConc/kd))))
  ki.fct3 <- param[,2] + (param[,1]-param[,2])/(1+10^(x-log10(10^param[,3]*(1+ligandConc/kd))))
  
  return(c(ki.fct, ki.ssft, ki.names))
}
oneSiteKi.ssf <- function(data) {
  Top <- max(data[,2])
  Bottom <- min(data[,2])
  logKi <- -8.0
  #print(data)
  return(c(Top, Bottom, logKi))
}
#fitModelNormalized <- drm(NORMALIZEDRESULT ~ CONCENTRATION, data = points, fct = list(kifct, kissfct, kiNames), curveid=PTODWELLLITERAL, robust = "mean")
#fitModelEfficacy <- drm(EFFICACY ~ CONCENTRATION, data = points, fct = list(kifct, kissfct, kiNames), curveid=PTODWELLLITERAL, robust = "mean")


kissfctFree <- function(data) {
  Top <- max(data[,2])
  KiuM <- 0.1
  Bottom <- min(data[,2])
  return(c(Bottom, KiuM, Top))
}
getFitModel <- function(dataSet, drcFunction = LL.4, subs = NA, paramNames = eval(formals(drcFunction)$names), fixedValues = eval(formals(drcFunction)$fixed), robust = "mean") {
  fct <- drcFunction(fixed=fixedValues, names=paramNames)
  drcObj <- NULL
  tryCatch({
    drcObj <- drm(formula = response ~ dose, data = dataSet, subset = !dataSet$flag, robust=robust, fct = fct)
  },
           error = function(ex) {
             #Turned of printing of error message because shiny was printing to the browser because of a bug
             #print(ex$message)
           })
  return(drcObj)
}

drcObject.getKeyValues <- function(drcObj = drcObject) {
  #Get calculated values (only non-fixed parameters)
  fitValues <- as.data.frame(drcObj$parmMat)
  row.names(fitValues) <- drcObj$fct$names
  fixedValues <- as.data.frame(drcObj$fct$fixed)
  fixedValues <- subset(fixedValues, row.names(fixedValues) <= length(drcObj$paramNames))
  row.names(fixedValues) <- drcObj$paramNames
  names(fixedValues) <- drcObj$name
  fixedValues <- subset(fixedValues, !is.na(fixedValues))  
  keyValues <- rbind(fitValues,fixedValues)
  keyValues <- as.data.table(t(keyValues))
  return(keyValues)
}

drcObject.getKeyValues.as.dataFrame <- function(drcObj = drcObject) {
  #Get calculated values (non fixed parameters)
  fitValues <- as.data.frame(drcObj$parmMat)
  row.names(fitValues) <- drcObj$fct$names
  fixedValues <- as.data.frame(drcObj$fct$fixed)
  fixedValues <- subset(fixedValues, row.names(fixedValues) <= length(drcObj$paramNames))
  row.names(fixedValues) <- drcObj$paramNames
  names(fixedValues) <- drcObj$name
  fixedValues <- subset(fixedValues, !is.na(fixedValues))	
  keyValues <- rbind(fitValues,fixedValues)
  keyValues <- as.data.frame(t(keyValues))
  return(keyValues)
}

drcList.getKeyValues.as.dataFrame <- function(drcList = drcObjectList) {
  keyValueMatrix <- t(sapply(drcList, drcObject.getKeyValues.as.dataFrame))
  mode(keyValueMatrix) <- "numeric"
  keyValueDataFrame <- as.data.frame(keyValueMatrix)
  return(keyValueDataFrame)
}

drcObject.getMaxDose.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$dName
  value <- max(drcObj$data[,name])
  return(value)
}

drcObject.getMinDose.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$dName
  value <- min(drcObj$data[,name])
  return(value)
}

drcObject.getMaxActivity.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$orName
  dose <- max(drcObj$data[,name])
  return(dose)
}

drcObject.getMinActivity.as.numeric <- function(drcObj = drObject) {
  name <- drcObj$dataList$names$orName
  value <- min(drcObj$data[,name])
  return(value)
}

drcObject.getDRCFitStats <- function(drcObject) {
  SSE <- sum(residuals(drcObject)^2)
  SST <- sum(treatmentData$RESPONSE-mean(treatmentData$RESPONSE)^2)
  rSquared <- 1-(SSE/SST)
}

drcObject.getEC50Intercept.as.numeric <- function(drcObj = drcObject) {
  ed50 <- as.data.frame(ED(drcObj, respLev = 50, display = FALSE))[1]
  value <- predict(drcObj,data.frame(conc=ed50))[[1]]
  return(value)
}

drcObject.getPredictedResponseFromDose.as.numeric <- function(drcObj = drcObject, responseLevel = numericValue) {
  value <- predict(drcObj,data.frame(conc=responseLevel))[[1]]
  return(value)
}

fitParams.getOperatorParams <- function(fixedValues) {
  #Removed fixed values with operators
  fixedValues <- subset(fixedValues,!is.na(fixedValues$resultoperator))
  return(fixedValues)
}

fitParams.getAboveMaxTestedParams <- function(drData = data$rawPoints, curveID = "curve_id", fixedValues = data$fitParams) {
  #Function to remove IC50 if above the max concentration tested
  IC50AboveMaxTested <- function(id) {
    returnValues <- subset(fixedValues,FALSE)
    if(length(fixedValues$resultvalue[fixedValues$curveid==id]) > 0) {
      maxTestedConc <- max(drData$dose[drData$TREATMENTGROUP==id])
      fixedIC50 <- fixedValues$resultvalue[fixedValues$curveid==id]
      if(fixedIC50 > maxTestedConc) {
        returnValues <- subset(fixedValues,fixedValues$curveid==id)
      }
      
    }
    return(returnValues)
  }
  treatmentGroups <- split(drData, drData[,curveID])
  drData$TREATMENTGROUP <- factor(drData[,curveID])
  fixedValuesList <- sapply(levels(drData$TREATMENTGROUP), simplify = FALSE, USE.NAMES = TRUE, FUN = IC50AboveMaxTested)
  fixedValues <- do.call("rbind", lapply(fixedValuesList, data.frame, stringsAsFactors = FALSE))
  row.names(fixedValues) <- NULL
  return(fixedValues)
}

setdiff.data.frame <- function(A,B) {
  if(nrow(B) == 0) {
    ans <- A
  } else {
    ans <- A[!duplicated( rbind(B,A) )[ -seq_len(nrow(B))] , ]
  }
  return(ans)
}

fitParams.getOperatorParams <- function(fixedValues) {
  #Removed fixed values with operators
  fixedValues <- subset(fixedValues,!is.na(fixedValues$resultoperator))
  return(fixedValues)
}

fitParams.getAboveMaxTestedParams <- function(drData = data$rawPoints, curveID = "curve_id", fixedValues = data$fitParams) {
  #Function to remove IC50 if above the max concentration tested
  IC50AboveMaxTested <- function(id) {
    returnValues <- subset(fixedValues,FALSE)
    if(length(fixedValues$resultvalue[fixedValues$curveid==id]) > 0) {
      maxTestedConc <- max(drData$dose[drData$TREATMENTGROUP==id])
      fixedIC50 <- fixedValues$resultvalue[fixedValues$curveid==id]
      if(fixedIC50 > maxTestedConc) {
        returnValues <- subset(fixedValues,fixedValues$curveid==id)
      }
      
    }
    return(returnValues)
  }
  treatmentGroups <- split(drData, drData[,curveID])
  drData$TREATMENTGROUP <- factor(drData[,curveID])
  fixedValuesList <- sapply(levels(drData$TREATMENTGROUP), simplify = FALSE, USE.NAMES = TRUE, FUN = IC50AboveMaxTested)
  fixedValues <- do.call("rbind", lapply(fixedValuesList, data.frame, stringsAsFactors = FALSE))
  row.names(fixedValues) <- NULL
  return(fixedValues)
}

#' Function to save dose response curve data
#' 
#' This function takes in changes to an already saved curve and updates the flags and fitted parameters
#'
#' @param subjectData A data frame of the changed flag ids (see examples for more on format)
#' 
#' Format:
#' \itemize{
#'   \item analysisGroupdID  The analysisGroupID of the flag
#'   \item valueKind A string that says "Flag"
#'   \item valueType A string that says "stringValue"
#'   \item subjectID The subjedtID of the flag
#'   \item subjectStateID The subjectStateID of the flag
#'   \item treatmentGroupID The treatmentGroupdID of the flag 
#'   \item stringValue A string value for the flag (E.G. "User" or "Algorithm")
#' }
#' @param analysisGroupData A data frame of the parameters to be changed (see examples for more on format)
#' 
#' Format: 
#' \itemize{
#'   \item analysisGroupdID
#'   \item batchCode
#'   \item valueType
#'   \item valueKind
#'   \item resultUnits
#'   \item numericValue
#'   \item StringValue
#' }
#' @keywords save, update, dose, response, curve
#' @export
#' 
updateDoseResponseCurve <- function (analysisGroupData, subjectData) {
  require(racas)
  require(RCurl)
  require(rjson)
  require(plyr)
  
  lsServerURL <<- racas::applicationSettings$client.service.persistence.fullpath
  
  lsTransaction <- createLsTransaction()
  
  ##### Subject States ====================================================================================
  
  updateFlags <- function (valueTable, newFlags) {
    require(plyr)
    
    addFlags <- function(stateID, stringValue) {
      newRow <- data.frame(ignored = FALSE, publicData = TRUE, stringValue = stringValue, valueKind = "flag", 
                           valueType = "stringValue", stateID = stateID, stringsAsFactors=FALSE)
    }
    
    valueTableWithoutFlags <- valueTable[valueTable$valueKind != "flag", ]
    flagValueTable <- mdply(newFlags, .fun = addFlags)
    
    return(rbind.fill(valueTableWithoutFlags, flagValueTable))
  }
  
  # This is a closure. Don't move it out without pulling lsTransaction through
  copyStates <- function(subject) {
    # Args:
    #   subject:    A list that is a subject
    #
    # Returns:
    #   a list of new states that are copies of the old ones
    
    
    # Copy and update these states
    stateTypeAndKinds <- sapply(subject$subjectStates, function(x) x$stateTypeAndKind)
    nonIgnoredStates <- sapply(subject$subjectStates, function(x) !x$ignored)
    statesToChange <- subject$subjectStates[stateTypeAndKinds =="data_results" & nonIgnoredStates]
    
    setTransaction <- function(stateToChange, subjectId) {
      stateToChange$lsTransaction <- lsTransaction
      stateToChange$subjectValues <- NULL
      stateToChange$subject = list(id = subjectId, version = 0)
      return(stateToChange)
    }
    statesToChange <- lapply(statesToChange, setTransaction, subject$id)
    return (statesToChange)
  }
  
  subjects <- fromJSON(getURL(
    paste0(lsServerURL, "subjects/stateTypeAndKind/data_results/jsonArray"),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(lapply(unique(subjectData$subjectID), function(x) list(id = x))))
  )
  
  newStates <- unlist(lapply(subjects, copyStates), recursive = FALSE)
  originalStateIds <- sapply(newStates, function(x) x$id)
  newStates <- lapply(newStates, function(x) {x$id <- NULL; return(x)})
  
  savedSubjectStates <- saveAcasEntities(newStates, "subjectstates")
  subjectStateIds <- sapply(savedSubjectStates, function(x) x$id)
  
  stateIdsToIgnore <- unique(originalStateIds)
  
  subjectStateTranslation <- data.frame(subjectStateId=subjectStateIds, originalStateId=originalStateIds)
  
  ##### Subject Values ====================================================================================
  
  valuesWithStatesIncluded <- ldply(subjects, getValuesFromSubject)
  
  valuesToReplace <- valuesWithStatesIncluded[valuesWithStatesIncluded$stateID %in% subjectData$subjectStateID & 
                                                !(valuesWithStatesIncluded$ignored), ]
  
  valueIdsToIgnore <- unique(valuesToReplace$id)
  
  # Not sure why this is here anymore...
  valuesToReplace <- valuesToReplace[!(is.na(valuesToReplace$stateID)), ]
  
  valuesToReplace$id <- NULL
  
  newFlags <- subjectData[subjectData$valueKind == "flag", c("subjectStateID", "stringValue")]
  names(newFlags) <- c("stateID","stringValue")
  valuesToReplace <- updateFlags(valuesToReplace, newFlags)
  
  valuesToReplace$oldSubjectStateID <- valuesToReplace$stateID
  valuesToReplace$stateID <- subjectStateTranslation$subjectStateId[match(valuesToReplace$stateID,
                                                                          subjectStateTranslation$originalStateId)]
  
  valuesToReplace$stateVersion <- 0
  valuesToReplace$stateGroupIndex <- 1
  
  savedSubjectValues <- saveValuesFromLongFormat(valuesToReplace, stateGroupIndices=1, entityKind="subject", lsTransaction=lsTransaction)
  
  # Ignore old subject states
  query(paste0(
    "UPDATE subject_state
    SET ignored       = 1,
    version                   = version+1
    WHERE id          IN (", paste(stateIdsToIgnore, collapse=","), ")"))
  # Ignore old subject values
  query(paste0(
    "UPDATE subject_value
    SET ignored       = 1,
    version                   = version+1
    WHERE id          IN  (", paste(valueIdsToIgnore, collapse=","), ")"))
  
  
  ##### Treatment Groups ======================================================
  treatmentGroupStateGroups <- list(list(entityKind = "treatmentgroups",
                                         stateType = "data", 
                                         stateKind = "results",
                                         includesOthers = TRUE,
                                         includesCorpName = FALSE))
  
  treatmentGroupInput <- valuesToReplace[valuesToReplace$valueKind %in% c("Response", "flag"), 
                                         c('numericValue', 'stringValue', 'valueKind', 'valueOperator', 'valueType', 'valueUnit','oldSubjectStateID')]
  
  treatmentGroupIdTranslation <- subjectData[,c('subjectStateID', 'treatmentGroupID')]
  
  treatmentGroupInput$treatmentGroupID <- treatmentGroupIdTranslation$treatmentGroupID[match(treatmentGroupInput$oldSubjectStateID, 
                                                                                             treatmentGroupIdTranslation$subjectStateID)]
  
  createTreatmentGroupData <- function(inputData) {
    flaggedStateIDs <- inputData$oldSubjectStateID[inputData$valueKind == "flag" & ((inputData$stringValue != "") | is.na(inputData$stringValue))]
    dataToAverage <- inputData[!(inputData$oldSubjectStateID %in% flaggedStateIDs) & inputData$valueType=="numericValue", ]
    return(data.frame(
      valueKind = "Response", 
      valueType = "numericValue",
      # TODO: once there is more real data, add this back in and check
      #valueUnit = inputData$valueUnit[1],
      treatmentGroupID = inputData$treatmentGroupID[1],
      numberOfReplicates = nrow(dataToAverage),
      uncertainty = sd(dataToAverage$numericValue),
      uncertaintyType = "standard deviation",
      numericValue = if (is.numeric(dataToAverage$numericValue)) mean(dataToAverage$numericValue) else NA,
      stringValue = if (!is.numeric(dataToAverage$numericValue)) "flagged" else NA
    ))
  }
  treatmentGroupData <- ddply(treatmentGroupInput, .(treatmentGroupID), createTreatmentGroupData)
  
  treatmentGroupData$stateGroupIndex <- 1
  treatmentGroupData$publicData <- TRUE
  
  query(paste0(
    "UPDATE treatment_group_state
    SET ignored               = 1,
    version                   = version+1
    WHERE state_type_and_kind = 'data_results'
    AND ignored               = 0
    AND treatment_group_id    IN (", paste(treatmentGroupData$treatmentGroupID, collapse=","), ")"))
  
  query(paste0(
    "UPDATE treatment_group_value
    SET ignored               = 1,
    version                   = version+1
    WHERE treatment_state_id    IN
    (SELECT id
    FROM treatment_group_state
    WHERE state_type_and_kind = 'data_results'
    AND ignored               = 0
    AND treatment_group_id IN (", paste(treatmentGroupData$treatmentGroupID, collapse=","), "))"))
  
  treatmentGroupData$stateID <- saveStatesFromLongFormat(entityData = treatmentGroupData, entityKind = "treatmentgroup", 
                                                         stateGroups=treatmentGroupStateGroups, stateGroupIndices = 1, 
                                                         idColumn = "treatmentGroupID", recordedBy = "curveCuration", lsTransaction = lsTransaction)[['entityStateId']]
  treatmentGroupData$stateVersion <- 0
  savedTreatmentGroupValues <- saveValuesFromLongFormat(entityData = treatmentGroupData, entityKind = "treatmentgroup", 
                                                        stateGroups = treatmentGroupStateGroups, stateGroupIndices = 1,
                                                        lsTransaction = lsTransaction)
  
  #####  AnalysisGroups =======================================================
  
  analysisGroupStateGroups <- list(list(entityKind = "analysisgroups",
                                        stateType = "data", 
                                        stateKind = "Dose Response",
                                        includesOthers = TRUE,
                                        includesCorpName = TRUE))
  
  analysisGroupData$stateGroupIndex <- 1
  analysisGroupData$publicData <- TRUE
  
  analysisGroupsToIgnore <- unique(analysisGroupData$analysisGroupID)
  
  sqlAnalysisGroupIds <- paste(analysisGroupData$analysisGroupID, collapse = ",")
  query(paste0(
    "UPDATE analysis_group_state
    SET ignored               = 1,
    version                   = version+1
    WHERE state_type_and_kind = 'data_Dose Response'
    AND analysis_group_id     IN (", sqlAnalysisGroupIds, ")"))
  query(paste0(
    "UPDATE analysis_group_value
    SET ignored               = 1,
    version                   = version+1
    WHERE analysis_state_id    IN
    (SELECT id
    FROM analysis_group_state
    WHERE state_type_and_kind = 'data_Dose Response'
    AND analysis_group_id IN (", sqlAnalysisGroupIds, "))"))
  
  analysisGroupData$stateID <- saveStatesFromLongFormat(entityData = analysisGroupData, entityKind = "analysisgroup", 
                                                        stateGroups=analysisGroupStateGroups, stateGroupIndices = 1, 
                                                        idColumn = "analysisGroupID", recordedBy = "curveCuration", lsTransaction = lsTransaction)[['entityStateId']]
  analysisGroupData$stateVersion <- 0
  analysisGroupData <- rbind.fill(analysisGroupData, meltBatchCodes(analysisGroupData, 1))
  saveValuesFromLongFormat(entityData = analysisGroupData, entityKind = "analysisgroup", 
                           stateGroups = analysisGroupStateGroups, stateGroupIndices = 1,
                           lsTransaction = lsTransaction)
  return(TRUE)
}

replaceNullWithNA <- function(inputList) {
  inputList[sapply(inputList,is.null)] <- NA
  return(inputList)
}
addSubjectState <- function(subjectValue, subjectStateId) {
  print(i)
  subjectValue$stateID <- subjectStateId
  subjectValue$lsTransaction <- subjectValue$lsTransaction$id
  subjectValue <- replaceNullWithNA(subjectValue)
  return(as.data.frame(subjectValue, stringsAsFactors=FALSE))
  i <<- i+1
}
getValuesFromState <- function(subjectState) {
  require('plyr')
  return(ldply(subjectState$subjectValues, addSubjectState, subjectStateId=subjectState$id))
}
getValuesFromSubject <- function(subject) {
  require('plyr')
  return(ldply(subject$subjectStates, getValuesFromState))
}

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
getPoints <- function(curveids, renderingHint = as.character(NA), flagsAsLogical = TRUE, ...) {
  
  
  drQU <- paste("SELECT curveid, dose, doseunits, response, responseunits, flag as flag_on_load, response_ss_id, response_ss_version, response_sv_id, flag_sv_id, s_id, tg_id, ag_id from api_dose_response where curveid in (",sqliz(curveids),")",sep="")
  ivPO <- function(type)  {
    paste0("SELECT *
  	FROM
		  (SELECT s.id    AS S_ID,
                el.label_text AS experiment_name,
                'Animal-' || cl.label_text AS name,
                agv.string_value AS curveid,
                MAX(CASE WHEN sv.ls_kind = 'time' THEN sv.numeric_value ELSE NULL END)   AS dose,
                'Time' AS dosetype,
                MAX(CASE WHEN sv.ls_kind = 'time' THEN sv.unit_kind ELSE NULL END) AS doseunits,
                MAX(CASE WHEN sv.ls_kind = '",type," - PK_Concentration' THEN sv.numeric_value ELSE NULL END) AS response,
                'Conc' AS responsetype,
                MAX(CASE WHEN sv.ls_kind = '",type," - PK_Concentration' THEN sv.unit_kind ELSE NULL END) AS responseunits,
                MAX(CASE sv.ls_kind WHEN 'flag' then sv.id else null end) as flag_sv_id,
                MAX(CASE sv.ls_kind WHEN 'flag' THEN sv.string_value ELSE NULL END) AS flag_on_load,
                MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN sv.subject_state_id ELSE NULL END) AS response_ss_id,
                MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN sv.id ELSE NULL END) AS response_sv_id,
                MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN ss.version ELSE NULL END) AS response_ss_version,
                MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN s.treatment_group_id ELSE NULL END) AS tg_id,
                MAX(ag.id) AS ag_id
                FROM
                analysis_GROUP ag 
                JOIN analysis_GROUP_state ags ON ags.analysis_GROUP_id = ag.id
                JOIN analysis_GROUP_value agv ON agv.analysis_state_id = ags.id
                JOIN treatment_GROUP tg ON ag.id=tg.analysis_GROUP_id
                JOIN experiment e ON ag.experiment_id=e.id
                JOIN experiment_label el ON e.id=el.experiment_id
                JOIN subject s ON tg.id=s.treatment_GROUP_id
                JOIN subject_state ss ON ss.subject_id = s.id
                JOIN subject_value sv ON sv.subject_state_id = ss.id
                JOIN itx_subject_container itxsc ON s.id = itxsc.subject_id
                JOIN container c ON c.id=itxsc.container_id
                JOIN container_label cl ON cl.container_id    =c.id
                WHERE agv.ls_kind     = '",type," pk curve id'
                AND sv.ls_kind       IN ('time', '",type," - PK_Concentration')
                AND agv.string_value IN (  ",sqliz(curveids)," )
                GROUP BY s.id, ss.id, agv.string_value, cl.label_text, el.label_text
                )
                WHERE response IS NOT NULL
                ORDER BY tg_id ASC ")
  }
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
                  max(CASE tv.ls_kind WHEN 'flag' then tv.string_value else null end) as flag_on_load,
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
               "PO pk curve id" = ivPO("PO"),
               "IV pk curve id" = ivPO("IV")
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
                     data.frame(  curveid = as.character(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.character(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseType = as.character(points$dosetype), 
                                  doseUnits = as.character(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseType = as.character(points$responsetype),
                                  responseUnits = as.character(points$responseunits),
                                  standardDeviation = as.numeric(points$standarddeviation),
                                  flag_on.load = as.character(points$flag_on_load),
                                  response_ss_id = as.integer(points$response_ss_id),
                                  s_id = as.integer(points$s_id),
                                  tg_id = as.integer(points$tg_id),
                                  ag_id = as.integer(points$ag_id)
                     )
                   },
                   "IV pk curve id" = {
                     data.frame(  curveid = as.character(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.character(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseType = as.character(points$dosetype), 
                                  doseUnits = as.character(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseType = as.character(points$responsetype),
                                  responseUnits = as.character(points$responseunits),
                                  flag_on.load = as.character(points$flag_on_load),
                                  response_ss_id = as.integer(points$response_ss_id),
                                  response_sv_id = as.integer(points$response_sv_id),
                                  response_ss_version = as.integer(points$response_ss_version),
                                  flag_sv_id = as.integer(points$flag_sv_id),
                                  s_id = as.integer(points$s_id),
                                  tg_id = as.integer(points$tg_id),
                                  ag_id = as.integer(points$ag_id)
                     )
                   },
                   "PO pk curve id" = {
                     data.frame(  curveid = as.character(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.character(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseType = as.character(points$dosetype), 
                                  doseUnits = as.character(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseType = as.character(points$responsetype),
                                  responseUnits = as.character(points$responseunits),
                                  flag_on.load = as.character(points$flag_on_load),
                                  response_ss_id = as.integer(points$response_ss_id),
                                  response_ss_version = as.integer(points$response_ss_version),
                                  response_sv_id = as.integer(points$response_sv_id),
                                  flag_sv_id = as.integer(points$flag_sv_id),
                                  s_id = as.integer(points$s_id),
                                  tg_id = as.integer(points$tg_id),
                                  ag_id = as.integer(points$ag_id)
                     )
                   },
                   data.frame(  curveid = as.character(points$curveid),
                                name = as.character(points$curveid),
                                dose = as.numeric(points$dose), 
                                doseUnits = as.character(points$doseunits), 
                                response = as.numeric(points$response),
                                responseUnits = as.character(points$responseunits),
                                flag_on.load = as.character(points$flag_on_load),
                                response_ss_id = as.integer(points$response_ss_id),
                                response_sv_id = as.integer(points$response_sv_id),
                                response_ss_version = as.integer(points$response_ss_version),
                                flag_sv_id = as.integer(points$flag_sv_id),
                                s_id = as.integer(points$s_id),
                                tg_id = as.integer(points$tg_id),
                                ag_id = as.integer(points$ag_id)
                   )
  )
  if(nrow(points) > 0) {
    if(flagsAsLogical) {
      points$flag <- factor(points$flag_on.load, levels = c(levels(points$flag_on.load), TRUE, FALSE))
      points$flag_user <- as.character(NA)
      points$flag_algorithm <- as.character(NA)
      points$flag_temp <- as.character(NA)
    }
  }
  return(points)
}
getCurveIDAnalsysiGroupResults <- function(curveids, ...) {
  parameters <- query(paste("SELECT TESTED_LOT,
                            AG_ID,
                            AG_CODE_NAME,
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
  longFormat <- longFormat[,c("ag_id","ag_code_name","tested_lot","ls_kind","numeric_value","string_value","unit_kind", "operator_kind")]
  wideFormat <- reshape(longFormat,
                        timevar="ls_kind",
                        idvar=c("ag_id","ag_code_name","tested_lot"),direction="wide")
  
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
  parameters$renderingHint <- renderingHint
  return(parameters)
}

getLL4ParametersFromWideFormat <- function(wideFormat) {
  wideName = c("ag_code_name","tested_lot", "string_value.curve id", "string_value.Rendering Hint", "numeric_value.Min","numeric_value.Fitted Min",
               "numeric_value.Max", "numeric_value.Fitted Max", "numeric_value.Slope", "numeric_value.Fitted Slope", "numeric_value.Hill slope", "numeric_value.Fitted Hill slope", 
               "numeric_value.EC50", "numeric_value.Fitted EC50", "operator_kind.EC50")
  newName = c("ag_code_name","tested_lot", "curveid", "renderingHint", "min", "fitted_min",
              "max", "fitted_max", "slope", "fitted_slope",  "hillslope", "fitted_hillslop",
              "ec50", "fitted_ec50", "operator")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  return(extractParametersFromWideFormat(valuesToGet, wideFormat))
}

getPKParametersFromWideFormat <- function(wideFormat, renderingHint) {
  parameters <- list(renderingHint = renderingHint)
  wideName = c("ag_code_name","tested_lot", paste0("string_value.",renderingHint))
  newName = c("ag_code_name", "tested_lot", "curveid")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  parameters$parameters <- extractParametersFromWideFormat(valuesToGet, wideFormat)
  return(parameters)
}

getPOIVPKParametersFromLongFormat <- function(longFormat) {
  curveIDList <- c('PO pk curve id','IV pk curve id')
  parameters <- list(curveids = longFormat$string_value[longFormat$ls_kind %in% curveIDList])
  parameters$parameters <- subset(longFormat, ls_kind %in% curveIDList, select = c("ag_id", "tested_lot", "string_value") )
  names(parameters$parameters) <- c("ag_id","ag_code_name", "tested_lot", "curveid")
  return(parameters)  
}

getPOIVPKParametersFromWideFormat <- function(wideFormat) {
  parameters <- list(curveids = c(longFormat$"string_value.PO pk curve id",wideFormat$"string_value.IV pk curve id"))
  wideName = c("ag_code_name", "tested_lot", "string_value.PO IV pk curve id", "string_value.PO pk curve id", "string_value.IV pk curve id")
  newName = c("ag_code_name", "tested_lot", "curveid", "poPKCurveID", "ivPKCurveID")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  parameters$parameters <- extractParametersFromWideFormat(valuesToGet, wideFormat)
  return(parameters)  
}

extractParametersFromWideFormat <- function(valuesToGet, wideFormat) {
  parameters <- data.frame(ag_id = as.integer(wideFormat$ag_id), stringsAsFactors = FALSE)
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
                                               as.character(wideFormat[,colName]))
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
#' plotData(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#' plotData(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE)
#' plotData(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE)
#' plotData(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = TRUE)
#' 
#' #Ki Data (using raw data)
#' data(kiData)
#' params <- kiData$parameters
#' points <- kiData$points
#' paramNames <- c("Top", "Bottom", "HotNM", "HotKDNM", "Log10Ki")
#' KiFCT <- 'Bottom + (Top-Bottom)/(1+10^(x-log10((10^Log10Ki)*(1+HotNM/HotKDNM))))'
#' plotData(points, params, KiFCT, paramNames, drawIntercept= "Log10Ki", outFile = NA, ymin = NA, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#'
#' #PK Curves
#' #PO
#' data(poPKCurveData)
#' params <- poPKCurveData$parameters
#' curveData <- poPKCurveData$points
#' plotData(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' plotData(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #IV
#' data(ivPKCurveData)
#' params <- ivPKCurveData$parameters
#' curveData <- ivPKCurveData$points
#' plotData(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' plotData(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #IV Overlay
#' data(overlayIVPKCurveData)
#' params <- overlayIVPKCurveData$parameters
#' curveData <- overlayIVPKCurveData$points
#' plotData(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' 
#' #PO IV
#' data(poIVPKCurveData)
#' params <- poIVPKCurveData$parameters
#' curveData <- poIVPKCurveData$points
#' plotData(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, addShapes = TRUE, drawStdDevs = TRUE)
#' 

plotCurve <- function(curveData, params, fitFunction, paramNames = c("ec50", "min", "max", "slope"), drawIntercept = "ec50", outFile = NA, ymin = NA, logDose = FALSE, logResponse = FALSE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, drawCurve = TRUE, connectPoints = FALSE, plotMeans = FALSE, drawStdDevs = FALSE, addShapes = FALSE, labelAxes = FALSE, ...) {
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
  flaggedPoints <- subset(curveData, !is.na(curveData$flag_user) | !is.na(curveData$flag_algorithm) | !is.na(curveData$flag_on.load) | !is.na(curveData$flag_temp))
  goodPoints <- subset(curveData, is.na(curveData$flag_user) & is.na(curveData$flag_algorithm) & is.na(curveData$flag_on.load) & is.na(curveData$flag_temp))
  
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
    
    fittedColumnNames <- paste0("fitted_",paramNames)
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
      ylin$x <- c(params[,drawIntercept], params[,drawIntercept])
      ylin$y <- c(par("usr")[3],curveIntercept)
      #Horizontal
      xlin <- c()
      if(logDose) {
        xlin$x <- c(0.0000000000000001,params[,drawIntercept])
      } else {
        xlin$x <- c(par("usr")[1],params[,drawIntercept])
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
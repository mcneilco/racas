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
  parameters <- getCurveIDAnalsysiGroupResults(curveids, ...)
  renderingHint <- parameters[stringvalue %in% curveids]$lskind
  renderingHintParameters <- parameters[ lskind == "batch code", c("codevalue", "curveid", "valueid"), with = FALSE]
  
  #There are cases where getParametersByRenderingHint return curveids (See PK), in this case we call getParametersByRenderingHint again with those curveids 3_AG-00000040
  points <- as.data.table(getPoints(curveids, renderingHint = renderingHint, ...))
  points[ , oldcurveid := curveId ]
  points[ , curveId := paste0(curveId,"_s_id_",s_id)]
  ptMerge <- unique(points[, c("name","curveId","oldcurveid"), with = FALSE])
  setkey(ptMerge, oldcurveid)
  setkey(renderingHintParameters, curveid)
  renderingHintParameters <- renderingHintParameters[ptMerge]
  renderingHintParameters[ , curveId := curveId]
  renderingHintParameters[ , curveid := NULL]
  #Remove when sam fixes container saving
  #   renderingHintParameters[ , name := curveId]
  
  return (list(
    points = points,
    parameters = renderingHintParameters
  ))
}
getPoints <- function(curveids, renderingHint = as.character(NA), flagsAsLogical = TRUE, ...) {
  
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
           MAX(CASE sv.ls_kind WHEN 'flag' THEN sv.string_value ELSE NULL END) AS preprocessFlagStatus,
           MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN sv.subject_state_id ELSE NULL END) AS response_ss_id,
           MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN sv.id ELSE NULL END) AS response_sv_id,
           MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN ss.version ELSE NULL END) AS response_ss_version,
           MAX(CASE sv.ls_kind WHEN '",type," - PK_Concentration' THEN tg.id ELSE NULL END) AS tg_id,
           MAX(ag.id) AS ag_id
           FROM analysis_group ag
           JOIN analysis_GROUP_state ags ON ags.analysis_GROUP_id = ag.id
           JOIN analysis_GROUP_value agv ON agv.analysis_state_id = ags.id
           JOIN analysisgroup_treatmentgroup agtg on ag.id = agtg.analysis_group_id
           JOIN treatment_GROUP tg ON tg.id=agtg.treatment_GROUP_id
           JOIN experiment_analysisgroup eag on eag.analysis_group_id=ag.id
           JOIN experiment e ON eag.experiment_id=e.id
           JOIN experiment_label el ON e.id=el.experiment_id
           JOIN treatmentgroup_subject tgs on tgs.treatment_group_id=tg.id
           JOIN subject s ON s.id=tgs.subject_id
           JOIN subject_state ss ON ss.subject_id = s.id
           JOIN subject_value sv ON sv.subject_state_id = ss.id
           LEFT JOIN itx_subject_container itxsc ON s.id = itxsc.subject_id
           LEFT JOIN container c ON c.id=itxsc.container_id
           LEFT JOIN container_label cl ON cl.container_id    =c.id
           WHERE agv.ls_kind     = '",type," pk curve id'
           AND sv.ls_kind       IN ('time', '",type," - PK_Concentration')
           AND agv.string_value IN (  ",sqliz(curveids)," )
           GROUP BY s.id, ss.id, agv.string_value, cl.label_text, el.label_text
           ) foo
           WHERE response IS NOT NULL
           ORDER BY tg_id ASC ")
  }
  poIVQU <- paste("SELECT a.*, a.Route || '-' || b.Dose as name
                  FROM (
                  select max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration' ) then tg.id else null end) as s_id,
                  agv.string_value as curveid,
                  cl.label_text as animal,
                  el.label_text as experiment_name,
                  max(CASE WHEN tv.ls_kind in ('time') then tv.numeric_value else null end) as dose,
                  'Time' as dosetype,
                  max(CASE WHEN tv.ls_kind in ('time') then tv.unit_kind else null end) as doseunits,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.numeric_value else null end) as response,
                  'Conc' as responsetype,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.unit_kind else null end) as responseunits,
                  max(CASE tv.ls_kind WHEN 'flag' then tv.string_value else null end) as preprocessflagstatus,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.treatment_state_id else null end) as response_ss_id,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration' ) then tg.id else null end) as tg_id,
                  max(ag.id) AS ag_id,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.uncertainty else null end) as standardDeviation,
                  max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration') then 'PO' WHEN tv.ls_kind in ('IV - PK_Concentration') then 'IV' else null end) as Route
                  FROM analysis_group ag
                  JOIN analysis_GROUP_state ags ON ags.analysis_GROUP_id = ag.id
                  JOIN analysis_GROUP_value agv ON agv.analysis_state_id = ags.id
                  JOIN analysisgroup_treatmentgroup agtg on ag.id = agtg.analysis_group_id
                  JOIN treatment_GROUP tg ON tg.id=agtg.treatment_GROUP_id
                  JOIN experiment_analysisgroup eag on eag.analysis_group_id=ag.id
                  JOIN experiment e ON eag.experiment_id=e.id
                  JOIN experiment_label el ON e.id=el.experiment_id
                  JOIN treatment_group_state ts ON ts.treatment_group_id = tg.id
                  JOIN treatment_group_value tv ON tv.treatment_state_id = ts.id
                  JOIN treatmentgroup_subject tgs on tgs.treatment_group_id=tg.id
                  JOIN subject s ON s.id=tgs.subject_id
                  JOIN subject_state ss ON ss.subject_id = s.id
                  JOIN subject_value sv ON sv.subject_state_id = ss.id
                  LEFT JOIN itx_subject_container itxsc on s.id = itxsc.subject_id
                  LEFT JOIN container c on c.id=itxsc.container_id
                  LEFT JOIN container_label cl on cl.container_id=c.id
                  WHERE agv.ls_kind like 'PO IV pk curve id'
                  AND tv.ls_kind in ('time', 'PO - PK_Concentration', 'IV - PK_Concentration')
                  AND agv.string_value in (",sqliz(curveids),")
                  GROUP by tg.id, ts.id, agv.string_value, cl.label_text, el.label_text
                  ) a
                  LEFT OUTER JOIN (
                  SELECT tv.concentration || tv.conc_unit as Dose,
                  tg.id AS s_id
                  FROM analysis_group ag
                  JOIN analysis_GROUP_state ags ON ags.analysis_GROUP_id = ag.id
                  JOIN analysis_GROUP_value agv ON agv.analysis_state_id = ags.id
                  JOIN analysisgroup_treatmentgroup agtg on ag.id = agtg.analysis_group_id
                  JOIN treatment_GROUP tg ON tg.id=agtg.treatment_GROUP_id
                  JOIN treatment_group_state ts
                  ON ts.treatment_group_id = tg.id
                  JOIN treatment_group_value tv
                  ON tv.treatment_state_id = ts.id
                  WHERE agv.ls_kind LIKE 'PO IV pk curve id'
                  AND tv.ls_kind             IN ('batch code')
                  AND agv.string_value IN (",sqliz(curveids),")
                  ) b
                  ON a.s_id = b.s_id
                  order by tg_id asc"
  )  
  
  qu <- switch(renderingHint,
               "PO IV pk curve id" = poIVQU,
               "PO pk curve id" = ivPO("PO"),
               "IV pk curve id" = ivPO("IV")
  )
  
  points <- query(qu, ...)
  names(points) <- tolower(names(points))
  if(nrow(points)==0) {
    stop("Got 0 rows of points")
  }
  points <- switch(renderingHint,
                   "PO IV pk curve id" = {
                     data.frame(  curveId = as.character(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.character(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseKind = as.character(points$dosetype), 
                                  doseUnits = as.character(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseKind = as.character(points$responsetype),
                                  responseUnits = as.character(points$responseunits),
                                  standardDeviation = as.numeric(points$standarddeviation),
                                  preprocessFlagStatus = as.character(points$preprocessflagstatus),
                                  response_ss_id = as.integer(points$response_ss_id),
                                  s_id = as.integer(points$s_id),
                                  tg_id = as.integer(points$tg_id),
                                  ag_id = as.integer(points$ag_id)
                     )
                   },
                   "IV pk curve id" = {
                     data.frame(  curveId = as.character(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.character(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseKind = as.character(points$dosetype), 
                                  doseUnits = as.character(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseKind = as.character(points$responsetype),
                                  responseUnits = as.character(points$responseunits),
                                  preprocessFlagStatus = as.character(points$preprocessflagstatus),
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
                     data.frame(  curveId = as.character(points$curveid),
                                  name = gsub(paste0(unique(as.character(points$experiment_name)),"_"),"",as.character(points$name)),
                                  dose = as.numeric(points$dose), 
                                  doseKind = as.character(points$dosetype), 
                                  doseUnits = as.character(points$doseunits), 
                                  response = as.numeric(points$response),
                                  responseKind = as.character(points$responsetype),
                                  responseUnits = as.character(points$responseunits),
                                  preprocessFlagStatus = as.character(points$preprocessflagstatus),
                                  response_ss_id = as.integer(points$response_ss_id),
                                  response_ss_version = as.integer(points$response_ss_version),
                                  response_sv_id = as.integer(points$response_sv_id),
                                  flag_sv_id = as.integer(points$flag_sv_id),
                                  s_id = as.integer(points$s_id),
                                  tg_id = as.integer(points$tg_id),
                                  ag_id = as.integer(points$ag_id)
                     )
                   }
  )
  points$preprocessFlagStatus <- as.character(points$preprocessFlagStatus)
  points[is.na(points$preprocessFlagStatus),]$preprocessFlagStatus <- ""
  points$userFlagStatus <- ""
  points$algorithmFlagStatus <- ""
  points$tempFlagStatus <- ""
  return(points)
  }

getCurveIDAnalsysiGroupResults <- function(curveids, ...) {
  parameters <- rbindlist(query_replace_string_with_values("SELECT lsvalues0_.analysis_state_id AS stateId,
                                                           lsvalues0_.id                     AS valueId,
                                                           lsvalues0_.code_kind              AS codeKind,
                                                           lsvalues0_.code_origin            AS codeOrigin,
                                                           lsvalues0_.code_type              AS codeType,
                                                           lsvalues0_.code_value             AS codeValue,
                                                           lsvalues0_.comments               AS comments,
                                                           lsvalues0_.conc_unit              AS concUnit,
                                                           lsvalues0_.concentration          AS concentration,
                                                           lsvalues0_.ls_kind                AS lsKind,
                                                           lsvalues0_.ls_transaction         AS lsTransaction,
                                                           lsvalues0_.ls_type                AS lsType,
                                                           lsvalues0_.numeric_value          AS numericValue,
                                                           lsvalues0_.operator_kind          AS operatorKind,
                                                           lsvalues0_.operator_type          AS operatorType,
                                                           lsvalues0_.public_data            AS publicData,
                                                           lsvalues0_.recorded_by            AS recordedBy,
                                                           lsvalues0_.recorded_date          AS recordedDate,
                                                           lsvalues0_.string_value           AS stringValue,
                                                           lsvalues0_.uncertainty            AS uncertainty,
                                                           lsvalues0_.uncertainty_type       AS uncertaintyType,
                                                           lsvalues0_.unit_kind              AS unitKind,
                                                           lsvalues0_.unit_type              AS unitType,
                                                           lsvalues0_.url_value              AS urlValue,
                                                           lsvalues0_.version                AS version,
                                                           analysisgr0_.string_value         AS curveId
                                                           FROM analysis_group_value analysisgr0_
                                                           INNER JOIN analysis_group_state analysisgr1_
                                                           ON analysisgr0_.analysis_state_id=analysisgr1_.id
                                                           INNER JOIN analysis_group analysisgr2_
                                                           ON analysisgr1_.analysis_group_id=analysisgr2_.id
                                                           INNER JOIN experiment_analysisgroup expt_ag_group
                                                           ON analysisgr2_.id=expt_ag_group.analysis_group_id
                                                           INNER JOIN analysis_group_value lsvalues0_
                                                           ON analysisgr1_.id         =lsvalues0_.analysis_state_id
                                                           WHERE analysisgr0_.ls_type ='stringValue'
                                                           AND analysisgr0_.ls_kind in ('PO IV pk curve id','IV pk curve id','PO pk curve id')
                                                           AND analysisgr0_.ignored = '0'
                                                           AND analysisgr1_.ignored = '0'
                                                           AND analysisgr2_.ignored = '0'
                                                           AND analysisgr1_.ls_type ='data'
                                                           AND analysisgr0_.string_value in (REPLACEME)", string = "REPLACEME", values = curveids, ...))
  setnames(parameters, tolower(names(parameters)))
  return(parameters)
}

#' Curve plotting function
#'
#' This function takes in a set of data points, curve parameters, and an equation and plots the data
#'
#' @param curveData a data frame with the points with column names curveId, dose, response, flag
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
#' plotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#' plotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE)
#' plotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE)
#' plotCurve(curveData, params, paramNames = c("ec50", "min", "max", "hill"), LL4, outFile = NA, ymin = NA, logDose = TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = TRUE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = TRUE)
#' 
#' #Ki Data (using raw data)
#' data(kiData)
#' params <- kiData$parameters
#' points <- kiData$points
#' paramNames <- c("Top", "Bottom", "HotNM", "HotKDNM", "Log10Ki")
#' KiFCT <- 'Bottom + (Top-Bottom)/(1+10^(x-log10((10^Log10Ki)*(1+HotNM/HotKDNM))))'
#' plotCurve(points, params, KiFCT, paramNames, drawIntercept= "Log10Ki", outFile = NA, ymin = NA, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE)
#'
#' #PK Curves
#' #PO
#' data(poPKCurveData)
#' params <- poPKCurveData$parameters
#' curveData <- poPKCurveData$points
#' plotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' plotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #IV
#' data(ivPKCurveData)
#' params <- ivPKCurveData$parameters
#' curveData <- ivPKCurveData$points
#' plotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' plotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE, addShapes = TRUE)
#' 
#' #IV Overlay
#' data(overlayIVPKCurveData)
#' params <- overlayIVPKCurveData$parameters
#' curveData <- overlayIVPKCurveData$points
#' plotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, drawStdDevs = FALSE)
#' 
#' #PO IV
#' data(poIVPKCurveData)
#' params <- poIVPKCurveData$parameters
#' curveData <- poIVPKCurveData$points
#' plotCurve(curveData, params, paramNames = NA, outFile = NA, ymin = NA, logDose = FALSE, logResponse=TRUE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, plotMeans = FALSE, connectPoints = TRUE, drawCurve = FALSE, addShapes = TRUE, drawStdDevs = TRUE)
#' 

plotCurve <- function(curveData, params, fitFunction, paramNames = c("ec50", "min", "max", "slope"), drawIntercept = "ec50", outFile = NA, ymin = NA, logDose = FALSE, logResponse = FALSE, ymax = NA, xmin = NA, xmax = NA, height = 300, width = 300, showGrid = FALSE, showLegend = FALSE, showAxes = TRUE, drawCurve = TRUE, drawFlagged = FALSE, connectPoints = FALSE, plotMeans = FALSE, drawStdDevs = FALSE, addShapes = FALSE, labelAxes = FALSE, curveXrn = c(NA, NA), mostRecentCurveColor = NA, axes = c("x","y"), modZero = TRUE, drawPointsForRejectedCurve = racas::applicationSettings$server.curveRender.drawPointsForRejectedCurve, plotColors = c("black"),curveLwd = 1, plotPoints = TRUE, xlabel = NA, ylabel = NA) {
  #Check if paramNames match params column headers
  if(!is.na(paramNames) && drawCurve == TRUE) {
  } else {
    drawCurve <- FALSE
    drawIntercept <- NA
  }
  if(is.null(curveLwd) || is.na(curveLwd)) {
    curveLwd <- 1
    if(!is.null(racas::applicationSettings$server.curveRender.curveLwd) && racas::applicationSettings$server.curveRender.curveLwd != "") {
      curveLwd <- racas::applicationSettings$server.curveRender.curveLwd
    }
  }
  curveLwd <- as.numeric(curveLwd)
  
  # Determine if overlay
  overlay <- FALSE
  if(nrow(params) > 1) {
    overlay <- TRUE
  }
  drawPoints <- TRUE
  if(!is.null(plotPoints) && !is.na(plotPoints) ) {
    drawPoints <- plotPoints
  } else {
    drawPoints <- !overlay || racas::applicationSettings$server.curveRender.plotPointsOnOverlay
  }

  #Yay Pythagoras
  defaultDiagonal <- sqrt(formals(plotCurve)$height^2+formals(plotCurve)$width^2)
  scaleFactor <- sqrt(height^2+width^2)/defaultDiagonal
  scaleFactor <- max(scaleFactor, 0.7)
  
  #Assign Colors
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
            rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  if(is.null(plotColors) | length(plotColors) == 0) {
    plotColors <- "black"
    if(!is.null(racas::applicationSettings$server.curveRender.plotColors) && racas::applicationSettings$server.curveRender.usePlotColorsByDefault) {
      plotColors <- trimws(strsplit(racas::applicationSettings$server.curveRender.plotColors,",")[[1]])
    }
  }

  if(is.na(mostRecentCurveColor)) {
    if(!is.null(racas::applicationSettings$server.curveRender.mostRecentCurveColor) && racas::applicationSettings$server.curveRender.mostRecentCurveColor != "") {
      mostRecentCurveColor <- trimws(racas::applicationSettings$server.curveRender.mostRecentCurveColor)
    }
  }
  
  if("recordedDate" %in% names(params)) {
    params <- params[order(params$recordedDate, decreasing = TRUE),]
  }
  if(!"color" %in% names(params)) {
    if(nrow(params) > 1 && !is.na(mostRecentCurveColor)) {
      params$color <- mostRecentCurveColor
      params[2:nrow(params), ]$color <- rep_len(plotColors,nrow(params)-1)
    } else {
      params$color <- rep_len(plotColors,nrow(params))
  #     params$color <- grDevices::cm.colors(nrow(params), alpha = 1)
    }
  }
  plotColorsAlpha <- add.alpha(params$color, alpha=0.3)
  curveData$color <- params$color[match(curveData$curveId,params$curveId)] 
  curveData$coloralpha <- plotColorsAlpha[match(curveData$curveId,params$curveId)] 
  
  #Add shapes
  if(addShapes) {
    pchs <- 1:24
    pchs <- rep(pchs[-c(4)],100, replace = TRUE)
    params$pch <- pchs[1:nrow(params)]
    curveData$pch <- params$pch[match(curveData$curveId,params$curveId)]
  }
  
  #Doses at 0 don't really make sense (and won't work) so this function moves 0 doses down one more dose (calculated by using the next two doses)
  #If the function can't 
  if(modZero && drawPoints) {
    curveData <- modify_or_remove_zero_dose_points(curveData, logDose)
  }
  
  if(!drawPointsForRejectedCurve && drawPoints) {
    rejectedCurveIds <-  params$curveId[params$userFlagStatus == "rejected" && params$algorithmFlagStatus != "no fit"]
    curveData <- subset(curveData, !curveId %in% rejectedCurveIds)
  }
  
  ##Seperate Flagged and good points for plotting different point shapes..etc.
  if(drawPoints) {
    flaggedPoints <- subset(curveData, userFlagStatus=="knocked out" | preprocessFlagStatus=="knocked out" | algorithmFlagStatus=="knocked out" | tempFlagStatus=="knocked out")
    goodPoints <- subset(curveData, userFlagStatus!="knocked out" & preprocessFlagStatus!="knocked out" & algorithmFlagStatus!="knocked out" & tempFlagStatus!="knocked out")
  }

  ##Calculate Means and SDs
  if(drawPoints && nrow(goodPoints) > 0) {
    sds <- aggregate(goodPoints$response,list(dose=goodPoints$dose,curveId=goodPoints$curveId, color = goodPoints$color), sd)
    names(sds)[ncol(sds)] <- "sd"
    means <- aggregate(goodPoints$response,list(dose=goodPoints$dose,curveId=goodPoints$curveId, color = goodPoints$color), mean)
    names(means)[ncol(means)] <- "mean"
  }
  
  ###Begin Drawing the Plot
  if(!is.na(outFile)) {
    png(file = outFile, height = height, width = width)
    on.exit(dev.off())
  }
  
  originalMargins <- par("mar")
  plotError <- function(error) {
    if(!is.na(outFile)) {
      dev.off(dev.prev())
      png(file = outFile)
    }
    par(mar = originalMargins)
    plot(-1:1, -1:1, type = "n", xlab = NA, ylab = NA, axes = FALSE)
    text(c(0,0),c(0,0),labels=c(error$message))
  }
  tryCatch({
    #Axes and Labels require extra margins
    #TODO: make this a bit nicer, right now there is probably too much padding in the margins when label is on
    defaultMargins=c(0.1,1,0.3,0.8)
    #par(mar=c(2.1,3,0.1,0.1)) #Set margin to east to fit legend
    margins <- defaultMargins
    if(labelAxes) {
      margins[c(1,2)] <- defaultMargins[c(1,2)] + 4
    } else {
      if(showAxes) {
        marginAdd <- c()
        if("x" %in% axes) {
          marginAdd <- 1
        }
        if("y" %in% axes) {
          marginAdd <- c(marginAdd,2)
        }
        margins[marginAdd] <- defaultMargins[marginAdd] + 2
      }
    }
    par(mar = margins)
    #Determine which axes will require log scale plotting
    plotLog <- paste0(ifelse(logDose, "x", ""),ifelse(logResponse, "y", ""))
    
    getDrawValues <- function(params) {
      reportedValueColumns <- match(paramNames, names(params))
      reportedValueColumns <- reportedValueColumns[!is.na(reportedValueColumns)]
      reportedValues <- sapply(params[,reportedValueColumns], as.numeric)
      reportedValues <- reportedValues[sapply(reportedValues, function(x) !any(is.na(x)))] 
      if(length(paramNames) == 1) {
        reportedValues <- data.frame(reportedValues)
        names(reportedValues) <- paramNames
      }
      
      tmp <- data.frame(matrix(nrow=1, ncol=length(paramNames))) 
      names(tmp) <- paramNames
      tmp[1,match(names(reportedValues), paramNames)] <- reportedValues
      
      fittedColumnNames <- tolower(gsub(" ", "", paste0("fitted",paramNames)))
      fittedValueColumns <- match(fittedColumnNames,gsub(" ", "", tolower(names(params))))
      fittedValueColumns <- fittedValueColumns[!is.na(fittedValueColumns)]
      
      if(length(fittedValueColumns) > 0) {
        fittedValues <-  params[,fittedValueColumns]
        fittedValues <- fittedValues[sapply(fittedValues, function(x) !any(is.na(x)))] 
        tmp[1,match(tolower(names(fittedValues)),fittedColumnNames)] <- fittedValues
      }
      return(tmp)
    }
    
    #Determine axes ranges
    plot_limits <- get_plot_window(curveData, logDose = logDose, logResponse = logResponse, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax)
    xrn <- plot_limits[c(1,3)]
    yrn <- plot_limits[c(4,2)]
    if(is.na(curveXrn[1])) {
      curveXrn[1] <- xrn[1]
    }
    if(length(curveXrn) == 1 | is.na(curveXrn[2])) {
      curveXrn[2] <- xrn[2]
    }
    
    #Curve Drawing Function
    extractCurveData <- function(cid) {
      flagged <- any(params[cid,]$userFlagStatus == "rejected" && params[cid,]$algorithmFlagStatus != "no fit")
      curveID <- params$curveId[cid]
      curveParams <- subset(params, params$curveId == curveID)
      color <- curveParams$color
      curveData <- NULL
      if(drawFlagged == FALSE && !flagged) {
        drawValues <- getDrawValues(params = params[cid,])
        for(i in 1:ncol(drawValues)) {
          assign(names(drawValues)[i], drawValues[,i])
        }
        fct <- eval(parse(text=paste0('function(x) ', fitFunction)))
        curveData <- getCurveRangeData(fct, from = curveXrn[1], to = curveXrn[2], log = plotLog)
      }
      return(list(curveID=curveID, curveData=curveData, color=color))
    }
    
    # getCurveData 
    curveData <- lapply(1:length(params$curveId), extractCurveData)
    curveDataDT <- rbindlist(Map(function(x) x$curveData, curveData))
    
    #Determine axes ranges
    if(!drawPoints && nrow(curveDataDT) > 0) {
      curveDataDT <- setnames(curveDataDT, c('dose','response'))
      plot_limits <- get_plot_window(as.data.frame(curveDataDT), logDose = logDose, logResponse = logResponse, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax)
    }
    xrn <- plot_limits[c(1,3)]
    yrn <- plot_limits[c(4,2)]

    
    #First Plot Good Points so we that can see the flagged points if they are overlayed
    plotPoints <- function(yrn, pts, ...) {
      if(!plotMeans) {
        #TODO: what if plotMeans but also plotPoints? deal with that later
        plot(pts$dose, pts$response, log = plotLog, col = pts$color, pch = pts$pch, xlim = xrn, ylim = yrn, xaxt = "n", family = "sans", axes = FALSE, ylab = "", xlab = "", cex = 1*scaleFactor, ...)
      } else {
        plot(means$dose, means$mean, log = plotLog, col = means$color, xlim = xrn, ylim = yrn, xaxt = "n", family = "sans", axes = FALSE, ylab = "", xlab = "", cex = 1*scaleFactor, ...)
      }
      if(drawStdDevs) {
        plotCI(x=pts$dose,y=pts$response, uiw=pts$standardDeviation, col = pts$color, add=TRUE,err="y",pch=NA)
      }
    }

    #Draw Legend if specified
    if(drawPoints && nrow(goodPoints) > 0) {
      plotPoints(yrn = yrn, goodPoints)
    } else {
      plot.new()
      plot.window(xrn,yrn, log = plotLog)
    }

    if(showLegend) {
      #par(xpd=TRUE) # allows legends to be printed outside plot area
      #legendYPosition <- 10 ^ par("usr")[2]
      #legendXPosition <- par("usr")[4]
      legendDF <- data.frame(color = params$color, stringsAsFactors = FALSE)
      if(is.null(params$name)) {
        legendDF$text <- params$curveId
      } else {
        legendDF$text <- params$name
      }
      legendDF <- unique(legendDF)
      legendPCH <- params$pch
      
      legendLineWidth <- 1
      leg <- legend("topright",legend = legendDF$text, col = legendDF$color, lty = legendLineWidth, pch = legendPCH, cex=0.7*scaleFactor, box.lwd = 0)
      if(drawPoints) {
        if(nrow(goodPoints) > 0) {
          plotPoints(yrn = c(yrn[1], yrn[2] + leg$rect$h), goodPoints)
        } else {
          plotPoints(yrn = c(yrn[1], yrn[2] + leg$rect$h), flaggedPoints)
        }
      }
      leg <- legend("topright",legend = legendDF$text, col = legendDF$color, lty = legendLineWidth, pch = legendPCH, cex=0.7*scaleFactor, box.lwd = 0)
    }
    #If grid, then add grid
    if(showGrid) {
      grid(lwd = 1.7*scaleFactor)
    }
    if(drawPoints && connectPoints && exists("means")) {
      cids <- unique(means$curveId)
      for(c in 1:length(cids)) {
        cid <- cids[c]
        lineData <- subset(means, means$curveId == cid)
        lines(x = lineData$dose, y = lineData$mean, col = lineData$color, pch = 4, lty = 'dotted', lwd = 1.2*scaleFactor)
      }
    }
    
    #Now Plot Flagged Points
    if(drawPoints && nrow(flaggedPoints) > 0) {
      points(x = flaggedPoints$dose, y = flaggedPoints$response, col = flaggedPoints$coloralpha, pch = 4)
    }
    #Draw Error Bars and Means
    #plotCI(x=means$dose,y=means$MEAN,uiw=sds$SD,add=TRUE,err="y",pch="-")

    #Actually Draw Curves
    if(drawCurve) {
      null <- lapply(curveData,function(x) lines(x = x$curveData$x, y = x$curveData$y, type = "l", col = x$color, lwd = curveLwd*scaleFactor))
    }
    ##DO axes and Grid
    box()
    if(showAxes) {
      if("x" %in% axes) {
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
      }
      if("y" %in% axes) {
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
    }
    ##If only one curve then draw ac50 lines
    #Get coordinates to draw lines through curve at AC50
    #Vertical
    if(!is.na(drawIntercept) && !is.na(as.numeric(params[,drawIntercept]))) {
      if(nrow(params) == 1) {
        drawValues <- getDrawValues(params = params[1,])
        for(i in 1:ncol(drawValues)) {
          assign(names(drawValues)[i], drawValues[,i])
        }
        fct <- eval(parse(text=paste0('function(x) ', fitFunction)))
        curveIntercept <- fct(as.numeric(params[,drawIntercept]))
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
        if(!is.NULLorNA(params$operator)) {
          col <- '#ff0000'
        } else {
          col <- '#808080'
        }
        lines(ylin,lty = 2, lwd = 2.0*scaleFactor,col= col)
        lines(xlin, lty = 2, lwd = 2.0*scaleFactor,col= col)
      }
    }
    if(labelAxes) {
      if(is.na(xlabel)) {
        xlabel <- paste0(ifelse(is.null(curveData$doseKind[1]) || is.na(curveData$doseKind[1]),'Dose',as.character(curveData$doseKind[1])), ifelse(is.null(curveData$doseUnits[1]) || is.na(curveData$doseUnits[1]),'',paste0(" (",as.character(curveData$doseUnits[1]),")")))
      }
      if(is.na(ylabel)) {
        ylabel <- paste0(ifelse(is.null(curveData$responseKind[1]) || is.na(curveData$responseKind[1]),'Response',as.character(curveData$responseKind[1])), ifelse(is.null(curveData$responseUnits[1]) || is.na(curveData$responseUnits[1]),'',paste0(" (",as.character(curveData$responseUnits[1]),")")))
      }
      title(xlab = xlabel, ylab = ylabel)
    }
  }, error = plotError)
}

is.NULLorNA <- function(value) {
  if(is.null(value)) return(TRUE)
  return(is.na(value))
}

modify_or_remove_zero_dose_points <- function(points, logDose) {
  points <- as.data.table(points)
  setkey(points, dose)
  points[dose==0, dose := rep(
    points[ , {
      doses <- unique(dose)
      if(length(doses) > 2) {
        values <- unique(doses)[2:3]
        if(logDose) {
          answer <- 10^(log10(values[1]) - (log10(values[2])-log10(values[1])))
        } else {
          answer <- values[1] - (values[2] - values[1])
        }
      } else {
        answer <- 0
      }
      answer
    },
    by = curveId]$V1,
    .N)]
  return(points[dose!=0,])
}

get_curve_curator_url <- function(curveid, ...) {
  experimentCode <- query(paste0("SELECT e.code_name
                                 FROM experiment e
                                 JOIN experiment_analysisgroup eag ON e.id = eag.experiment_id
                                 JOIN analysis_group ag ON ag.id = eag.analysis_group_id
                                 JOIN analysis_group_state ags on ags.analysis_group_id=ag.id
                                 JOIN analysis_group_value agv on agv.analysis_state_id=ags.id
                                 WHERE agv.string_value = ",sqliz(curveid),"
                                 AND agv.ls_kind        = 'curve id'"),...)
  url <- paste(getSSLString(), applicationSettings$client.host, ":",
               applicationSettings$client.port,
               "/curveCurator/",experimentCode,"/",curveid,
               sep = "") 
  return(url)
}
api_get_curve_curator_url <- function(curveid, inTable, ...) {
  if(is.null(inTable) || as.logical(inTable) == TRUE || length(curveid) != 1) {
    return(list(shouldRedirect = FALSE, url = ""))
  } else {
    url <- get_curve_curator_url(curveid, ...)
    return(list(shouldRedirect = TRUE, url = url))
  }
}
get_rendering_hint_options <- function(renderingHint = NA) {
  renderingOptions <- switch(renderingHint,
                             "4 parameter D-R" = list(fct = LL4, paramNames = c("ec50", "min", "max", "slope"), drawIntercept = "ec50"),
                             "4 parameter D-R IC50" = list(fct = LL4IC50, paramNames = c("ic50", "min", "max", "slope"), drawIntercept = "ic50"),
                             "4 parameter D-R IC50/DMax" = list(fct = LL4IC50, paramNames = c("ic50", "min", "max", "slope"), drawIntercept = "ic50"),
                             "Ki Fit" = list(fct = OneSiteKi, paramNames = c("ki", "min", "max", "kd", "ligandConc"),drawIntercept = "ki" ),
                             "Michaelis-Menten" = list(fct = MM2, paramNames = c("km", "vmax"),drawIntercept =  NA),
                             "Substrate Inhibition" = list(fct = substrateInhibitionEq, paramNames = c("vmax", "km", "ki"),drawIntercept =  NA),
                             {
                               modelFitClasses <- rbindlist(fromJSON(applicationSettings$client.curvefit.modelfitparameter.classes), fill = TRUE)
                               source(file.path(applicationSettings$appHome,modelFitClasses[code==renderingHint]$RSource), local = TRUE)
                               if(exists('renderingOptions')) {
                                 renderingOptions
                               } else {
                                 list(fct = NA, paramNames = NA, drawIntercept = NA)
                               }
                             }
  )
  
  modelFitClasses <- jsonlite::fromJSON(applicationSettings$client.curvefit.modelfitparameter.classes)
  modelClass <- modelFitClasses[modelFitClasses$code == renderingHint,]
  if("renderOptions" %in% names(modelClass)) {
    renderingHintConfigs <- as.list(modelClass$renderOptions)
    renderingOptions <- combine.lists(renderingOptions, renderingHintConfigs)
  }
  return(renderingOptions)
}

parse_params_curve_render_dr <- function(getParams = GET) {
  # Get data
  if(is.null(getParams$ymin)) {
    yMin <- NA
  } else {
    yMin <- as.numeric(getParams$ymin)
  }
  if(!is.null(getParams$yNormMin)) {
    yMin <- as.numeric(getParams$yNormMin)
  }
  if(is.null(getParams$ymax)) {
    yMax <- NA
  } else {
    yMax <- as.numeric(getParams$ymax)
  }
  if(!is.null(getParams$yNormMax)) {
    yMax <- as.numeric(getParams$yNormMax)
  }
  if(is.null(getParams$xmin)) {
    xMin <- NA
  } else {
    xMin <- as.numeric(getParams$xmin)
  }
  if(!is.null(getParams$xNormMin)) {
    xMin <- as.numeric(getParams$xNormMin)
  }
  if(is.null(getParams$xmax)) {
    xMax <- NA
  } else {
    xMax <- as.numeric(getParams$xmax)
  }
  if(!is.null(getParams$xNormMax)) {
    xMax <- as.numeric(getParams$xNormMax)
  }
  if(is.null(getParams$height)) {
    height <- 500
  } else {
    height <- as.numeric(getParams$height)
  }
  if(!is.null(getParams$cellHeight)) {
    height <- as.numeric(getParams$cellHeight)
  }
  if(is.null(getParams$width)) {
    width <- 700
  } else {
    width <- as.numeric(getParams$width)
  }
  if(!is.null(getParams$cellWidth)) {
    width <- as.numeric(getParams$cellWidth)
  }
  if(is.null(getParams$inTable)) {
    inTable <- FALSE
  } else {
    inTable <- as.logical(getParams$inTable)
  }
  if(is.null(getParams$showAxes)) {
    showAxes <- TRUE
  } else {
    showAxes <- as.logical(getParams$showAxes)
  }
  if(is.null(getParams$axes)) {
    axes <- c("x","y")
  } else {
    axes <- strsplit(getParams$axes, ",")[[1]]
  }
  if(is.null(getParams$showGrid)) {
    showGrid <- !inTable
  } else {
    showGrid <- as.logical(getParams$showGrid)
  }
  if(is.null(getParams$labelAxes)) {
    labelAxes <- !inTable
  } else {
    labelAxes <- as.logical(getParams$labelAxes)
  }
  if(is.null(getParams$legend)) {
    legend <- !inTable
  } else {
    legend <- as.logical(getParams$legend)
  }
  if(is.null(getParams$plotPoints)) {
    plotPoints <- !inTable
  } else {
    plotPoints <- as.logical(getParams$plotPoints)
  }
  if(is.null(getParams$curveIds)) {
    stop("curveIds not provided, provide curveIds")
    DONE
  } else {
    curveIds <- getParams$curveIds
    curveIdsStrings <- strsplit(curveIds,",")[[1]]
    curveIds <- suppressWarnings(as.integer(curveIds))
    if(is.na(curveIds)) {
      curveIds <- curveIdsStrings
    }
  }
  if(is.null(getParams$plotColors)) {
    plotColors <-  c()
  } else {
    plotColors <- getParams$plotColors
    plotColors <- strsplit(plotColors,",")[[1]]
  }
  if(is.null(getParams$mostRecentCurveColor)) {
    mostRecentCurveColor <- NA
  } else {
    mostRecentCurveColor <- getParams$mostRecentCurveColor
  }
  if(is.null(getParams$curveLwd)) {
    curveLwd <- NA
  } else {
    curveLwd <- getParams$curveLwd
  }
  if(is.null(getParams$colorBy)) {
    colorBy <- NA
  } else {
    colorBy <- getParams$colorBy
  }
  if(is.null(getParams$logDose)) {
    logDose <- NA
  } else {
    logDose <- as.logical(getParams$logDose)
  }
  if(is.null(getParams$logResponse)) {
    logResponse <- NA
  } else {
    logResponse <- as.logical(getParams$logResponse)
  }
  if(is.null(getParams$logResponse)) {
    logResponse <- NA
  } else {
    logResponse <- as.logical(getParams$logResponse)
  }
  if(is.null(getParams$xLab)) {
    xLab <- NA
  } else {
    xLab <- getParams$xLab
  }
  if(is.null(getParams$yLab)) {
    yLab <- NA
  } else {
    yLab <- getParams$yLab
  }
  return(list(yMin = yMin, yMax = yMax, xMin = xMin, xMax = xMax, height = height, width = width, inTable = inTable, showAxes = showAxes, labelAxes = labelAxes, showGrid = showGrid, plotPoints = plotPoints, legend = legend, curveIds = curveIds, axes = axes, mostRecentCurveColor = mostRecentCurveColor, plotColors = plotColors, curveLwd=curveLwd, colorBy = colorBy, logDose = logDose, logResponse = logResponse, xLab = xLab, yLab = yLab))
}


getCurveRangeData <- function (expr, from = NULL, to = NULL, n = 101, type = "l", xname = "x", log = NULL, xlim = NULL) {
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  }
  else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
          all.vars(sexpr))) 
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
                    xname), domain = NA)
    expr <- sexpr
  }

  if (is.null(from) || is.null(to)) {
    xl <- if (!is.null(xlim)) 
      xlim
    else if (!addF) {
      pu <- par("usr")[1L:2L]
      if (par("xaxs") == "r") 
        pu <- extendrange(pu, f = -1/27)
      if (par("xlog")) 
        10^pu
      else pu
    }
    else c(0, 1)
    if (is.null(from)) 
      from <- xl[1L]
    if (is.null(to)) 
      to <- xl[2L]
  }
  lg <- if (length(log)) 
    log
  else if (!addF && par("xlog")) 
    "x"
  else ""
  if (length(lg) == 0) 
    lg <- ""
  if (grepl("x", lg, fixed = TRUE)) {
    if (from <= 0 || to <= 0) 
      stop("'from' and 'to' must be > 0 with log=\"x\"")
    x <- exp(seq.int(log(from), log(to), length.out = n))
  }
  else x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())
  if (length(y) != length(x)) 
    stop("'expr' did not evaluate to an object of length 'n'")
  invisible(list(x = x, y = y))
}
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
    points$curveid <- paste0(points$curveid,"_s_id_",points$s_id)
    renderingHintParameters <- renderingHintParameters$parameters
    renderingHintParameters <- merge(renderingHintParameters, unique(points$s_id))
    renderingHintParameters$curveid <- paste0(renderingHintParameters$curveid,"_s_id_",renderingHintParameters$y)
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
  poQU <- paste("select * from (
                 select s.id AS S_ID,
                 api_agsvb.string_value as curveid,
                 max(CASE WHEN sv.ls_kind in ('time') then sv.numeric_value else null end) as dose,
                 'Time' as dosetype,
                 max(CASE WHEN sv.ls_kind in ('time') then sv.unit_kind else null end) as doseunits,
                 max(CASE WHEN sv.ls_kind in ('PO - PK_Concentration') then sv.numeric_value else null end) as response,
       	         'Conc' as responsetype,
                max(CASE WHEN sv.ls_kind in ('PO - PK_Concentration') then sv.unit_kind else null end) as responseunits,
                 max(CASE sv.ls_kind WHEN 'flag' then sv.string_value else null end) as Flag,
                  max(CASE sv.ls_kind WHEN 'PO - PK_Concentration' then sv.subject_state_id else null end) as response_ss_id,
                 max(CASE sv.ls_kind WHEN 'PO - PK_Concentration' then s.treatment_group_id else null end) as tg_id,
   	             max(api_agsvb.AG_ID) AS ag_id
                 FROM api_analysis_group_results api_agsvb JOIN treatment_GROUP tg on api_agsvb.ag_id=tg.analysis_GROUP_id
                 JOIN subject s on tg.id=s.treatment_GROUP_id
                 JOIN subject_state ss ON ss.subject_id = s.id
                 JOIN subject_value sv ON sv.subject_state_id = ss.id
                 WHERE api_agsvb.ls_kind like 'PO pk curve id'
                 AND sv.ls_kind in ('time', 'PO - PK_Concentration')
                 AND api_agsvb.string_value in (",sqliz(curveids)," )
                 GROUP by s.id, ss.id, api_agsvb.string_value)
                 where response is not null
                 order by tg_id asc")
  ivQU <- paste("select * from (
                 select s.id AS S_ID,
                 api_agsvb.string_value as curveid,
                 max(CASE WHEN sv.ls_kind in ('time') then sv.numeric_value else null end) as dose,
      	         'Time' as dosetype,
                 max(CASE WHEN sv.ls_kind in ('time') then sv.unit_kind else null end) as doseunits,
                 max(CASE WHEN sv.ls_kind in ('IV - PK_Concentration') then sv.numeric_value else null end) as response,
      	         'Conc' as responsetype,
                 max(CASE WHEN sv.ls_kind in ('IV - PK_Concentration') then sv.unit_kind else null end) as responseunits,
                 max(CASE sv.ls_kind WHEN 'flag' then sv.string_value else null end) as Flag,
                  max(CASE sv.ls_kind WHEN 'IV - PK_Concentration' then sv.subject_state_id else null end) as response_ss_id,
                 max(CASE sv.ls_kind WHEN 'IV - PK_Concentration' then s.treatment_group_id else null end) as tg_id,
                  max(api_agsvb.AG_ID) AS ag_id
                 FROM api_analysis_group_results api_agsvb JOIN treatment_GROUP tg on api_agsvb.ag_id=tg.analysis_GROUP_id
                 JOIN subject s on tg.id=s.treatment_GROUP_id
                 JOIN subject_state ss ON ss.subject_id = s.id
                 JOIN subject_value sv ON sv.subject_state_id = ss.id
                 WHERE api_agsvb.ls_kind like 'IV pk curve id'
                 AND sv.ls_kind in ('time', 'IV - PK_Concentration')
                 AND api_agsvb.string_value in (",sqliz(curveids)," )
                 GROUP by s.id, ss.id, api_agsvb.string_value)
                 where response is not null
                 order by tg_id asc")
  poIVQU <- paste("select max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration' ) then tg.id else null end) as s_id,
				api_agsvb.string_value as curveid,
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
				 max(CASE WHEN tv.ls_kind in ('PO - PK_Concentration', 'IV - PK_Concentration') then tv.uncertainty else null end) as standardDeviation
				FROM api_analysis_group_results api_agsvb JOIN treatment_GROUP tg on api_agsvb.ag_id=tg.analysis_GROUP_id
					JOIN treatment_group_state ts ON ts.treatment_group_id = tg.id
					JOIN treatment_group_value tv ON tv.treatment_state_id = ts.id
				WHERE api_agsvb.ls_kind like 'PO IV pk curve id'
				 AND tv.ls_kind in ('time', 'PO - PK_Concentration', 'IV - PK_Concentration')
				 AND api_agsvb.string_value in (",sqliz(curveids)," )
					GROUP by tg.id, ts.id, api_agsvb.string_value
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
      renderingHint <- parametersDataFrame$ls_kind[which(parametersDataFrame$string_value==curveids)][1]
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

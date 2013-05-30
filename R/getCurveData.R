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
  
  points <- getPoints(curveids, ...)

  parameters <- query(paste("SELECT TESTED_LOT,
                            AG_ID,
                            AGV_ID,
                            VALUE_KIND,
                            VALUE_OPERATOR,
                            NUMERIC_VALUE,
                            STRING_VALUE,
                            VALUE_UNIT,
                            RECORDED_DATE
                            FROM api_analysis_group_results
                            WHERE ag_id IN
                            (SELECT ag_id
                            FROM api_analysis_group_results
                            WHERE value_kind='curve id'
                            AND string_value in (",sqliz(curveids),"))
                            "), ...)
  names(parameters) <- tolower(names(parameters))
  
  #TODO: Check if parameters/points are null
  parameters <- getRenderingHintParameters(parameters)
  
  if(is.null(parameters) && nrow(points)==0) {
    warning(paste0("No Points or Parameters found for curveids: ", curveids))
  }
  return (list(
    points = points,
    parameters = parameters
  ))
}
getPoints <- function(curveids, ...) {
  
  points <- query(paste("SELECT curveid, dose, doseunits, response, responseunits, flag, response_ss_id, s_id, tg_id, ag_id from api_dose_response where curveid in (",sqliz(curveids),")",sep=""), ...)
  names(points) <- tolower(names(points))
  
  points <- data.frame(  curveid = as.factor(points$curveid),
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
  
  if(nrow(points) > 0) {
    points$flag <- factor(points$flag, levels = c(levels(points$flag), TRUE, FALSE))
    points$flag <- !is.na(points$flag)
    points$id <- 1:nrow(points)
  }
  return(points)
}

getRenderingHintParameters <- function(parametersDataFrame) {
  longFormat <- parametersDataFrame
  row.names(longFormat) <- parametersDataFrame$agv_id
  longFormat <- longFormat[,c("ag_id","tested_lot","value_kind","numeric_value","string_value","value_unit")]
  wideFormat <- reshape(longFormat,
                        timevar="value_kind",
                        idvar=c("ag_id","tested_lot"),direction="wide")
  
  renderingHint <- wideFormat$"string_value.Rendering Hint"[[1]]
  
  parameters <- switch(renderingHint,
                       "4 parameter D-R" = getLL4ParametersFromWideFormat(wideFormat),
                       "Ki D-R" = getKiParametersFromWideFormat(wideFormat)
  )  
}
getLL4ParametersFromWideFormat <- function(wideFormat) {
  wideName = c("tested_lot", "string_value.curve id", "numeric_value.Min","numeric_value.Fitted Min",
               "numeric_value.Max", "numeric_value.Fitted Max", "numeric_value.Hill slope", "numeric_value.Fitted Hill slope", 
               "numeric_value.EC50", "numeric_value.Fitted EC50", "string_value.Operator")
  newName = c("tested_lot", "curveid", "min", "fittedmin",
              "max", "fittedmax", "hill", "fittedhillslope",
              "ec50", "fittedec50", "operator")
  valuesToGet <- data.frame(wideName = as.character(wideName), newName = as.character(newName))
  extractParametersFromWideFormat(valuesToGet, wideFormat)
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
                                               "string_value" = as.factor(wideFormat[,colName])
        ))        
      } 
    } else {
      parameters <- cbind(parameters, switch(split[1],
                                             "numeric_value" = as.numeric(rep(NA, times = nrow(parameters))),
                                             "string_value" = factor(rep(NA, times = nrow(parameters)))
      ))      
    }
    names(parameters)[ncol(parameters)] <- newName
  }
  return(parameters)
}

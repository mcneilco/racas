createNewACASExperiment <- function(metaData, protocol, lsTransaction, pathToGenericDataFormatExcelFile) {
  # creates an experiment using the metaData
  # 
  # Args:
  #   metaData:               A data.frame including "Experiment Name", "Scientist", "Notebook", "Page", and "Assay Date"
  #   protocol:               A list that is a protocol
  #   lsTransaction:          A list that is a lsTransaction tag
  #
  # Returns:
  #  A list that is an experiment
  
  experimentStates <- list()
  
  # Store the metaData in experiment values
  experimentValues <- list()
  experimentValues[[length(experimentValues)+1]] <- createStateValue(valueType = "stringValue",
                                                                     valueKind = "notebook",
                                                                     stringValue = metaData$Notebook[1])
  if (!is.null(metaData$Page)) {
    experimentValues[[length(experimentValues)+1]] <- createStateValue(valueType = "stringValue",
                                                                       valueKind = "notebook page",
                                                                       stringValue = metaData$Page[1])
  }
  experimentValues[[length(experimentValues)+1]] <- createStateValue(valueType = "dateValue",
                                                                     valueKind = "completion date",
                                                                     dateValue = as.numeric(format(as.Date(metaData$"Assay Date"[1]), "%s"))*1000)
  experimentValues[[length(experimentValues)+1]] <- createStateValue(valueType = "stringValue",
                                                                     valueKind = "status",
                                                                     stringValue = "Approved")
  experimentValues[[length(experimentValues)+1]] <- createStateValue(valueType = "stringValue",
                                                                     valueKind = "analysis status",
                                                                     stringValue = "complete")
  experimentValues[[length(experimentValues)+1]] <- createStateValue(valueType = "clobValue",
                                                                     valueKind = "analysis result html",
                                                                     stringValue = "<p>Analysis completed in Galileo</p>")
  
  if (!is.null(metaData$Project)) {
    experimentValues[[length(experimentValues)+1]] <- createStateValue(valueType = "stringValue",
                                                                       valueKind = "project",
                                                                       stringValue = metaData$Project[1])
  }
  
  # Create an experiment state for metadata
  experimentStates[[length(experimentStates)+1]] <- createExperimentState(experimentValues=experimentValues,
                                                                          lsTransaction = lsTransaction, 
                                                                          recordedBy=metaData$Scientist[1], 
                                                                          stateType="metadata", 
                                                                          stateKind="experiment metadata")
  
  # Create a label for the experiment name
  experimentLabels <- list()
  experimentLabels[[length(experimentLabels)+1]] <- createExperimentLabel(lsTransaction = lsTransaction, 
                                                                          recordedBy=metaData$Scientist[1], 
                                                                          labelType="name", 
                                                                          labelKind="experiment name",
                                                                          labelText=metaData$"Experiment Name"[1],
                                                                          preferred=TRUE)
  # Create the experiment
  experiment <- createExperiment(lsTransaction = lsTransaction, 
                                 protocol = protocol,
                                 kind = "Galileo ETL",
                                 shortDescription="experiment created by galileo etl",  
                                 recordedBy=metaData$Scientist[1], 
                                 experimentLabels=experimentLabels,
                                 experimentStates=experimentStates)
  
  # Save the experiment to the server
  experiment <- saveExperiment(experiment)
  
  return(experiment)
}
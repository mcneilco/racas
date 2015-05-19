# Solves issues with rjson printing times in scientific notation and losing precision
options(scipen=99)

#racas::applicationSettings$client.service.persistence.fullpath <- "http://localhost:8080/labseer/"
#racas::applicationSettings$client.service.persistence.fullpath <- "http://host3.labsynch.com:8080/acas/"


############  FUNCTIONS ########################


#to get system label IDs
getAutoLabelId <- function(thingTypeAndKind="thingTypeAndKind", labelTypeAndKind="labelTypeAndKind", numberOfLabels=1, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  labelSequenceDTO = list(
    thingTypeAndKind=thingTypeAndKind,
    labelTypeAndKind=labelTypeAndKind,
    numberOfLabels=numberOfLabels
  )
  cat(toJSON(labelSequenceDTO))
  response <- fromJSON(getURL(
    paste(lsServerURL, "labelsequences/getNextLabelSequences", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(labelSequenceDTO)))
  return(response)
}


#to get system labels
getAutoLabels <- function(thingTypeAndKind="thingTypeAndKind", labelTypeAndKind="labelTypeAndKind", numberOfLabels=1, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  labelSequenceDTO = list(
    thingTypeAndKind=thingTypeAndKind,
    labelTypeAndKind=labelTypeAndKind,
    numberOfLabels=numberOfLabels
  )
  url <- paste0(lsServerURL, "labelsequences/getLabels")
  response <- postURLcheckStatus(url, postfields=toJSON(labelSequenceDTO), requireJSON = TRUE)
  response <- fromJSON(response)
  return(response)
}

# getAutoLabels(thingType="document", thingKind="protocol", labelType="id", labelKind="codeName", numberOfLabels=3)
# getAutoLabelId(thingType="document", thingKind="protocol", labelType="id", labelKind="codeName", numberOfLabels=1)

#to create a new thing kind
createThingKind <- function(thingType="thingType List Object", kindName="kindName", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  thingKind = list(
    thingType=thingType,
    kindName=kindName
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "thingkinds", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(thingKind)))
  return(response)
}


#to create a new labelkind
createLabelKind <- function(labelType="labelType List Object", kindName="kindName", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  labelKind = list( 
    labelType=labelType,
    kindName=kindName
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "labelkinds", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(labelKind)))
  return(response)
}

# to create a new thingstatetype
createStateType <- function(typeName="typeName", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  stateType = list(
    typeName=typeName
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "statetypes", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(stateType)))
  return(response)
}

# to create a new thingstatekind
createStateKind <- function(stateType="stateType List Object", kindName="kindName", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  stateKind = list(
    stateType=stateType,
    kindName=kindName
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "statekinds", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(stateKind)))
  return(response)
}

# to create a new state value type
createValueType <- function(typeName="typeName", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  valueType = list(
    typeName=typeName
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "valuetypes", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(valueType)))
  return(response)
}

# to create a new state value kind
createValueKind <- function(valueType="valueType List Object", kindName="kindName", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  valueKind = list(
    valueType=valueType,
    kindName=kindName
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "valuekinds", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(valueKind)))
  return(response)
}

# to create a new interaction kind
createInteractionKind <- function(interactionType="interactionType List Object", kindName="kindName", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  interactionKind = list(
    interactionType=interactionType,
    kindName=kindName
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "interactionkinds/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(interactionKind)))
  return(response)
}
##to create a new LsTransaction
createLsTransaction <- function(comments="", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  newLsTransaction = list(
    comments=comments,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "lstransactions", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(newLsTransaction)))
  return(response)
}
#to create a list of ls transactions
createLsTransactions <- function(transactionList, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- fromJSON(getURL(
    paste(lsServerURL, "lstransactions/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(transactionList)))
  return(response)
}

generateLsTransaction <- function(comments, recordedDate = as.numeric(format(Sys.time(), "%s"))*1000) {
  lsTransaction = list(
    comments=comments,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(lsTransaction)
}

##to create a new basic thing
createThing <- function(thingType="thingType List Object", thingKind="thingKind List Object", recordedBy="author List Object", lsTransaction=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  newThing = list(
    recordedBy=recordedBy,
    thingType=thingType,
    thingKind=thingKind,
    lsTransaction=lsTransaction,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  response <- fromJSON(getURL(
    paste(lsServerURL, "lsthings", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(newThing)))
  return(response)
}

createThingLabel <- function(thing, labelText, author, lsType, lsKind, lsTransaction=NULL, preferred=TRUE, ignored=FALSE){
  thingLabel = list(
    thing=thing,
    labelText=labelText,
    recordedBy=author,
    lsType=lsType,
    lsKind=lsKind,
    preferred=preferred,
    ignored=ignored,
    lsTransaction=lsTransaction,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(thingLabel)
}

saveThingLabels <- function(thingLabels, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- fromJSON(getURL(
    paste(lsServerURL, "thinglabels/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(thingLabels)))
  return(response)
}

#' Creates a tag
#' 
#' Creates a tag
#' 
#' @param tagText the text of the tag
#' @param id used to link to old tags
#' @param version the version of the tag (only used with an id)
createTag <- function(tagText, id=NULL, version=NULL){
  lsTag = list(
    tagText = tagText,
    id = id,
    version = version,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(lsTag)
}

createProtocolLabel <- function(protocol = NULL, labelText, recordedBy="authorName", lsType="name", lsKind="protocol name", lsTransaction=NULL, preferred=TRUE, ignored=FALSE,
                                recordedDate=as.numeric(format(Sys.time(), "%s"))*1000){
  # The protocol must include at least an id and version
  protocolLabel = list(
    protocol=protocol,
    labelText=labelText,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    preferred=preferred,
    ignored=ignored,
    lsTransaction=lsTransaction,
    recordedDate=recordedDate
  )
  return(protocolLabel)
}

createExperimentLabel <- function(experiment=NULL, labelText, recordedBy="authorName", lsType="name", lsKind="experiment name", lsTransaction=NULL, preferred=TRUE, ignored=FALSE, recordedDate=as.numeric(format(Sys.time(), "%s"))*1000){
  experimentLabel = list(
    experiment=experiment,
    labelText=labelText,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    preferred=preferred,
    ignored=ignored,
    lsTransaction=lsTransaction,
    recordedDate=recordedDate
  )
  return(experimentLabel)
}

#' Create experiment with name
#' 
#' Create an experiment with a label already built in
#' 
#' @param labelText name of the experiment
#' @param protocol protocol object (list with id and version) for attached protocol
#' @param recordedBy username of the person saving
#' @param lsTransaction integer of the lsTransaction
#' @param shortDescription short description < 255 characters
#' @param lsType experiment type
#' @param lsKind experiment kind 
createNamedExperiment <- function(labelText, protocol, recordedBy, lsTransaction, shortDescription, lsType="default", lsKind="default") {
  experiment <- createExperiment(
    protocol=protocol, 
    lsType=lsType,
    lsKind=lsKind,
    shortDescription=shortDescription,
    recordedBy=recordedBy,
    lsTransaction=lsTranscation,
    experimentLabels = createExperimentLabel(
      labelText=labelText, 
      recordedBy=recordedBy,
      lsTransaction=lsTransaction))
}

createAnalysisGroupLabel <- function(analysisGroup=NULL, labelText, recordedBy="authorName", lsType="name", lsKind="analysis group name", lsTransaction=NULL, preferred=TRUE, ignored=FALSE){
  analysisGroupLabel = list(
    analysisGroup=analysisGroup,
    labelText=labelText,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    preferred=preferred,
    ignored=ignored,
    lsTransaction=lsTransaction,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(analysisGroupLabel)
}

createTreatmentGroupLabel <- function(treatmentGroup=NULL, labelText, recordedBy="authorName", lsType="name", lsKind="treatment group name", lsTransaction=NULL, preferred=TRUE, ignored=FALSE){
  treatmentGroupLabel = list(
    treatmentGroup=treatmentGroup,
    labelText=labelText,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    preferred=preferred,
    ignored=ignored,
    lsTransaction=lsTransaction,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(treatmentGroupLabel)
}

createSubjectLabel <- function(subject=NULL, labelText, recordedBy="authorName", lsType="name", lsKind="subject name", lsTransaction=NULL, preferred=TRUE, ignored=FALSE){
  subjectLabel = list(
    subject=subject,
    labelText=labelText,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    preferred=preferred,
    ignored=ignored,
    lsTransaction=lsTransaction,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(subjectLabel)
}

createInteraction <- function(firstThing, secondThing, recordedBy, interactionType, interactionKind,
                              ignored=FALSE, lsTransaction=NULL){
  interaction = list(
    firstThing=firstThing,
    secondThing=secondThing,
    recordedBy=recordedBy,
    interactionType=interactionType,
    interactionKind=interactionKind,
    ignored=ignored,
    lsTransaction=lsTransaction,
    thingType="interaction",
    thingKind="interaction",
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(interaction)
}
createContainerContainerInteractionState <- function(itxContainerContainer=NULL, lsValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL){
  containerContainerInteractionState = list(
    itxContainerContainer=itxContainerContainer,
    lsValues=lsValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(containerContainerInteractionState)
}

createSubjectContainerInteractionState <- function(itxSubjectContainer=NULL, lsValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL){
  containerSubjectInteractionState = list(
    itxSubjectContainer=itxSubjectContainer,
    lsValues=lsValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(containerSubjectInteractionState)
}

saveLsInteractions <- function(lsInteractions, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- fromJSON(getURL(
    paste(lsServerURL, "interactions/lsinteraction/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(lsInteractions)))
  return(response)
}


createLsState <- function(lsValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL){
  LsState = list(
    lsValues=lsValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(LsState)
}
createProtocolState <- function(protocol=NULL, protocolValues=NULL, recordedBy="userName", lsType="lsType", 
                                lsKind="lsKind", comments="", lsTransaction=NULL, recordedDate=as.numeric(format(Sys.time(), "%s"))*1000){
  protocolState = list(
    protocol=protocol,
    lsValues=protocolValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=recordedDate
  )
  return(protocolState)
}
createExperimentState <- function(experimentValues=list(), recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL, experiment=NULL, testMode=FALSE, recordedDate=NULL){
  experimentState = list(
    experiment=experiment, #This will fail if not given an id and version (but the version does not matter)
    lsValues=experimentValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=if(testMode) 1376954591000 else if(!is.null(recordedDate)) recordedDate else as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(experimentState)
}

createAnalysisGroupState <- function(analysisGroup = NULL, analysisGroupValues=list(), recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL, testMode=FALSE){
  analysisGroupState = list(
    analysisGroup=analysisGroup, #This will fail if not given an id and version (but the version does not matter)
    lsValues=analysisGroupValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=if(testMode) 1376954591000 else as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(analysisGroupState)
}

createTreatmentGroupState <- function(treatmentGroup=NULL, treatmentGroupValues=list(), recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL){
  treatmentGroupState = list(
    treatmentGroup=treatmentGroup,
    lsValues=treatmentGroupValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(treatmentGroupState)
}
createTreatmentGroup <- function(analysisGroup=NULL,subjects=NULL,treatmentGroupStates=NULL, lsType="default", lsKind="default", codeName=NULL, recordedBy="userName", lsTransaction=NULL){
  
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="document_treatment group", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
  }
  
	treatmentGroup= list(
    analysisGroups=list(analysisGroup),
    lsType=lsType, 
    lsKind=lsKind, 
    codeName=codeName,		
    subjects=subjects,
    lsStates=treatmentGroupStates,
    recordedBy=recordedBy,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(treatmentGroup)
}
createSubject <- function(treatmentGroup=NULL, subjectStates=NULL, lsType="default", lsKind="default", codeName=NULL, recordedBy="userName", lsTransaction=NULL){

	if (is.null(codeName) ) {
		codeName <- getAutoLabels(thingTypeAndKind="document_subject", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
	}
	
	subject= list(
    treatmentGroups=list(treatmentGroup),
    lsType=lsType,
    lsKind=lsKind,
    codeName=codeName,
    lsStates=subjectStates,
    recordedBy=recordedBy,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(subject)
}

createSubjectState <- function(subject=NULL, subjectValues=list(), recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL){
  sampleState = list(
    subject=subject,
    lsValues=subjectValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(sampleState)
}


#'Creates a state value
#'
#'Creates a state value, can include an lsState or be nested inside one.
#'
#'@param testMode used for testing
#'@param lsType type of the value
#'@param lsKind lsKind of the value
#'@param stringValue <255 character
#'@param fileValue file code or path
#'@param urlValue url
#'@param publicData TRUE to be visible
#'@param ignored TRUE to mark as old
#'@param dateValue date in milliseconds
#'@param clobValue clob
#'@param blobValue blob
#'@param concentration numeric
#'@param concUnit character
#'@param valueOperator ">" or "<"
#'@param operatorType "comparison", not yet implemented
#'@param numericValue numeric
#'@param sigFigs integer
#'@param uncertainty numeric
#'@param uncertaintyType "standard deviation"
#'@param numberOfReplicates integer
#'@param valueUnit "uM", etc.
#'@param unitType not yet implemented
#'@param comments used by fileValue for a filename, flags for comments, etc.
#'@param lsTransaction id of the transaction
#'@param codeValue codename of something
#'@param lsState a state object
#'@param testMode used for testing
#'@param recordedBy the current username
#'@param lsServerURL the url for the roo server
#'  
#'@details Use either in a nested object or alone
#'  
#'@return list, a value object
#'@export
createStateValue <- function(lsType="lsType", lsKind="lsKind", stringValue=NULL, fileValue=NULL,
                             urlValue=NULL, publicData=TRUE, ignored=FALSE,
                             dateValue=NULL, clobValue=NULL, blobValue=NULL, concentration=NULL,
                             concUnit=NULL, valueOperator=NULL, operatorType=NULL, numericValue=NULL,
                             sigFigs=NULL, uncertainty=NULL, uncertaintyType=NULL,
                             numberOfReplicates=NULL, valueUnit=NULL, unitType=NULL, comments=NULL, 
                             lsTransaction=NULL, codeValue=NULL, recordedBy="username",
                             lsState=NULL, testMode=FALSE, recordedDate=as.numeric(format(Sys.time(), "%s"))*1000,
                             codeType = NULL, codeKind = NULL, codeOrigin = NULL){
  #TODO: use unitType and operatorType
  stateValue = list(
    lsState=lsState,
    lsType=lsType,
    lsKind=lsKind,
    stringValue=stringValue,
    fileValue=fileValue,
    urlValue=urlValue,
    dateValue=dateValue,
    clobValue=clobValue,
    blobValue=blobValue,
    concentration=concentration,
    concUnit=concUnit,
    operatorKind=valueOperator,
    operatorType=if(is.null(valueOperator)) NULL else "comparison",
    numericValue=numericValue,
    sigFigs=sigFigs,
    uncertainty=uncertainty,
    uncertaintyType=uncertaintyType,
    numberOfReplicates=numberOfReplicates,
    unitKind=valueUnit,
    comments=comments,
    ignored=ignored,
    publicData=publicData,
    codeValue=codeValue,
    codeOrigin=codeOrigin,
    codeType=codeType,
    codeKind=codeKind,
    recordedBy=recordedBy,
    recordedDate=if(testMode) 1376954591000 else recordedDate,
    lsTransaction=lsTransaction		
  )
  return(stateValue)
}


createProtocol <- function(codeName=NULL, lsType="default", lsKind="default", shortDescription="protocol short description", lsTransaction=NULL, 
                           recordedBy="userName", protocolLabels=NULL, protocolStates=NULL, recordedDate=as.numeric(format(Sys.time(), "%s"))*1000,
                           modifiedBy=NULL, modifiedDate=NULL){
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="document_protocol", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
  }
  protocol <- list(
    codeName=codeName,
    lsType=lsType,
    lsKind=lsKind,
    shortDescription=shortDescription,
    lsTransaction=lsTransaction,
    recordedBy=recordedBy,
    recordedDate=recordedDate,
    modifiedBy=modifiedBy,
    modifiedDate=modifiedDate,
    lsLabels=protocolLabels,
    lsStates=protocolStates
  )
  return(protocol)	
}			


createExperiment <- function(protocol=NULL, codeName=NULL, lsType="default", lsKind="default", shortDescription="Experiment Short Description text limit 255", 
                             lsTransaction=NULL, recordedBy="userName", experimentLabels=list(), experimentStates=list(), lsTags=list(), recordedDate = as.numeric(format(Sys.time(), "%s"))*1000, modifiedBy = recordedBy, modifiedDate = as.numeric(format(Sys.time(), "%s"))*1000){
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="document_experiment", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
  }
  experiment <- list(
    protocol=protocol,
    codeName=codeName,
    lsType=lsType,
    lsKind=lsKind,
    shortDescription=shortDescription,
    lsTransaction=lsTransaction,
    recordedBy=recordedBy,
    recordedDate=recordedDate,
    modifiedBy=modifiedBy,
    modifiedDate=modifiedDate,
    lsLabels=experimentLabels,
    lsStates=experimentStates,
    lsTags=lsTags
  )
  
  return(experiment)	
}			


createAnalysisGroup <- function(experiment=NULL, codeName=NULL, lsType="default", lsKind="default", lsTransaction=NULL, recordedBy="userName",
                                treatmentGroups=NULL, analysisGroupStates=list(), testMode=FALSE){
  if (is.null(codeName) ) {
    if(testMode) {
      codeName <- "AG-TEST"
    } else {
      codeName <- getAutoLabels(thingTypeAndKind="document_analysis group", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]
    }
  }
  analysisGroup <- list(
    codeName=codeName,
    lsType=lsType,
    lsKind=lsKind,
		experiments=list(experiment),
		recordedBy=recordedBy,
		lsTransaction=lsTransaction,
		treatmentGroups=treatmentGroups,
		lsStates=analysisGroupStates,
		recordedDate=if(testMode) 1376954591000 else as.numeric(format(Sys.time(), "%s"))*1000
	)

	return(analysisGroup)	
}			


createContainer <- function(codeName=NULL, ignored = FALSE, lsType="material", lsKind="well", lsTransaction=NULL, recordedBy="userName",
                            containerStates=NULL, containerLabels=NULL){
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="material_container", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
  }
  sysDateTime <- as.numeric(format(Sys.time(), "%s"))*1000
  container <- list(
    codeName=codeName,
    ignored=ignored,
    lsType=lsType,
    lsKind=lsKind,
    recordedBy=recordedBy,
    lsTransaction=lsTransaction,
    lsStates=containerStates,
    lsLabels=containerLabels,
    recordedDate=sysDateTime,
    modifiedBy=recordedBy,
    modifiedDate=sysDateTime
  )
  
  return(container)	
}		

createLabelSequence <- function(labelPrefix = "PREF", labelSeparator="-", groupDigits = FALSE, digits=8, latestNumber = 1,
                                ignored=FALSE, modifiedDate = as.numeric(format(Sys.time(), "%s"))*1000, thingTypeAndKind,
                                labelTypeAndKind = "id_codeName") {
  labelSequence <- list(
    labelPrefix=labelPrefix,
    labelSeparator=labelSeparator,
    groupDigits=groupDigits,
    digits=digits,
    latestNumber=latestNumber,
    ignored=ignored,
    modifiedDate=modifiedDate,
    thingTypeAndKind=thingTypeAndKind,
    labelTypeAndKind=labelTypeAndKind)
}

createContainerState <- function(container=NULL,containerValues=list(), recordedBy="userName", lsType="lsType", lsKind="lsKind", 
                                 comments="", lsTransaction=NULL){
  containerState = list(
    container=container,
    lsValues=containerValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(containerState)
}

createContainerLabel <- function(container=NULL,labelText, recordedBy="userName", lsType="lsType", lsKind="lsKind", 
                                 lsTransaction=NULL, preferred=TRUE, imageFile=NULL, physicallyLabeled=FALSE,
                                 modifiedDate=NULL,version=NULL){
  containerLabel = list(
    container=container,
    recordedBy=recordedBy,
    labelText=labelText,
    lsType=lsType,
    lsKind=lsKind,
    lsTransaction=lsTransaction,
    preferred=preferred,
    imageFile=imageFile,
    physicallyLabled=physicallyLabeled, # Roo has it spelled wrong, so we have to match that
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000,
    modifiedDate=modifiedDate,
    version=version
  )
  return(containerLabel)
}


createContainerContainerInteraction <- function(codeName=NULL, ignored = FALSE, lsTransaction=NULL, recordedBy="userName",
                                                interactionStates=NULL, lsType, lsKind="interaction", 
                                                firstContainer, secondContainer){
  #interactionType = c("added to","removed from","operated on", "created by", "destroyed by", "refers to", "member of")
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="interaction_containerContainer", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]  					
  }
  sysDateTime <- as.numeric(format(Sys.time(), "%s"))*1000
  containerContainerInteraction <- list(
    codeName=codeName,
    ignored=ignored,
    recordedBy=recordedBy,
    lsTransaction=lsTransaction,
    interactionStates=interactionStates,
    recordedDate=sysDateTime,
    modifiedBy=recordedBy,
    modifiedDate=sysDateTime,
    lsType=lsType,
    lsKind=lsKind,
    firstContainer=firstContainer,
    secondContainer=secondContainer
  )
  
  return(containerContainerInteraction)	
}			

#' create subject container interaction 
#' @details interactionStates is ignored for now,
#' could add back later (as lsStates) with roo update.
#' 
#' This is super sensitive to subject and container being nested- only known to
#' work with containers and subjects that only have an id and version.
createSubjectContainerInteraction <- function(subject, container, lsType, lsKind="interaction", codeName=NULL, ignored = FALSE,
                                              lsTransaction=NULL, recordedBy="userName", interactionStates=NULL){
  #lsType = c("added to","removed from","operated on", "created by", "destroyed by", "refers to", "member of")
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="interaction_subjectContainer", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]    				
  }
  sysDateTime <- as.numeric(format(Sys.time(), "%s"))*1000
  subjectContainerInteraction <- list(
    codeName=codeName,
    ignored=ignored,
    recordedBy=recordedBy,
    lsTransaction=lsTransaction,
    recordedDate=sysDateTime,
    modifiedBy=recordedBy,
    modifiedDate=sysDateTime,
    lsType=lsType,
    lsKind=lsKind,
    subject=subject,
    container=container
  )
  
  return(subjectContainerInteraction)	
}			

createSubjectContainerItxState <- function(subjectContainerInteraction=NULL, interactionValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", 
                                           comments="", lsTransaction=NULL){
  interactionState = list(
    subjectContainerInteraction=subjectContainerInteraction,
    lsValues=interactionValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(interactionState)
}

saveProtocols <- function(protocols, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(protocols, "protocols"))
}

saveProtocol <- function(protocol, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(protocol, "protocols"))
}



saveExperiment <- function(experiment, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(experiment, "experiments"))
}


saveExperiments <- function(experiments, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(experiments, "experiments"))
}

saveAnalysisGroups <- function(analysisGroups, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(analysisGroups, "analysisgroups"))
}

saveAnalysisGroup <- function(analysisGroup, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(analysisGroup, "analysisgroups"))
}

#' save container objects
#' Currently, this cannot accept labels and states
saveContainer <- function(container, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(container, "containers"))
}

saveContainers <- function(containers, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(containers, "containers"))
}

saveContainerLabel <- function(containerLabel, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  return(saveAcasEntity(containerLabel, "containerlabels"))
}

saveContainerLabels <- function(containerLabels, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  return(saveAcasEntities(containerLabels, "containerlabels"))
}

saveContainerState <- function(containerState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  return(saveAcasEntity(containerState, "containerstates"))
}

saveContainerStates <- function(containerStates, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  return(saveAcasEntities(containerStates, "containerstates"))
}

saveContainerContainerInteraction <- function(containerContainerInteraction, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(containerContainerInteraction, "itxcontainercontainers"))
}

saveContainerContainerInteractions <- function(containerContainerInteractions, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(containerContainerInteractions, "itxcontainercontainers"))
}

saveSubjectContainerInteraction <- function(subjectContainerInteraction, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(subjectContainerInteraction, "itxsubjectcontainers"))
}

saveSubjectContainerInteractions <- function(subjectContainerInteractions, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(subjectContainerInteractions, "itxsubjectcontainers"))
}

saveProtocolLabel <- function(protocolLabel, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  return(saveAcasEntity(protocolLabel, "protocollabels"))
}

#' @rdname saveAcasEntities
saveAcasEntity <- function(entity, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  # If you have trouble, make sure the acasCategory is all lowercase, has no spaces, and is plural
  message <- toJSON(entity)
  url <- paste0(lsServerURL, acasCategory, "/")
  response <- postURLcheckStatus(url, message, requireJSON = TRUE)
  response <- fromJSON(response)
  return(response)
}

#' Save ACAS entities to the server
#' 
#' Save protocols, labels, experiments, etc.
#' 
#' @param entity a single entity (a named list, becomes a JSON object)
#' @param entities a list of entities
#' @param acasCategory e.g. "experiments", "subjectlabels", etc.
#' @param lsServerURL url of ACAS server
#' @return a list, sometimes empty
#' @details \code{updateAcasEntities} replaces the entity that is at the URL
#'   with the one sent. Sub-entities (label, state, value) must have a parent
#'   object. \code{deleteAcasEntities} is not implemented for states and values.
#' @export
saveAcasEntities <- function(entities, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  if (length(entities) > 1000) {
    output <- saveAcasEntitiesInternal(entities[1:1000], acasCategory, lsServerURL)
    otherSaves <- saveAcasEntities(entities[1001:length(entities)], acasCategory, lsServerURL)
    return(c(output, otherSaves))
  } else {
    return(saveAcasEntitiesInternal(entities, acasCategory, lsServerURL))
  }
}

saveAcasEntitiesInternal <- function(entities, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  # If you have trouble, make sure the acasCategory is all lowercase, has no spaces, and is plural
  
  message <- toJSON(entities)
  url <- paste0(lsServerURL, acasCategory, "/jsonArray")
  response <- postURLcheckStatus(url, postfields=message, requireJSON = TRUE)
  
  if (grepl("^\\s*$", response)) {
    return("")
  }
  
  response <- fromJSON(response)
  return(response)
}

saveAnalysisGroupState <- function(analysisGroupState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(analysisGroupState, "analysisgroupstates"))
}

saveAnalysisGroupStates <- function(analysisGroupStates, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(analysisGroupStates, "analysisgroupstates"))
}

saveExperimentState <- function(experimentState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(experimentState, "experimentstates"))
}

saveExperimentStates <- function(experimentStates, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(experimentStates, "experimentstates"))
}

saveExperimentValue <- function(experimentValue, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntity(experimentValue, "experimentvalues"))
}

saveExperimentValues <- function(experimentValues, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  return(saveAcasEntities(experimentValues, "experimentvalues"))
}

saveLabelSequence <- function(labelSequence, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- saveAcasEntity("labelsequences")
  return(response)
}


compactList <- function(inputList) Filter(Negate(is.null), inputList) ## remove null elements from a list

returnListItem <- function(outputList){
  ## input: list object
  ## output: single list object if there is a single list element
  ## 			return error if 0 or > 1 elements found in the list
  ## note: null list elements are removed
  parsedList <- compactList(outputList)
  if (length(parsedList) == 0){
    return("Error: No results found")
  } else if (length(parsedList) > 1){
    return("Error: Multiple results found")		
  } else {
    return(compactList(parsedList)[[1]])
  }
}

# getThingKind <- function( thingType="typeName", thingKind="kindName" ){
# 	getThingKindFromList <- function(inputList, thingType=thingType, thingKind=thingKind){
# 		if(inputList$thingType$typeName == thingType && inputList$kindName == thingKind){
# 			return(inputList)
# 		}	
# 	}			
# 	outputList <- lapply(thingKinds.list, getThingKindFromList, thingType=thingType, thingKind=thingKind)
# 	return (returnListItem(outputList))	
# }
# 
# getThingKindByKindName <- function( thingKind="kindName" ){
# 	getThingKindFromList <- function(inputList, thingType=typeName, thingKind=thingKind){
# 		if( inputList$kindName == thingKind){
# 			return(inputList)
# 		}	
# 	}			
# 	outputList <- lapply(thingKinds.list, getThingKindFromList, thingKind=thingKind)
# 	return (returnListItem(outputList))	
# }
# 
# getThingType <- function( typeName="typeName" ){
# 	getThingTypeFromList <- function(inputList, typeName=""){
# 		if(inputList$typeName == typeName){
# 			return(inputList)
# 		}	
# 	}	
# 	outputList <- lapply(thingTypes.list, getThingTypeFromList, typeName=typeName)
# 	return (returnListItem(outputList))
# }
# 
# getLabelType <- function( typeName="typeName" ){
# 	getTypeFromList <- function(inputList, typeName=""){
# 		if(inputList$typeName == typeName){
# 			return(inputList)
# 		}	
# 	}	
# 	outputList <- lapply(labelTypes.list, getTypeFromList, typeName=typeName)
# 	return (returnListItem(outputList))
# }
# 
# getLabelKind <- function( labelType="typeName", labelKind="kindName" ){
# 	getLabelKindFromList <- function(inputList, labelType="", labelKind=""){
# 		if(inputList$labelType$typeName == labelType && inputList$kindName == labelKind){
# 			return(inputList)
# 		}	
# 	}			
# 	outputList <- lapply(labelKinds.list, getLabelKindFromList, labelType=labelType, labelKind=labelKind)
# 	return (returnListItem(outputList))	
# }
# 
# getLabelKindByKindName <- function( labelKind="kindName" ){
# 	getLabelKindFromList <- function(inputList, labelType="", labelKind=""){
# 		if(inputList$kindName == labelKind){
# 			return(inputList)
# 		}	
# 	}			
# 	outputList <- lapply(labelKinds.list, getLabelKindFromList, labelKind=labelKind)
# 	return (returnListItem(outputList))	
# }
#
# getInteractionTypes <- function( typeName="typeName" ){
# 	getTypeFromList <- function(inputList, typeName=""){
# 		if(inputList$typeName == typeName){
# 			return(inputList)
# 		}	
# 	}	
# 	outputList <- lapply(interactionTypes.list, getTypeFromList, typeName=typeName)
# 	return (returnListItem(outputList))
# }
# 
# getInteractionTypeByVerb <- function( typeVerb="typeVerb" ){
# 	getTypeFromList <- function(inputList, typeVerb=""){
# 		if(inputList$typeVerb == typeVerb){
# 			return(inputList)
# 		}	
# 	}	
# 	outputList <- lapply(interactionTypes.list, getTypeFromList, typeVerb=typeVerb)
# 	return (returnListItem(outputList))
# }
# 
# getInteractionKind <- function( typeName="typeName", kindName="kindName" ){
# 	getInteractionKindFromList <- function(inputList, typeName="", kindName=""){
# 		if(inputList$interactionType$typeName == typeName && inputList$kindName == kindName){
# 			return(inputList)
# 		}	
# 	}			
# 	outputList <- lapply(interactionKinds.list, getInteractionKindFromList, typeName=typeName, kindName=kindName)
# 	return (returnListItem(outputList))	
# }
# 
# getInteractionKindByVerb <- function( typeVerb="typeVerb", kindName="kindName" ){
# 	getInteractionKindFromList <- function(inputList, typeVerb="", kindName=""){
# 		if(inputList$interactionType$typeVerb == typeVerb && inputList$kindName == kindName){
# 			return(inputList)
# 		}	
# 	}			
# 	outputList <- lapply(interactionKinds.list, getInteractionKindFromList, typeVerb=typeVerb, kindName=kindName)
# 	return (returnListItem(outputList))	
# }
# 
# getStateType <- function( stateType="typeName" ){
# 	getTypeFromList <- function(inputList, stateType=""){
# 		if(inputList$typeName == stateType){
# 			return(inputList)
# 		}	
# 	}	
# 	outputList <- lapply(stateTypes.list, getTypeFromList, stateType=stateType)
# 	return (returnListItem(outputList))
# }
# 
# getStateKind <- function( stateType="typeName", stateKind="kindName" ){
# 	getStateKindFromList <- function(inputList, stateType="", stateKind=""){
# 		if(inputList$stateType$typeName == stateType && inputList$kindName == stateKind){
# 			return(inputList)
# 		}	
# 	}			
# 	outputList <- lapply(stateKinds.list, getStateKindFromList, stateType=stateType, stateKind=stateKind)
# 	return (returnListItem(outputList))	
# }
# 
# getAuthorByUserName <- function( userName="userName" ){
# 	getUserNameFromList <- function(inputList, userName=""){
# 		if(inputList$userName == userName){
# 			return(inputList)
# 		}	
# 	}	
# 	outputList <- lapply(authors.list, getUserNameFromList, userName=userName)
# 	return (returnListItem(outputList))
# }
# 
# getAuthorById <- function( userId="userId" ){
# 	getTypeFromList <- function(inputList, userId=""){
# 		if(inputList$id == userId){
# 			return(inputList)
# 		}	
# 	}	
# 	outputList <- lapply(authors.list, getTypeFromList, userId=userId)
# 	return (returnListItem(outputList))
# }

deleteExperiment <- function(experiment, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- deleteAcasEntity(experiment, "experiments")
  return(response)
}

deleteExperimentValue <- function(experimentValue, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- deleteAcasEntity(experimentValue, "experimentvalues")
  return(response)
}

#' Delete analysis groups by experiment
#' 
#' Deletes all analysis groups within an experiment
#' 
#' @param experiment a list that has an element id for the experiment
#' @param lsServerURL the URL of the persistence server
#'   
#' @return empty string
#' @details Deletes all of the analysis groups, even if they are linked to other
#'   experiments. This is intended for fully clearing uploaded data. Does not
#'   mark treatment groups and subjects as deleted, retrieval functions should
#'   respect parent deletion.
#' @export
#' 
deleteAnalysisGroupsByExperiment <- function(experiment, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- deleteURLcheckStatus(
    paste0(lsServerURL, "experiments/",experiment$id, "/deleteChildren"),
    requireJSON=TRUE)
  return(response)
}

deleteAnalysisGroupState <- function(analysisGroupState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- deleteAcasEntity(analysisGroupState, "analysisgroupstates")
  return(response)
}

#' @rdname saveAcasEntities
deleteAcasEntity <- function(entity, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  url <- paste0(lsServerURL, acasCategory, "/", entity$id)
  response <- deleteURLcheckStatus(url, requireJSON = TRUE)
  if(response!="") {
    stopUser (paste0("The loader was unable to delete the ", acasCategory, ". Instead, it got this response: ", response))
  }
  return(response)
}

#' Turns 'true' and 'false' into TRUE and FALSE
#' 
#' @param JSONBoolean a string of "true" or "false"
#' 
#' Other inputs not affected
interpretJSONBoolean <- function(JSONBoolean) {
  if (is.null(JSONBoolean)) {
    return(NULL)
  } else if (JSONBoolean=="true") {
    return(TRUE)
  } else if (JSONBoolean=="false") {
    return(FALSE)
  } else {
    return(JSONBoolean)
  }
}

#' Get Containers by label text
#' 
#' Allows searching for containers by their label, multiple labels are supported but order is not maintained
#' 
#'@param searchText a character vector of labelText(s) to find
#'@param ignored not yet implemented, now gets non-ignored labels
#'@param lsServerURL the url to the server
#'@return full container objects (nested list of lists)
getContainerByLabelText <- function(searchText, ignored=F, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  searchText <- unique(searchText)
  labelList <- lapply(searchText, function(x) {list(labelText=x)})
  url <- paste0(lsServerURL, "containers/findByLabels/jsonArray")
  postfields <- toJSON(labelList)
  response <- postURLcheckStatus(url, postfields = postfields, requireJSON = TRUE)
  tryCatch({
    response <- fromJSON(response)
  }, error = function(e) {
    logName <- "com.acas.racas.getContainerByLabelText"
    logFileName <- "racas.log"
    stopUserAndLogInvalidJSON(logName, longFileName, url, 'GET', postfields)
  })
  return(response)
}

#' Get URL and check status
#'
#'This is a wrapper for getURL that throws an error when the HTTP status is 400 
#'or greater, or possibly when the response is html.
#'
#'@param url the url to get/post
#'@param postfields data sent to the server
#'@param requireJSON boolean if errors should be thrown on JSON
#'@param ... optional parameters passed to getURL
#'  
#'@details Checks the HTTP status and logs to racas.log as com.acas.sel if 400 
#'  or greater. Setting requireJSON to \code{TRUE} will add a check for if the 
#'  response is HTML (but not necessarily valid JSON). POST, PUT, and DELETE can
#'  be done with their own functions. In POST and PUT, the \code{postfields}
#'  will also be logged. Within \code{racas}, \code{postfields} is usually JSON.
getURLcheckStatus <- function(url, ..., requireJSON=FALSE) {
  logName <- "com.acas.racas.getURLcheckStatus"
  logFileName <- "racas.log"
  h <- basicTextGatherer()
  response <- getURL(url=url, ..., headerfunction = h$update)
  responseHeader <- as.list(parseHTTPHeader(h$value()))
  statusCode <- as.numeric(responseHeader$status)
  if (statusCode >= 400) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0("Request to ", url, " with method 'GET' failed with status '",
                           statusCode, " ", responseHeader$statusMessage, "' returning: \n", 
                           response, "\nHeader was \n", h$value())
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  } else if (requireJSON==TRUE & grepl("^<",response)) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0(
      "Request to ", url, " with method 'GET' responded with HTML. Response header was: \n", 
      h$value(), "\nBody was: \n", response)
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  }
  return(response)
}

#' @rdname getURLcheckStatus
postURLcheckStatus <- function(url, postfields, ..., requireJSON=FALSE) {
  logName <- "com.acas.racas.postURLcheckStatus"
  logFileName <- "racas.log"
  h <- basicTextGatherer()
  response <- getURL(url=url, ..., postfields=postfields, customrequest='POST', 
                     httpheader=c('Content-Type'='application/json'), headerfunction = h$update)
  responseHeader <- as.list(parseHTTPHeader(h$value()))
  statusCode <- as.numeric(responseHeader$status)
  if (statusCode >= 400) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0("Request to ", url, " with method 'POST' failed with status '",
                           statusCode, " ", responseHeader$statusMessage, "' when sent the following: \n", 
                           postfields, "\nResponse header was: \n", h$value(), "\nBody was: \n", response)
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  } else if (requireJSON==TRUE & grepl("^<",response)) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0(
      "Request to ", url, " with method 'POST' responded with HTML when sent the following: \n", 
      postfields, "\nResponse header was: \n", h$value(), "\nBody was: \n", response)
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  }
  return(response)
}

#' @rdname getURLcheckStatus
putURLcheckStatus <- function(url, postfields, ..., requireJSON=FALSE) {
  logName <- "com.acas.racas.putURLcheckStatus"
  logFileName <- "racas.log"
  h <- basicTextGatherer()
  response <- getURL(url=url, ..., postfields=postfields, customrequest='PUT', 
                     httpheader=c('Content-Type'='application/json'), headerfunction = h$update)
  responseHeader <- as.list(parseHTTPHeader(h$value()))
  statusCode <- as.numeric(responseHeader$status)
  if (statusCode >= 400) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0("Request to ", url, " with method 'PUT' failed with status '",
                           statusCode, " ", responseHeader$statusMessage, "' when sent the following: \n", 
                           postfields, "\nResponse header was: \n", h$value(), "\nBody was: \n", response)
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  } else if (requireJSON==TRUE & grepl("^<",response)) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0(
      "Request to ", url, " with method 'PUT' responded with HTML when sent the following: \n", 
      postfields, "\nResponse header was: \n", h$value(), "\nBody was: \n", response)
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  }
  return(response)
}

#' @rdname getURLcheckStatus
deleteURLcheckStatus <- function(url, ..., requireJSON=FALSE) {
  logName <- "com.acas.racas.deleteURLcheckStatus"
  logFileName <- "racas.log"
  h <- basicTextGatherer()
  response <- getURL(url=url, customrequest='DELETE', ..., headerfunction = h$update)
  responseHeader <- as.list(parseHTTPHeader(h$value()))
  statusCode <- as.numeric(responseHeader$status)
  if (statusCode >= 400) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0("Request to ", url, " with method 'DELETE' failed with status '",
                           statusCode, " ", responseHeader$statusMessage, "' returning: \n", 
                           response, "\nHeader was \n", h$value())
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  } else if (requireJSON==TRUE & grepl("^<",response)) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0(
      "Request to ", url, " with method 'DELETE' responded with HTML. Response header was: \n", 
      h$value(), "\nBody was: \n", response)
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  }
  return(response)
}

#' Protocol search by name
#' 
#' Gets protocols by name
#' 
#' @param protocolName a string, the name of the protocol
#' @param lsServerURL url for roo server
#' 
#' @return a list of protocols
#' 
#' @details returns a list as uniqueness is not always enforced
#' @export
getProtocolsByName <- function(protocolName, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) { 
  url <- paste0(lsServerURL, 
                "protocols?FindByProtocolName&protocolName=", 
                URLencode(protocolName, reserved = TRUE))
  protocols <- getURLcheckStatus(url)
  tryCatch({
    protocols <- fromJSON(protocols)
  }, error = function(e) {
    logName <- "com.acas.racas.getProtocolsByName"
    logFileName <- "racas.log"
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0("Request to ", url, " received invalid JSON: \n", 
                           response, "\nHeader was \n", h$value())
    myLogger$error(errorMessage)
    stopUserWithTime(logFileName)
  })
  return(protocols)
}
#' Experiment search by name
#' 
#' Gets experiments by name
#' 
#' @param experimentName a string, the name of the experiment
#' @param lsServerURL url for roo server
#' 
#' @return a list of experiments
#' 
#' @details returns a list as uniqueness is not always enforced
#' @export
getExperimentsByName <- function(experimentName, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) { 
  url <- paste0(lsServerURL, 
                "experiments?FindByExperimentName&experimentName=", 
                URLencode(experimentName, reserved = TRUE))
  experiments <- getURLcheckStatus(url, requireJSON = TRUE)
  tryCatch({
    experiments <- fromJSON(experiments)
  }, error = function(e) {
    stopUserAndLogInvalidJSON(logName, logFileName, url, response)
  })
  return(experiments)
}
#' Check valueKinds
#' 
#' Checks that entered valueKinds are valid valueKinds
#' 
#' @param neededValueKinds character vector of valueKinds
#' @param neededValueKindTypes character vector of valueTypes, with order matching neededValueKinds
#' 
#' @return a list of two vectors and a data.frame: new valueKinds, old valueKinds, and a data.frame with corrected valueType for valueKinds
#' @export
checkValueKinds <- function(neededValueKinds, neededValueKindTypes) {
  currentValueKindsList <- getAllValueKinds()
  if (length(currentValueKindsList)==0) stopUser ("Setup error: valueKinds are missing")
  currentValueKinds <- sapply(currentValueKindsList, getElement, "kindName")
  matchingValueTypes <- sapply(currentValueKindsList, function(x) x$lsType$typeName)
  
  newValueKinds <- setdiff(neededValueKinds, currentValueKinds)
  oldValueKinds <- intersect(neededValueKinds, currentValueKinds)
  
  # Check that the value kinds that have been entered before have the correct Datatype (valueType)
  oldValueKindTypes <- neededValueKindTypes[match(oldValueKinds, neededValueKinds)]
  currentValueKindTypeFrame <- data.frame(currentValueKinds,  matchingValueTypes, stringsAsFactors=FALSE)
  oldValueKindTypeFrame <- data.frame(oldValueKinds, oldValueKindTypes, stringsAsFactors=FALSE)
  
  comparisonFrame <- merge(oldValueKindTypeFrame, currentValueKindTypeFrame, by.x = "oldValueKinds", by.y = "currentValueKinds")
  wrongValueTypes <- comparisonFrame$oldValueKindTypes != comparisonFrame$matchingValueTypes
  
  wrongTypeKindFrame <- comparisonFrame[wrongValueTypes, ]
  names(wrongTypeKindFrame)[names(wrongTypeKindFrame) == "matchingValueTypes"] <- "enteredValueTypes"
  
  goodValueKinds <- comparisonFrame$oldValueKinds[!wrongValueTypes]
  return(list(newValueKinds=newValueKinds, goodValueKinds=goodValueKinds, wrongTypeKindFrame=wrongTypeKindFrame))
}

#' valueKinds
#'
#' Gets a list of all valueKinds available.
#'
#' @param lsServerURL url for roo server
getAllValueKinds <- function(lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  valueKindsList <- getURLcheckStatus(paste0(lsServerURL, "valuekinds/"), requireJSON = TRUE)
  return(fromJSON(valueKindsList))
}
#' valueTypes
#'
#' Gets a list of all valueTypes available.
#'
#' @param lsServerURL url for roo server
getAllValueTypes <- function(lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  valueTypesList <- getURLcheckStatus(paste0(lsServerURL, "valuetypes/"), requireJSON = TRUE)
  return(fromJSON(valueTypesList))
}
#' Saves value kinds
#' 
#' Saves value kinds with matching value types
#' @param valueKinds character vector of new valueKinds
#' @param valueTypes character vector of valueTypes (e.g. \code{c("stringValue",
#'   "numericValue")})
#' @param errorEnv Error environment
#' @param lsServerURL url for roo server
#' @details valueKinds must be new, and valueTypes must exist. Removes the need
#'   to pass in full valueType objects.
#' @export
saveValueKinds <- function(valueKinds, valueTypes, errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  valueTypesList <- getAllValueTypes(lsServerURL=lsServerURL)
  allowedValueTypes <- sapply(valueTypesList, getElement, "typeName")
  
  newValueTypesList <- valueTypesList[match(valueTypes, allowedValueTypes)]
  newValueKindsUpload <- mapply(function(x, y) list(kindName=x, lsType=y), valueKinds, newValueTypesList,
                                SIMPLIFY = F, USE.NAMES = F)
  saveAcasEntities(newValueKindsUpload, "valuekinds")
}

#' Flattens Nested ACAS Entities
#' 
#' Gets values within nested ACAS entities
#' 
#' @param entity an ACAS entity such as a protocol or subject
#' @param desiredAcasCategory acasCategory where the desired values are stored
#' @param currentAcasCategory acasCategory of the entity provided
#' @param includeFromState a character vector of column names to include from the state
#' @param includeFromEntity a character vector of column names to include from the state
#' 
#' @details \code{flattenDeepEntity} pulls values out of nested objects. This can be used
#' on any ACAS object that has lsStates that have lsValues. If no information is
#' needed from the state or entity, \code{includeFromState} and
#' \code{includeFromEntity}, respectively, can be set to an empty list,
#' \code{c()}. Columns in \code{includeFromState} will have "state" prepended
#' and the first letter capitalized, while  columns in \code{includeFromEntity} 
#' will have \code{acasCategory} prepended and the first letter capitalized. The
#' list of ACAS categories can be found in \code{racas::acasEntityHierarchy}
#' (\link{acasEntityHierarchy})
#' 
#' @examples
#' \dontrun{
#' experiment <- getExperimentByCodeName("EXPT-00012398", include = "fullobject")
#' x <- flattenDeepEntity(experiment, "subject", "experiment")
#' }
flattenDeepEntity <- function(entity, desiredAcasCategory, currentAcasCategory="experiment", includeFromState = c("id", "lsType", "lsKind"), includeFromEntity = c("id")) {
  currentAcasCategoryIndex <- which(racas::acasEntityHierarchy == currentAcasCategory)
  if (desiredAcasCategory == currentAcasCategory) {
    output <- flattenEntity(entity, desiredAcasCategory, includeFromState, includeFromEntity)
  } else {
    lowerCategory <- racas::acasEntityHierarchy[currentAcasCategoryIndex + 1]
    lowerCategoryCamel <- racas::acasEntityHierarchyCamel[currentAcasCategoryIndex + 1]
    lowerCategoryCamelPlural <- paste0(lowerCategoryCamel, "s")
    if (length(entity[[lowerCategoryCamelPlural]]) == 0) {
      return(data.frame(stringsAsFactors=F))
    }
    output <- plyr::ldply(entity[[lowerCategoryCamelPlural]], flattenDeepEntity, 
                          desiredAcasCategory=desiredAcasCategory, currentAcasCategory=lowerCategory, 
                          includeFromState=includeFromState, includeFromEntity=includeFromEntity)
    if (nrow(output) > 0) {
      currentCategoryCamel <- racas::acasEntityHierarchyCamel[currentAcasCategoryIndex]
      output[, paste0(currentCategoryCamel, "Id")] <- entity$id
    }
  }
  return(output)
}

#' Flattens ACAS Entities
#' 
#' Gets values from a given entity
#' 
#' @param entity an ACAS entity such as a protocol or subject
#' @param acasCategory one of the following: "protocol", "experiment", "analysisgroup", "treatmentgroup", "subject"
#' @param includeFromState a character vector of column names to include from the state
#' @param includeFromEntity a character vector of column names to include from the state
#' 
#' \code{flattenEntity} changes the json objects that were good for Java into an
#' R data frame. This can be used on any ACAS object that has lsStates that have
#' lsValues. If no information is needed from the state or entity, 
#' \code{includeFromState} and \code{includeFromEntity}, respectively, can be 
#' set to an empty list, \code{c()}. columns in \code{includeFromState} will 
#' have "state" prepended and the first letter capitalized, while  columns in 
#' \code{includeFromEntity} will have \code{acasCategory} prepended and the
#' first letter capitalized.
#' 
#' @export
#' 
flattenEntity <- function(entity, acasCategory=NULL, includeFromState = c("id", "lsType", "lsKind"), includeFromEntity = c("id")) {
  output <- plyr::ldply(entity$lsStates, flattenState, includeFromState=includeFromState)
  entityColumnNames <- paste0(acasCategory, toupper(substring(includeFromEntity, 1, 1)), substring(includeFromEntity, 2))
  output[, entityColumnNames] <- entity[includeFromEntity]
  return(output)
}

#' Flattens an lsState
#' 
#' Gets values into a data.frame
#' 
#' @param lsState an lsState that has lsValues
#' @param includeFromState a character vector of column names to include from the state
#' 
#' Will return an empty data frame if there are no lsValues
#' 
flattenState <- function(lsState, includeFromState) {
  if (!is.list(lsState$lsValues) || length(lsState$lsValues) == 0) {
    return(data.frame(stringsAsFactors=F))
  }
  output <- plyr::ldply(lsState$lsValues, flattenValue)
  stateColumnNames <- paste0("state", toupper(substring(includeFromState, 1, 1)), substring(includeFromState, 2))
  output[, stateColumnNames] <- lsState[includeFromState]
  #names(output)[names(output) == "id"] <- "valueId"
  return(output)
}

#' @rdname saveAcasEntities
updateAcasEntity <- function(entity, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  url <- paste0(lsServerURL, acasCategory, "/")
  putURLcheckStatus(url, toJSON(entity), requireJSON = TRUE)
}

#' Change container names
#' 
#' Appends a value to a container name, ignoring the old label and replacing
#' with the new
#' 
#' @param containerName the name of the container (labelText)
#' @param appendText text to append
#' 
#' @return the container without the changes (so an id is accessible)
#' 
#' @examples
#' \dontrun{
#' container <- appendToContainerName("AP0001", "_fail")
#' container$ignored <- TRUE
#' container$lsStates <- NULL
#' container$lsLabels <- NULL
#' updateAcasEntity(container, "containers")
#' }
appendToContainerName <- function(containerName, appendText) {
  containers <- getContainerByLabelText(containerName)
  if (length(containers) > 1) {
    warnUser("More than one container has the given name, will change the first one")
  }
  container <- containers[[1]]
  containerLabels <- container$lsLabels
  oldPreferredLabel <- containerLabels[vapply(containerLabels, getElement, c(TRUE), "preferred")][[1]]
  newLabel <- oldPreferredLabel
  newLabel$container <- container
  newLabel$labelText <- paste0(newLabel$labelText, appendText)
  newLabel$id <- NULL
  oldPreferredLabel$ignored <- TRUE
  oldPreferredLabel$preferred <- FALSE
  oldPreferredLabel$container <- container
  updateAcasEntity(oldPreferredLabel, "containerlabels")
  saveAcasEntity(newLabel, "containerlabels")
  return(container)
}

#' Flattens an lsValue
#' 
#' @param lsValue an lsValue
#' 
#' Just turns a list into a data frame, not meant to be exported
flattenValue <- function(lsValue) {
  lsValue[vapply(lsValue, is.null, c(TRUE))] <- NA
  output <- as.data.frame(lsValue, stringsAsFactors=FALSE)
  return(output)
}

#' Flattens an lsLabel
#' 
#' @param lsLabel an lsLabel
#' 
#' Just turns a list into a data frame, not meant to be exported
flattenLabel <- function(lsLabel) {
  lsLabel[vapply(lsLabel, is.null, c(TRUE))] <- NA
  output <- as.data.frame(lsLabel, stringsAsFactors=FALSE)
  return(output)
}

#' Flattens a list of lsLabels
#' 
#' @param lsLabels a list os lsLabels
#' 
#' Just turns a list into a data frame, not meant to be exported
flattenLabels <- function(lsLabels) {
  ldply(lsLabels, flattenLabel)
}

#' Gets an experiment
#' 
#' Gets an experiment by id or codename, with options of what to get
#' 
#' @param experimentId the id of the experiment
#' @param experimentCodeName the codename of an experiment
#' @param include a character string describing what to include
#' @param errorEnv the environment where errors will be stored to
#' @param lsServerURL the url for the roo server
#'   
#' @details \code{include} can be in the list: \itemize{ 
#' \item{analysisgroups: returns the experiment stub with analysis group stubs} 
#' \item{fullobject: returns the full experiment object (warning: this may be 
#' slow if there is a lot of data)}
#' \item{prettyjsonstub: returns the experiment stub in pretty json format}
#' \item{prettyjson: returns the full experiment in pretty json format}
#' \item{analysisgroupvalues: returns the experiment stub with full analysis
#' groups}
#' \item{analysisgroupstates: returns the experiment stub with analysis group
#' states}}
#' If left blank, an experiment stub (with states and values) is returned. The
#' codeName will do the same as include=analysisgroups.
#'   
#' @return the experiment object, or if it does not exist, \code{addError} is
#'   run and NULL is returned
#' 
#' @export
#' 
getExperimentById <- function(experimentId, include=NULL, errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  experiment <- getEntityById(experimentId, "experiments/stub", include = include, errorEnv = errorEnv, lsServerURL = lsServerURL)
  return(experiment)
}

#' @rdname getExperimentById
#' @export
getExperimentByCodeName <- function(experimentCodeName, include=NULL, errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  experiment <- getEntityByCodeName(experimentCodeName, "experiments", include = include, errorEnv = errorEnv, lsServerURL = lsServerURL)
  return(experiment)
}

#' The hierarcy of ACAS entities
#' 
#' Each category of entity contains the level below. A lowercase version (for
#' URLs), a camelCase verison (for JSON), and a spaced version (for getting
#' codeNames) exist. Can be found in \code{racas::acasEntityHierarchy},
#' \code{racas::acasEntityHierarchyCamel} and
#' \code{racas::acasEntityHierarchySpace}
acasEntityHierarchy <- c("protocol", "experiment", "analysisgroup", "treatmentgroup", "subject")

#' @rdname acasEntityHierarchy
acasEntityHierarchyCamel <- c("protocol", "experiment", "analysisGroup", "treatmentGroup", "subject")

#' @rdname acasEntityHierarchy
acasEntityHierarchySpace <- c("protocol", "experiment", "analysis group", "treatment group", "subject")

#' Get parent of ACAS entity
#' 
#' Get the parent of an acas entity (experiment, analysis group, etc.). Useful 
#' for generic functions that can accept any level.
#' 
#' @param entityKind Something from the racas::acasEntityHierarchy (or
#'   acasEntityHierarchyCamel or acasEntityHierarchySpace)
#' @param currentMode One of "lowercase", "camel", or "space"
#' 
#' @details returns an empty character vector when given "protocol"
parentAcasEntity <- function(entityKind, currentMode = "lowercase") {
  switch(
    currentMode,
    lowercase = acasEntityHierarchy[which(entityKind == acasEntityHierarchy) - 1],
    camel = acasEntityHierarchyCamel[which(entityKind == acasEntityHierarchyCamel) - 1],
    space = acasEntityHierarchySpace[which(entityKind == acasEntityHierarchySpace) - 1],
    stop(paste0("Internal error: ", currentMode, " is not a valid mode")))
}

#' Change mode ACAS entity
#' 
#' Changes the mode of an acas entity from camelcase to all lowercase or with a
#' space between words
#' 
#' @param entityKind A list, one of racas::acasEntityHierarchy (or 
#'   acasEntityHierarchyCamel or acasEntityHierarchySpace)
#' @param currentMode One of "lowercase", "camel", or "space"
#' @param desiredMode One of "lowercase", "camel", or "space"
changeEntityMode <- function(entityKind, currentMode, desiredMode) {
  entityKindIndex <- switch(
    currentMode,
    lowercase = which(entityKind == acasEntityHierarchy),
    camel = which(entityKind == acasEntityHierarchyCamel),
    space = which(entityKind == acasEntityHierarchySpace),
    stop(paste0("Internal error: ", currentMode, " is not a valid mode")))
  
  return(switch(
    desiredMode,
    lowercase = acasEntityHierarchy[entityKindIndex],
    camel = acasEntityHierarchyCamel[entityKindIndex],
    space = acasEntityHierarchySpace[entityKindIndex],
    stop(paste0("Internal error: ", desiredMode, " is not a valid mode"))))
}

#' Gets Entity Name
#' 
#' Determines the preferred name of an entity (protocol, experiment, etc.)
#' 
#' @param entity an ACAS entity such as a protocol or subject
#'   
#' @details returns the name that has \code{preferred==TRUE},
#'   \code{ignored==FALSE}. Ties are broken by the most recent
#'   \code{recordedDate}.
#'   
#' @return a string name
#' @export
getPreferredName <- function(entity) {
  labelList <- entity$lsLabels
  labelFrame <- flattenLabels(labelList)
  # limit to labels that are names and not ignored
  labelFrame <- labelFrame[labelFrame$lsType == "name" & !labelFrame$ignored & labelFrame$preferred, ]
  if (nrow(labelFrame) < 1) {
    stop("No preferred label found")
  }
  bestIndex <- which.max(labelFrame$recordedDate)
  if (length(bestIndex) == 0) {
    bestName <- labelFrame$labelText[1]
  } else {
    bestName <- labelFrame$labelText[bestIndex]
  }
  return(bestName)
}

#' Gets a protocol
#' 
#' Gets a protocol by id or codename, with options of what to get
#' 
#' @param protocolId the id of the protocol
#' @param protocolCodeName the codename of an protocol
#' @param include a character string describing what to include
#' @param errorEnv the environment where errors will be stored to
#' @param lsServerURL the url for the roo server
#' @details \code{include} not yet implemented by roo server
#' @export
getProtocolById <- function(id, include="", errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  protocol <- getEntityById(id, "protocols", include = include, errorEnv = errorEnv, lsServerURL = lsServerURL)
  return(protocol)
}

#' @rdname getProtocolById
#' @export
getProtocolByCodeName <- function(protocolCodeName, include="", errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  protocol <- getEntityByCodeName(protocolCodeName, "protocols", include = include, errorEnv = errorEnv, lsServerURL = lsServerURL)
  return(protocol)
}

#' Gets an experiment state
#' 
#' Gets an experiment state by stateKind if it exists, or creates a new one if
#' not.
#' 
#' @param experiment an experiment object
#' @param stateType lsType of the state
#' @param stateKind lsKind of the state
#' @param recordedBy the current username
#' @param lsTransaction the id of the transaction
#' @param lsServerURL the url for the roo server
#' @details This will fail if the experiment has more than one non-ignored state
#'   of entered stateKind, as it would be unclear which to update.
#' @export
getOrCreateExperimentState <- function(experiment, stateType, stateKind, recordedBy, lsTransaction, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  getOrCreateEntityState(experiment, "experiment", stateType, stateKind, recordedBy, lsTransaction, lsServerURL)
}

#' Gets a state
#' 
#' Gets a state by stateKind if it exists, or creates a new one if not
#' 
#' @param entity an entity object, such as an experiment or analysis group
#' @param entityKind the kind of entity, such as "experiment" or "analysisgroup", see \link{acasEntityHierarchy}
#' @param stateType lsType of the state
#' @param stateKind lsKind of the state
#' @param recordedBy the current username
#' @param lsTransaction the id of the transaction
#' @param lsServerURL the url for the roo server
#' @details This will fail if the entity has more than one non-ignored state of entered stateKind, as it would be unclear which to update.
#' @export
getOrCreateEntityState <- function(entity, entityKind, stateType, stateKind, recordedBy, lsTransaction, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  lsStates <- Filter(f = function(x) x$lsKind == stateKind && x$lsType == stateType && !x$ignored, 
                     x = entity$lsStates)
  if (length(lsStates) > 1) {
    stopUser("Usage: getStateOrCreate cannot be used with multiple lsStates of the same lsKind")
  } else if (length(lsStates) == 1) {
    lsState <- lsStates[[1]]
  } else {
    # Does not exist yet
    lsState <- createLsState(recordedBy = recordedBy, 
                             lsType = stateType, lsKind = stateKind, lsTransaction = lsTransaction)
    lsState[entityKind] <- entity
    lsState <- saveAcasEntity(lsState, paste0(entityKind, "states"), lsServerURL)
  }
  return(lsState)
}

#' Gets an entity by id
#' 
#' Gets an entity object by id and kind
#' 
#' @param entity an entity object, such as an experiment or analysis group
#' @param entityKind the kind of entity, such as "experiment" or
#'   "analysisgroup", see \link{acasEntityHierarchy}
#' @param include a character string describing what to include
#' @param lsServerURL the url for the roo server
#' @details This will fail with an error if the object does not exist.
#'   \code{include} is only implemented for some entities, see
#'   \link{getExperimentById} for more detail.
#' @return a named list representing an object.
#' @export
getEntityById <- function(id, entityKind, include="", errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  logName <- "com.acas.racas.getEntityById"
  logFileName <- "racas.log"
  if(is.null(include) || include == "") {
    url <- paste0(lsServerURL, entityKind, "/", id)
  } else {
    url <- paste0(lsServerURL, entityKind, "/", id, "?with=", include)
  }
  response <- getURLcheckStatus(url, requireJSON = TRUE)
  tryCatch({
    entity <- fromJSON(response)
  }, error = function(e) {
    stopUserAndLogInvalidJSON(logName, logFileName, url, response)
  })
  return(entity)
}

#' @rdname getEntityById
getEntityByCodeName <- function(codeName, entityKind, include="", errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  logName <- "com.acas.racas.getEntityByCodeName"
  logFileName <- "racas.log"
  if (is.null(include)) {
    include = ""
  } else {
    include = paste0("?with=", include)
  }
  url <- paste0(lsServerURL, entityKind, "/codename/", codeName, include)
  response <- getURLcheckStatus(url, requireJSON = TRUE)
  tryCatch({
    entity <- fromJSON(response)
  }, error = function(e) {
    stopUserAndLogInvalidJSON(logName, logFileName, url, response)
  })
  return(entity)
}

#' @rdname saveAcasEntities
getAcasEntity <- function() {
  stop("Use getEntityById or getEntityByCodeName")
}

#' @rdname saveAcasEntities
getAcasEntities <- function(acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  return(getURLcheckStatus(paste0(lsServerURL, acasCategory)))
}

#' Update a value
#' 
#' If a value with the correct lsType and lsKind exists within the provided lsState, it is updated, otherwise, it is created.
#' 
#' @param entityKind the kind of parent entity, such as "experiment" or "analysisgroup", see \link{acasEntityHierarchy}
#' @param lsType type of the value
#' @param lsKind lsKind of the value
#' @param stringValue string, <255 characters
#' @param fileValue file code or path
#' @param urlValue url
#' @param publicData TRUE to be visible
#' @param ignored TRUE to mark as old
#' @param dateValue date in milliseconds
#' @param clobValue clob
#' @param blobValue blob
#' @param valueOperator ">" or "<"
#' @param operatorType "comparison", not yet implemented
#' @param numericValue numeric
#' @param sigFigs integer
#' @param uncertainty numeric
#' @param uncertaintyType "standard deviation"
#' @param numberOfReplicates integer
#' @param valueUnit "uM", etc.
#' @param unitType not yet implemented
#' @param comments used by fileValue for a filename, flags for comments, etc.
#' @param lsTransaction id of the transaction
#' @param codeValue codename of something
#' @param lsState a state object
#' @param testMode used for testing
#' @param recordedBy the current username
#' @param lsServerURL the url for the roo server
#' @details This will fail if the entity has more than one non-ignored state of entered stateKind, 
#' as it would be unclear which to update. \code{\link{updateValueByTypeAndKind}} is often easier to use.
#' @return a named list of the lsValue object
#' @export
updateOrCreateStateValue <- function(entityKind, lsState, lsType, lsKind, stringValue=NULL, fileValue=NULL,
                                     urlValue=NULL, publicData=TRUE, ignored=FALSE,
                                     dateValue=NULL, clobValue=NULL, blobValue=NULL, valueOperator=NULL, operatorType=NULL, numericValue=NULL,
                                     sigFigs=NULL, uncertainty=NULL, uncertaintyType=NULL,
                                     numberOfReplicates=NULL, valueUnit=NULL, unitType=NULL, comments=NULL, 
                                     lsTransaction=NULL, codeValue=NULL, recordedBy="username",
                                     testMode=FALSE, recordedDate=as.numeric(format(Sys.time(), "%s"))*1000,
                                     codeType = NULL, codeKind = NULL, codeOrigin = NULL, 
                                     lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  lsValues <- Filter(f = function(x) x$lsKind == lsKind && x$lsType == lsType && !x$ignored, 
                     x = lsState$lsValues)
  newLsValue <- createStateValue(
    lsType=lsType, lsKind=lsKind, stringValue=stringValue, fileValue=fileValue,
    urlValue=urlValue, publicData=publicData, ignored=ignored,
    dateValue=dateValue, clobValue=clobValue, blobValue=blobValue, valueOperator=valueOperator, 
    operatorType=operatorType, numericValue=numericValue,
    sigFigs=sigFigs, uncertainty=uncertainty, uncertaintyType=uncertaintyType,
    numberOfReplicates=numberOfReplicates, valueUnit=valueUnit, unitType=unitType, comments=comments, 
    lsTransaction=lsTransaction, codeValue=codeValue, recordedBy=recordedBy,
    lsState=lsState, testMode=testMode, recordedDate=recordedDate,
    codeType = codeType, codeKind = codeKind, codeOrigin = codeOrigin)
  if (length(lsValues) > 1) {
    stopUser("Usage: updateOrCreateStateValue cannot be used with multiple lsValues of the same lsKind")
  } else if (length(lsValues) == 1) {
    lsValue <- lsValues[[1]]
    newLsValue$id <- lsValue$id
    newLsValue$version <- lsValue$version
    output <- updateAcasEntity(newLsValue, "experiment", lsServerURL = lsServerURL)
  } else {
    # Does not exist yet
    output <- saveAcasEntity(newLsValue, paste0(entityKind, "values"), lsServerURL)
  }
  return(output)
}

#' Update Values
#' 
#' Updates values without requiring knowledge of whether the value already
#' exists or not- it will be checked by the roo server. Also adds valueType and
#' valueKind if needed.
#' 
#' @param newValue value to save, will be sent as a string
#' @param entityKind kind of entity, e.g. "experiment"
#' @param parentId id of the parent entity
#' @param stateType lsType of the state
#' @param stateKind lsKind of the state
#' @param valueType lsType of the value
#' @param valueKind lsKind of the value
#' @return updated value object
#' @export
updateValueByTypeAndKind <- function(newValue, entityKind, parentId, stateType, stateKind, valueType, valueKind, 
                                     lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  #url <- "acas/api/v1/values/{entity}/{idOrCodeName}/bystate/{stateType}/{stateKind}/byvalue/{valueType}/{valueKind}/"
  url <- paste0(lsServerURL, "values/", entityKind, "/", parentId, "/bystate/", 
                stateType, "/", stateKind, "/byvalue/", valueType, "/", valueKind, "/")
  putURLcheckStatus(URLencode(url), postfields = newValue, requireJSON = TRUE)
}
#' Get Or Create Value Kind
#' 
#' Gets or creates a set of value kinds given a data frame of lsType (name only) and lsKind (name)
#' 
#' @param a data frame (or data table) with columns lsType and lsKind
#' @return a list object of returned lsKinds
#' @export
get_or_create_value_kinds <- function(df, persistence_full_path = racas::applicationSettings$client.service.persistence.fullpath) {
  valueTypeAndKindsJSON <- jsonlite::toJSON(df)
  response <- fromJSON(getURL(
    paste0(persistence_full_path, "valuekinds/getOrCreate/jsonArray"),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=valueTypeAndKindsJSON))
  return(response)
}
#' Loads a set of default lsKinds per module
#' 
#' There are a set of required lsKinds that need to be loaded in modules. This function loads them all if given no input or a subset if provied
#' 
#' @param requiredModules a character vector of modules to load see \link{modules}
#' @return a list object of returned lsKinds
#' @export
load_value_type_and_kinds <- function(requiredModules = NA, ...) {
  valueTypeAndKindsFile <- system.file("docs", "value_type_and_kinds.csv", package = "racas")
  valueTypeAndKinds <- fread(valueTypeAndKindsFile)
  if(!is.na(requiredModules)) {
    valueTypeAndKinds <- valueTypeAndKinds[Module %in% requiredModules]
  }
  valueTypeAndKinds[ , Module:= NULL]
  valueTypeAndKinds <- unique(valueTypeAndKinds)
  setnames(valueTypeAndKinds, c("lsType", "lsKind"))
  return(get_or_create_value_kinds(valueTypeAndKinds, ...))
}
#' Known acas modules which require lsKinds to exist
#' 
#' There are a set of required lsKinds that need to be loaded in modules. This function returns the module names for which there are required lsKinds that need to be registered
#' See \link{load_value_type_and_kinds} to load lsKinds
#' 
#' @return a list of modules which require lsKinds to be registered
#' @export
modules <- function() {
  valueTypeAndKindsFile <- system.file("docs", "value_type_and_kinds.csv", package = "racas")
  valueTypeAndKinds <- fread(valueTypeAndKindsFile)
  return(unique(valueTypeAndKinds$Module))
}

#' Get preferred labels
#' 
#' Gets the best label, possibly limited to a labelTypeAndKind. "Best" is 
#' defined as the preferred label, or if that fails, the most recent one. This
#' is translated from coffeescript Label.coffee.
#' 
#' @param entity a list entity, such as a protocol or experiment.
#' @param labelTypeAndKind a labelTypeAndKind such as "name_protocol name".
#' @return A label list object.
pickBestLabel <- function(entity, labelTypeAndKind = NA) {
  if (length(entity$lsLabels) == 0) {
    stop("no labels found")
  }
  if (!is.na(labelTypeAndKind)) {
    correctLabels <- Filter(function(x) x$lsTypeAndKind == labelTypeAndKind, entity$lsLabels)
  } else {
    correctLabels <- entity$lsLabels
  }
  if (length(correctLabels) == 0) {
    stop(paste("no labels found with labelTypeAndKind", labelTypeAndKind))
  }
  preferredLabels <- Filter(function(x) x$preferred, correctLabels)
  if (length(preferredLabels) > 1) {
    dates <- vapply(preferredLabels, getElement, 1, "recordedDate")
    bestLabelIndex <- which(dates == max(dates))
    return(preferredLabels[[bestLabelIndex]])
  } else if (length(preferredLabels) == 1) {
    return(preferredLabels[[1]])
  } else {
    dates <- vapply(correctLabels, getElement, 1, "recordedDate")
    bestLabelIndex <- which(dates == max(dates))
    return(correctLabels[[bestLabelIndex]])
  }
}

#' Get preferred label text
#' 
#' Gets the preferred name of an entity.
#' 
#' @param entity a list entity, such as a protocol or experiment.
#' @param labelTypeAndKind a labelTypeAndKind such as "name_protocol name".
#' @return A text string.
getPreferredLabelText <- function(entity, labelTypeAndKind = NA) {
  pickBestLabel(entity, labelTypeAndKind)$labelText
}

#' Get ddict values by type and kind
#' 
#' Gets all ddict values by type and kind
#' 
#' @param lsKind
#' @param lsType
#' @param format (json, tsv)
#' @return A data.frame or json of ddict values
getDDictValuesByTypeKindFormat <- function(lsKind, lsType, format = "json", lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  getURLcheckStatus(URLencode(paste0(lsServerURL, "ddictvalues/all/",lsType,"/",lsKind,"/",format)))
}
#' getOrCreateDDictTypes
#' 
#' Register's ddict types from json
#' 
#' @param list (described here URLencode(paste0(racas::applicationSettings$client.service.persistence.fullpath,"/api/v1/setup/ddicttypes")))
#' @return list of types
getOrCreateDDictTypes <- function(typesList, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  json <- toJSON(typesList)
  url <- URLencode(paste0(lsServerURL, "setup/ddicttypes"))
  response <- postURLcheckStatus(url, postfields=json, requireJSON = TRUE)
  return(response)
}
#' getOrCreateDDictKinds
#' 
#' Registers ddict kinds from json
#' 
#' @param typesKindsDataFrame (described here URLencode(paste0(racas::applicationSettings$client.service.persistence.fullpath,"/api/v1/setup/ddictkinds")))
#' @return list of types and kinds
getOrCreateDDictKinds <- function(typesKindsDataFrame, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  json <- jsonlite::toJSON(typesKindsList)
  url <- URLencode(paste0(lsServerURL, "setup/ddictkinds"))
  response <- postURLcheckStatus(url, postfields=json, requireJSON = TRUE)
  return(response)
}
#' getDdictKinds
#' 
#' Get ddict kinds as described by URLencode(paste0(racas::applicationSettings$client.service.persistence.fullpath,"/api/v1/ddictkinds"))
#' 
#' @param lsServerURL (racas::applicationSettings$client.service.persistence.fullpath)
#' @return a data frame of kinds
getDDictKinds <- function(lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  url <- URLencode(paste0(lsServerURL, "ddictkinds"))
  response <- jsonlite::fromJSON(getURLcheckStatus(url))
  return(response)
}
#' createCodeTablesFromJsonArray
#' 
#' Create code table (d dict values) from json array as described here URLencode(paste0(racas::applicationSettings$client.service.persistence.fullpath,"ddictvalues/codetable/jsonArray"))
#' 
#' @param codeTableDataFrame of ddict values
#' @return a data frame of kinds
createCodeTablesFromJsonArray <- function(codeTableDataFrame, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  json <- jsonlite::toJSON(codeTableDataFrame)
  url <- URLencode(paste0(lsServerURL, "ddictvalues/codetable/jsonArray"))
  response <- postURLcheckStatus(url, postfields=json, requireJSON = TRUE)
  return(response)
}




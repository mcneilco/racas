## R code follows here
## example R code to interact with LabSynch JSON services

options(scipen=99)

#racas::applicationSettings$client.service.persistence.fullpath <- "http://localhost:8080/labseer/"
#racas::applicationSettings$client.service.persistence.fullpath <- "http://host3.labsynch.com:8080/acas/"


############  FUNCTIONS ########################

# http://localhost:8080/labseer/labelsequences?getNextLabelSequences&thingType=document&thingKind=protocol&labelType=id&labelKind=codeName&numberOfLabels=10
# http://host3.labsynch.com:8080/labseer/labelsequences?getNextLabelSequences&thingType=document&thingKind=protocol&labelType=id&labelKind=codeName&numberOfLabels=10

# curl http://host3.labsynch.com:8080/labseer/protocols/codename/PROT-000101
# curl http://host3.labsynch.com:8080/labseer/experiments/codename/EXP-000-101


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
  response <- getURL(
    paste(lsServerURL, "labelsequences/getLabels", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(labelSequenceDTO))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to get labels. Instead, it got this response:", response))
  }
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

createExperimentLabel <- function(experiment=NULL, labelText, recordedBy="authorName", lsType="name", lsKind="experiment name", lsTransaction=NULL, preferred=TRUE, ignored=FALSE){
  experimentLabel = list(
    experiment=experiment,
    labelText=labelText,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    preferred=preferred,
    ignored=ignored,
    lsTransaction=lsTransaction,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
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
createExperimentState <- function(experimentValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL, experiment=NULL, testMode=FALSE){
  experimentState = list(
    experiment=experiment, #This will fail if not given an id and version (but the version does not matter)
    lsValues=experimentValues,
    recordedBy=recordedBy,
    lsType=lsType,
    lsKind=lsKind,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=if(testMode) 1376954591000 else as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(experimentState)
}

createAnalysisGroupState <- function(analysisGroup = NULL, analysisGroupValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL, testMode=FALSE){
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

createTreatmentGroupState <- function(treatmentGroup=NULL, treatmentGroupValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL){
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
createTreatmentGroup <- function(analysisGroup=NULL,subjects=NULL,treatmentGroupStates=NULL, lsType="default", lsKind="default", codeName=NULL, recordedBy="userName", comments="", lsTransaction=NULL){
  
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="document_treatment group", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
  }
  
  treatmentGroup= list(
    analysisGroup=analysisGroup,
    lsType=lsType, 
    lsKind=lsKind, 
    codeName=codeName,		
    subjects=subjects,
    lsStates=treatmentGroupStates,
    recordedBy=recordedBy,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(treatmentGroup)
}
createSubject <- function(treatmentGroup=NULL, subjectStates=NULL, lsType="default", lsKind="default", codeName=NULL, recordedBy="userName", comments="", lsTransaction=NULL){
  
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="document_subject", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
  }
  
  subject= list(
    treatmentGroup=treatmentGroup,
    lsType=lsType,
    lsKind=lsKind,
    codeName=codeName,
    lsStates=subjectStates,
    recordedBy=recordedBy,
    comments=comments,
    lsTransaction=lsTransaction,
    ignored=FALSE,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  return(subject)
}

createSubjectState <- function(subject=NULL, subjectValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", comments="", lsTransaction=NULL){
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
#'Creates a state value, used either in all cases
#'
#'@param lsType
#'@param lsKind
#'@param stringValue
#'@param fileValue
#'@param urlValue
#'@param publicData
#'@param ignored
#'@param dateValue
#'@param clobValue
#'@param blobValue
#'@param valueOperator
#'@param operatorType
#'@param numericValue
#'@param sigFigs
#'@param uncertainty
#'@param uncertaintyType
#'@param numberOfReplicates
#'@param valueUnit
#'@param unitType
#'@param comments
#'@param lsTransaction
#'@param codeValue
#'@param recordedBy
#'@param lsState
#'@param testMode used for testing
#'
#'@details Use either in a nested object or alone
#'
#'@return list, a value object
#'@export
createStateValue <- function(lsType="lsType", lsKind="lsKind", stringValue=NULL, fileValue=NULL,
                             urlValue=NULL, publicData=TRUE, ignored=FALSE,
                             dateValue=NULL, clobValue=NULL, blobValue=NULL, valueOperator=NULL, operatorType=NULL, numericValue=NULL,
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
                             lsTransaction=NULL, recordedBy="userName", experimentLabels=list(), experimentStates=list(), lsTags=list()){
  if (is.null(codeName) ) {
    codeName <- getAutoLabels(thingTypeAndKind="document_experiment", labelTypeAndKind="id_codeName", numberOfLabels=1)[[1]][[1]]						
  }
  experiment <- list(
    protocol=protocol,
    codeName=codeName,
    lsType=lsType,
    lsKind=lsKind,
    shortDescription=shortDescription,
    recordedBy=recordedBy,
    lsTransaction=lsTransaction,
    lsLabels=experimentLabels,
    lsStates=experimentStates,
    lsTags=lsTags,
    recordedDate=as.numeric(format(Sys.time(), "%s"))*1000
  )
  
  return(experiment)	
}			


createAnalysisGroup <- function(experiment=NULL, codeName=NULL, lsType="default", lsKind="default", lsTransaction=NULL, recordedBy="userName",
                                treatmentGroups=NULL, analysisGroupStates=NULL, testMode=FALSE){
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
    experiment=experiment,
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

createContainerState <- function(container=NULL,containerValues=NULL, recordedBy="userName", lsType="lsType", lsKind="lsKind", 
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
                                 lsTransaction=NULL, preferred=TRUE, imageFile=NULL, labelTypeAndKind=NULL, physicallyLabeled=FALSE,
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
    labelTypeAndKind=labelTypeAndKind,
    physicallyLabeled=physicallyLabeled,
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
    interactionStates=interactionStates,
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
  response <- getURL(
    paste(lsServerURL, "protocols/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(protocols))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your protocols. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}


saveProtocol <- function(protocol, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "protocols/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(protocol))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your protocol. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}



saveExperiment <- function(experiment, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "experiments/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(experiment))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your experiment. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}


saveExperiments <- function(experiments, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "experiments/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(experiments))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your experiments. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveAnalysisGroups <- function(analysisGroups, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  message <- toJSON(analysisGroups)
  # toJSON fails with NA, NaN, and Inf, but so far it seems that these have been successfully stripped out
  #message <- gsub("\"NA\"|\"NaN\"", "null", message)
  response <- getURL(
    paste(lsServerURL, "analysisgroups/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your data. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveAnalysisGroup <- function(analysisGroup, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "analysisgroups/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(analysisGroup))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your data. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

# Currently, this cannot accept labels and states
saveContainer <- function(container, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "containers/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(container))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your container. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveContainers <- function(containers, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "containers/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containers))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your containers. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveContainerLabel <- function(containerLabel, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, "containerlabels/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containerLabel))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your container label. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveContainerLabels <- function(containerLabels, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, "containerlabels/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containerLabels))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your container labels. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveContainerState <- function(containerState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, "containerstates/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containerLabel))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your container state. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveContainerStates <- function(containerStates, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, "containerstates/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containerStates))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your container states. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveContainerContainerInteraction <- function(containerContainerInteraction, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "itxcontainercontainers/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containerContainerInteraction))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your interaction. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveContainerContainerInteractions <- function(containerContainerInteractions, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "itxcontainercontainers/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containerContainerInteractions))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your interactions. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveSubjectContainerInteraction <- function(subjectContainerInteraction, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "itxsubjectcontainers/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(subjectContainerInteraction))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your interaction. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveProtocolLabel <- function(containerLabel, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, "protocollabels/", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(containerLabel))
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your container label. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveAcasEntity <- function(entity, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  # If you have trouble, make sure the acasCategory is all lowercase, has no spaces, and is plural
  message <- toJSON(entity)
  response <- getURL(
    paste0(lsServerURL, acasCategory, "/"),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    myLogger <- createLogger(logName="com.acas.sel", logFileName = "racas.log")
    myLogger$error(response)
    stopUser (paste0("The loader was unable to save your ", acasCategory ,". Check the logs at ", Sys.time()))
  }
  response <- fromJSON(response)
  return(response)
}

#' Save ACAS entities to the server
#' 
#' Save protocols, labels, experiments, etc.
#' 
#' @param entities a list of entities
#' @param acasCategory e.g. "experiments", "subjectlabels", etc.
#' @param lsServerURL url of ACAS server
#' @return a list, sometimes empty
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
  
  response <- postURLcheckStatus(
    paste0(lsServerURL, acasCategory, "/jsonArray"),
    postfields=message,
    httpheader=c('Content-Type'='application/json')
  )
  
  if (grepl("^\\s*$", response)) {
    return(list())
  }
  
  response <- fromJSON(response)
  return(response)
}

saveAnalysisGroupState <- function(analysisGroupState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  message <- toJSON(analysisGroupState)
  response <- getURL(
    paste(lsServerURL, "analysisgroupstates", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your analysis group state. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveAnalysisGroupStates <- function(analysisGroupStates, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  message <- toJSON(analysisGroupStates)
  response <- getURL(
    paste(lsServerURL, "analysisgroupstates/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your analysis group states. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveExperimentState <- function(experimentState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  message <- toJSON(experimentState)
  response <- getURL(
    paste(lsServerURL, "experimentstates", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your experiment state. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveExperimentStates <- function(experimentStates, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  message <- toJSON(experimentStates)
  response <- getURL(
    paste(lsServerURL, "experimentstates/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your experiment states. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveExperimentValue <- function(experimentValue, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  message <- toJSON(experimentValue)
  response <- getURL(
    paste(lsServerURL, "experimentvalues", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your experiment value. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveExperimentValues <- function(experimentValues, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  message <- toJSON(experimentValues)
  response <- getURL(
    paste(lsServerURL, "experimentvalues/jsonArray", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=message)
  if (grepl("^<",response)) {
    stopUser (paste("The loader was unable to save your experiment values. Instead, it got this response:", response))
  }
  response <- fromJSON(response)
  return(response)
}

saveLabelSequence <- function(labelSequence, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, "labelsequences", sep=""),
    customrequest='POST',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(labelSequence))
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
  response <- getURL(
    paste(lsServerURL, "experiments/",experiment$id, sep=""),
    customrequest='DELETE',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(experiment))
  if(response!="") {
    stopUser (paste("The loader was unable to delete the old experiment. Instead, it got this response:", response))
  }
  return(response)
}

deleteExperimentValue <- function(experimentValue, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  response <- getURL(
    paste(lsServerURL, "experimentvalues/",experimentValue$id, sep=""),
    customrequest='DELETE',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(experimentValue))
  if(response!="") {
    stopUser (paste("The loader was unable to delete the experiment values. Instead, it got this response:", response))
  }
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
#' @export
deleteAnalysisGroupByExperiment <- function(experiment, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath){
  tryCatch({
    response <- getURLcheckStatus(
      paste0(lsServerURL, "experiments/", experiment$id, "?with=analysisgroups"),
      customrequest='DELETE',
      httpheader=c('Content-Type'='application/json'),
      postfields=toJSON(experiment))
  }, error = function(e) {
    stop(paste0("The loader was unable to delete the experiment's analysis groups. Check the logs at ", Sys.time()))
  })
  return(response)
}

deleteAnalysisGroupState <- function(analysisGroupState, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, "analysisgroupstates/",analysisGroupState$id, sep=""),
    customrequest='DELETE',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(analysisGroupState))
  if(response!="") {
    stopUser (paste("The loader was unable to delete the old analysis group state. Instead, it got this response:", response))
  }
  return(response)
}

deleteEntity <- function(entity, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, acasCategory, "/", entity$id, sep=""),
    customrequest='DELETE',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(entity))
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
  tryCatch({
    response <- getURLcheckStatus(
      paste(lsServerURL, "containers/findByLabels/jsonArray", sep=""),
      customrequest='POST',
      httpheader=c('Content-Type'='application/json'),
      postfields=toJSON(labelList))
    response <- fromJSON(response)
  }, error = function(e) {
    stopUser (paste0("Internal Error: The loader was unable to get container labels by text. Check the logs at ", Sys.time()))
  })
#     response <- getURLstatusCheck(paste0(lsServerURL,
#                                          "containerlabels?find=ByLabelTextEqualsAndIgnoredNot&labelText=", searchText,
#                                          "&ignored=", ifelse(ignored, "on", "off")))
#     response <- fromJSON(response)
#     }, error = function(e) {
#       stopUser (paste0("Internal Error: The loader was unable to get container labels. Check the logs at ", Sys.time()))
#     })
  return(response)
}

#' Get URL and check status
#'
#' This is a wrapper for getURL that throws an error when the HTTP status is 400 or greater
#' 
#'@param url the url to get/post
#'@param ... optional parameters passed to getURL
#'
#'@details checks the HTTP status and logs to racas.log as com.acas.sel if 400 or greater
getURLcheckStatus <- function(url, ...) {
  logName <- "com.acas.sel"
  logFileName <- "racas.log"
  h <- basicTextGatherer()
  response <- getURL(url=url, ..., headerfunction = h$update)
  statusCode <- as.numeric(as.list(parseHTTPHeader(h$value()))$status)
  if (statusCode >= 400) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    myLogger$error(response)
    stopUser (paste("Server Error, see logs at", Sys.time()))
  }
  return(response)
}

#'Get URL and check status
#'
#'This is similar to getURLcheckStatus, but does a POST, and the postfields are
#'logged in case of an error
#'
#'@param url the url to get/post
#'@param ... optional parameters passed to getURL
#'@param postfields data sent to the server
#'  
#'@details checks the HTTP status and logs to racas.log as com.acas.sel if 400
#'  or greater
postURLcheckStatus <- function(url, postfields, ...) {
  logName <- "com.acas.sel"
  logFileName <- "racas.log"
  h <- basicTextGatherer()
  response <- getURL(url=url, ..., postfields=postfields, customrequest='POST', headerfunction = h$update)
  responseHeader <- as.list(parseHTTPHeader(h$value()))
  statusCode <- as.numeric(responseHeader$status)
  if (statusCode >= 400) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0("Request to ", url, " with method 'POST' failed with status '",
                           statusCode, " ", responseHeader$statusMessage, "' when sent the following JSON: \n", 
                           message, "\nHeader was \n", h$value())
    myLogger$error(errorMessage)
    stopUser (paste0("Internal Error: The loader was unable to save your data. Check the log ", 
                     logFileName, " at ", Sys.time()))
  } else if (grepl("^<",response)) {
    myLogger <- createLogger(logName = logName, logFileName = logFileName)
    errorMessage <- paste0("POST:\n", postfields, "\nResponse:\n", response)
    myLogger$error(errorMessage)
    stopUser (paste0("Internal Error: The loader was unable to save your data. Check the logs at ", Sys.time()))
  }
  return(response)
}

#' Protocol search by name
#' 
#' Gets protocols by name
#' 
#' @param protocolName a string, the name of the protocol
#' 
#' @return a list of protocols
#' 
#' @details returns a list as uniqueness is not always enforced
#' @export
getProtocolsByName <- function(protocolName) { 
  tryCatch({
    protocolList <- fromJSON(getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, 
                                           "protocols?FindByProtocolName&protocolName=", 
                                           URLencode(protocolName, reserved = TRUE))))
  }, error = function(e) {
    stopUser("There was an error in accessing the protocol. Please contact your system administrator.")
  })
  
  return(protocols)
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
  currentValueKindsList <- fromJSON(getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, "valuekinds/")))
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

#' Saves value kinds
#' 
#' Saves value kinds with matching value types
#' @param valueKinds character vector of new valueKinds
#' @param valueTypes character vector of valueTypes (e.g. c("stringValue", "numericValue"))
#' @details valueKinds must be new, and valueTypes must exist
#' @export
saveValueKinds <- function(valueKinds, valueTypes, errorEnv=NULL) {
  valueTypesList <- fromJSON(getURL(paste0(racas::applicationSettings$client.service.persistence.fullpath, "valuetypes")))
  allowedValueTypes <- sapply(valueTypesList, getElement, "typeName")
  
  newValueTypesList <- valueTypesList[match(valueTypes, allowedValueTypes)]
  newValueKindsUpload <- mapply(function(x, y) list(kindName=x, lsType=y), valueKinds, newValueTypesList,
                                SIMPLIFY = F, USE.NAMES = F)
  tryCatch({
    response <- getURL(
      paste0(racas::applicationSettings$client.service.persistence.fullpath, "valuekinds/jsonArray"),
      customrequest='POST',
      httpheader=c('Content-Type'='application/json'),
      postfields=toJSON(newValueKindsUpload))
  }, error = function(e) {
    addError(paste("Internal error in saving new valueKinds:", e$message), errorEnv=errorEnv)
  })
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

#' Updates an entity
#' 
#' Replaces the entity that is at the URL with the one sent. Sub-entities (label, state, value) must have a parent object
#' 
#' @param entity the entity to place
#' @param acasCategory the category (e.g. "experiments", "containervalues")
#' @param lsServerURL the URL of the persistence server
updateAcasEntity <- function(entity, acasCategory, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  response <- getURL(
    paste(lsServerURL, acasCategory, "/", entity$id, sep=""),
    customrequest='PUT',
    httpheader=c('Content-Type'='application/json'),
    postfields=toJSON(entity))
  if (grepl("^<",response)) {
    myLogger <- createLogger(logName="com.acas.sel", logFileName = "racas.log")
    myLogger$error(response)
    stopUser (paste0("Internal Error: The loader was unable to update your ", acasCategory, ". Check the logs at ", Sys.time()))
  }
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
#' @details \code{include} can be in the list: \itemize{ \item{analysisgroups:
#'   returns the experiment stub with analysis group stubs} \item{fullobject:
#'   returns the full experiment object (warning: this may be slow if there is a
#'   lot of data)} \item{prettyjsonstub: returns the experiment stub in pretty
#'   json format} \item{prettyjsons: returns the full experiment in pretty json 
#'   format} \item{analysisgroupvalues: returns the experiment stub with full
#'   analysis groups}} If left blank, an experiment stub (with states and
#'   values) is returned. The codeName will do the same as
#'   include=analysisgroups.
#'   
#' @return the experiment object, or if it does not exist, \code{addError} is
#'   run and NULL is returned
#' 
#' @export
#' 
getExperimentById <- function(experimentId, include=NULL, errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  experiment <- NULL
  if (is.null(include)) {
    include = ""
  } else {
    include = paste0("?with=", include)
  }
  tryCatch({
    experiment <- getURL(paste0(lsServerURL, "experiments/", experimentId, include))
    experiment <- fromJSON(experiment)
  }, error = function(e) {
    addError(paste0("Could not get experiment ", experimentId, " from the server"), errorEnv)
  })
  return(experiment)
}

#' @rdname getExperimentById
#' @export
getExperimentByCodeName <- function(experimentCodeName, include=NULL, errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  experiment <- NULL
  if (is.null(include)) {
    include = ""
  } else {
    include = paste0("?with=", include)
  }
  tryCatch({
    experiments <- getURL(paste0(lsServerURL, "experiments/codename/", experimentCodeName, include))
    experiment <- fromJSON(experiments)[[1]]
  }, error = function(e) {
    addError(paste0("Could not get experiment ", experimentCodeName, " from the server"), errorEnv)
  })
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
  protocol <- NULL
  tryCatch({
    protocol <- getURL(paste0(lsServerURL, "protocols/", id, "?with=", include))
    protocol <- fromJSON(protocol)
  }, error = function(e) {
    addError(paste0("Could not get protocol ", id, " from the server"), errorEnv)
  })
  return(protocol)
}

#' @rdname getProtocolById
#' @export
getProtocolByCodeName <- function(protocolCodeName, include="", errorEnv=NULL, lsServerURL = racas::applicationSettings$client.service.persistence.fullpath) {
  protocol <- NULL
  tryCatch({
    protocols <- getURL(paste0(lsServerURL, "protocols/codename/", protocolCodeName, "?with=", include))
    protocol <- fromJSON(protocols)[[1]]
  }, error = function(e) {
    addError(paste0("Could not get protocol ", protocolCodeName, " from the server"), errorEnv)
  })
  return(protocol)
}

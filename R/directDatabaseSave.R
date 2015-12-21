## R functions to load data directly into the ACAS database

#' Write table with matching columns names
#' 
#' Writes a data.table to a database table like \code{\link{dbWriteTable}}, but
#' matches to the column names. Column names must be all lowercase in your
#' data.table.
#' 
#' @param conn a DBIConnection.
#' @param name A character string specifying a DBMS table name.
#' @param value a data.table to write.
dbWriteTableMatchCol <- function(conn, name, value, ...) {
  fields <- dbListFields(conn, tolower(name))
  # Remove dropped postgres columns and change Oracle columns to lowercase
  fields <- tolower(fields[!grepl("..pg.dropped", fields, fixed = TRUE)])
  setcolorder(value, fields)
  dbWriteTable(conn, name, value, ...)
}
#' @rdname saveEntitiesDD
getEntityIdsDD <- function(conn, numberOfIds) {
	if (getDBType() == "Oracle"){
    return(chunkMillionIds(conn, numberOfIds, getEntityIdsDDInternal))
	} else {
    # No chunking tested for Postgreql yet
		entityIdSql <- paste0("select nextval('thing_pkseq') as id from generate_series(1,", numberOfIds, ")")
		entityIds <- dbGetQuery(conn, entityIdSql)
		return(as.integer(entityIds[,1]))
	}
}
#' @rdname saveEntitiesDD
getEntityIdsDDInternal <- function(conn, numberOfIds) {
  if (getDBType() == "Oracle"){
    entityIdSql <- paste0("select thing_pkseq.nextval as id from dual connect by level <= ", numberOfIds)
  } else {
    entityIdSql <- paste0("select nextval('thing_pkseq') as id from generate_series(1,", numberOfIds, ")")
  }
  
  entityIds <- dbGetQuery(conn, entityIdSql)
  return(as.integer(entityIds[,1]))
}
#' @rdname saveEntitiesDD
chunkMillionIds <- function(conn, numberOfIds, FUN) {
  # applies the FUN to get Ids on sets no greater than 1 million to avoid database 
  # memory issues on Oracle (postgres untested)
  # ORA-30009: Not enough memory for CONNECT BY operation
  limit <- 1000000
  setsOfMillion <- floor(numberOfIds / limit)
  leftOver <- numberOfIds %% limit
  output <- as.integer(as.vector(replicate(setsOfMillion, FUN(conn, limit))))
  if (leftOver > 0) {
    output <- c(output, FUN(conn, leftOver))
  }
  return(as.integer(output))
}
#' @rdname saveEntitiesDD
getLabelIdsDD <- function(conn, numberOfIds) {
  if (getDBType() == "Oracle"){
    # Oracle memory limits us to 1 million id's at a time
    return(chunkMillionIds(conn, numberOfIds, getLabelIdsDDInternal))
  } else {
    labelIdSql <- paste0("select nextval('label_pkseq') as id from generate_series(1,", numberOfIds, ")")
  }
  
  labelIds <- dbGetQuery(conn, labelIdSql)
  return(as.integer(labelIds[,1]))
}
#' @rdname saveEntitiesDD
getLabelIdsDDInternal <- function(conn, numberOfIds) {
  
  if (getDBType() == "Oracle"){
    labelIdSql <- paste0("select label_pkseq.nextval as id from dual connect by level <= ", numberOfIds)
  } else {
    labelIdSql <- paste0("select nextval('label_pkseq') as id from generate_series(1,", numberOfIds, ")")
  }
  
  labelIds <- dbGetQuery(conn, labelIdSql)
  return(as.integer(labelIds[,1]))
}
#' @rdname saveEntitiesDD
getStateIdsDD <- function(conn, numberOfIds) {
  if (getDBType() == "Oracle"){
    # Oracle memory limits us to 1 million id's at a time
    return(chunkMillionIds(conn, numberOfIds, getStateIdsDDInternal))
  } else {
    stateIdSql <- paste0("select nextval('state_pkseq') as id from generate_series(1,", numberOfIds, ")")
  }
  
  stateIds <- dbGetQuery(conn, stateIdSql)
  return(as.integer(stateIds[,1]))
}
#' @rdname saveEntitiesDD
getStateIdsDDInternal <- function(conn, numberOfIds) {
  
	if (getDBType() == "Oracle"){
		stateIdSql <- paste0("select state_pkseq.nextval as id from dual connect by level <= ", numberOfIds)
	} else {
		stateIdSql <- paste0("select nextval('state_pkseq') as id from generate_series(1,", numberOfIds, ")")
	}
	
	stateIds <- dbGetQuery(conn, stateIdSql)
	return(as.integer(stateIds[,1]))
}
#' @rdname saveEntitiesDD
getValueIdsDD <- function(conn, numberOfIds) {
  if (getDBType() == "Oracle"){
    return(chunkMillionIds(conn, numberOfIds, getValueIdsDDInternal))
  } else {
    valueIdSql <- paste0("select nextval('value_pkseq') as id from generate_series(1,", numberOfIds, ")")
  }
  
  valueIds <- dbGetQuery(conn, valueIdSql)
  return(as.integer(valueIds[,1]))
}
#' @rdname saveEntitiesDD
getValueIdsDDInternal <- function(conn, numberOfIds) {
	if (getDBType() == "Oracle"){
		valueIdSql <- paste0("select value_pkseq.nextval as id from dual connect by level <= ", numberOfIds)
	} else {
		valueIdSql <- paste0("select nextval('value_pkseq') as id from generate_series(1,", numberOfIds, ")")
	}
	
	valueIds <- dbGetQuery(conn, valueIdSql)
	return(as.integer(valueIds[,1]))
}
#' @rdname saveEntitiesDD
getEntityCodesBySqlDD <- function(conn, entityType, numberOfCodes) {
  if (numberOfCodes == 0) {
    return(c())
  }
	if (entityType == "ANALYSIS_GROUP"){
		entityPrefix <- "AG-"
		if (getDBType() == "Oracle"){
			entityCodeSql <- paste0("select lsseq_anlgrp_pkseq.nextval as id from dual connect by level <= ", numberOfCodes)
		} else {
			entityCodeSql <- paste0("select nextval('lsseq_anlgrp_pkseq') as id from generate_series(1,", numberOfCodes, ")")
		}
	} else if (entityType == "TREATMENT_GROUP"){
		entityPrefix <- "TG-"
		if (getDBType() == "Oracle"){
			entityCodeSql <- paste0("select lsseq_trtgrp_pkseq.nextval as id from dual connect by level <= ", numberOfCodes)
		} else {
			entityCodeSql <- paste0("select nextval('lsseq_trtgrp_pkseq') as id from generate_series(1,", numberOfCodes, ")")
		}
	} else if (entityType == "SUBJECT"){
		entityPrefix <- "SUBJ-"
		if (getDBType() == "Oracle"){
			entityCodeSql <- paste0("select lsseq_subj_pkseq.nextval as id from dual connect by level <= ", numberOfCodes)
		} else {
			entityCodeSql <- paste0("select nextval('lsseq_subj_pkseq') as id from generate_series(1,", numberOfCodes, ")")
		}
	} else if (entityType == "CONTAINER"){
	  entityPrefix <- "CONT-"
	  if (getDBType() == "Oracle"){
	    entityCodeSql <- paste0("select lsseq_container_pkseq.nextval as id from dual connect by level <= ", numberOfCodes)
	  } else {
	    entityCodeSql <- paste0("select nextval('lsseq_container_pkseq') as id from generate_series(1,", numberOfCodes, ")")
	  }
	} else if (entityType == "ITXCONTCONT") {
    entityPrefix <- "CITX-"
    if (getDBType() == "Oracle"){
      entityCodeSql <- paste0("select lsseq_itxcntrcntr_pkseq.nextval as id from dual connect by level <= ", numberOfCodes)
    } else {
      entityCodeSql <- paste0("select nextval('lsseq_itxcntrcntr_pkseq') as id from generate_series(1,", numberOfCodes, ")")
    }
	}

	entityCodeIds <- dbGetQuery(conn, entityCodeSql)
	entityCodes <- paste0(entityPrefix, entityCodeIds[,1])
	return(entityCodes)
}
#' @rdname saveEntitiesDD
getEntityCodesDD <- function(entityType, numberOfCodes){
	if (entityType == "ANALYSIS_GROUP"){
		entityCodes <- unlist(getAutoLabels(thingTypeAndKind = "document_analysis group", 
		                                                labelTypeAndKind = "id_codeName", 
		                                                numberOfLabels = numberOfCodes))
	} else if (entityType == "TREATMENT_GROUP"){
		entityCodes <- unlist(getAutoLabels(thingTypeAndKind = "document_treatment group", 
		                                                labelTypeAndKind = "id_codeName", 
		                                                numberOfLabels = numberOfCodes))
	} else if (entityType == "SUBJECT"){
		entityCodes <- unlist(getAutoLabels(thingTypeAndKind = "document_subject", 
		                                                labelTypeAndKind = "id_codeName", 
		                                                numberOfLabels = numberOfCodes))
	}

	return(entityCodes)
}

#' Read tsv file for direct database load
#' 
#' Reads tsv data into a data.table for \code{\link{saveDataDirectDatabase}}
#' 
#' @param dataFilePath path to tsv file
readTsvDataFileDD <- function(dataFilePath){
	tsvCols <- c(tempId = "character", tempStateId = "character", stateType = "character", stateKind = "character", 
				codeValue = "character", valueType = "character", valueKind = "character", codeType = "character", 
				codeKind = "character", lsType = "character", lsKind = "character", codeOrigin = "character", 
				recordedBy = "character", lsTransaction="numeric", clobValue = "character", comments = "character",
				parentId="numeric", id = "integer", codeName = "character", tempParentId = "character", publicData="logical")

	tsv_data <- fread(dataFilePath, header=TRUE, sep="\t", colClasses=tsvCols)
	
	return(tsv_data)
}
#' @rdname saveEntitiesDD
saveAgDataDD <- function(conn, inputDT, experimentId, lsTransactionId, recordedDate){
#inputDT <- ag_data
	if(all(is.na(inputDT$parentId))) inputDT[, parentId := experimentId ]
	if(all(is.na(inputDT$lsTransaction))) inputDT[, lsTransaction := lsTransactionId ]

	inputDT[ ,recordedDate := recordedDate ]
	inputDT[ ,ignored := FALSE ]
	inputDT[ ,modifiedBy := as.character("") ]
	if (getDBType() == "Oracle"){
	  inputDT[ ,modifiedDate := as.character("") ] 
	} else {
	  inputDT[ ,modifiedDate := NA ] 			
	}
	inputDT[ ,version := 0 ]
	inputDT[ ,deleted := FALSE ]
	inputDT[ publicData==NA, publicData := FALSE ]
	
	inputDT <- saveEntitiesDD(conn, entityType="ANALYSIS_GROUP", inputDT)
	parentDT <- unique(subset(inputDT, ,c("id", "tempId")))
	setnames(parentDT, "id", "parentId")
	setnames(parentDT, "tempId", "tempParentId")

	inputDT <- saveStatesDD(conn, entityType="ANALYSIS_GROUP", inputDT)
	inputDT <- saveValuesDD(conn, entityType="ANALYSIS_GROUP", inputDT)

	return(parentDT)	
}

#' @rdname saveEntitiesDD
saveTgDataDD <- function(conn, inputDT, ag_ids, lsTransactionId, recordedDate){
#inputDT <- tg_data
#ag_ids <- outputAgDT

	if (("parentId" %in% names(inputDT)) && (all(is.na(inputDT$parentId)))) inputDT[, parentId := NULL ]
	if (all(is.na(inputDT$lsTransaction))) inputDT[, lsTransaction := lsTransactionId ]
	inputDT[ lsType=="", lsType := "default" ]
	inputDT[ lsKind=="", lsKind := "default" ]

	inputDT <- merge(inputDT, ag_ids, by="tempParentId")

	inputDT[ ,recordedDate := recordedDate ]
	inputDT[ ,ignored := FALSE ]
	inputDT[ ,modifiedBy := as.character("") ]
	if (getDBType() == "Oracle"){
	  inputDT[ ,modifiedDate := as.character("") ] 
	} else {
	  inputDT[ ,modifiedDate := NA ] 			
	}
	inputDT[ ,version := 0 ]
	inputDT[ ,deleted := FALSE ]
	inputDT[ publicData==NA, publicData := FALSE ]


	inputDT <- saveEntitiesDD(conn, entityType="TREATMENT_GROUP", inputDT)
	parentDT <- unique(subset(inputDT, ,c("id", "tempId")))
	setnames(parentDT,"id", "parentId")
	setnames(parentDT,"tempId", "tempParentId")

	inputDT <- saveStatesDD(conn, entityType="TREATMENT_GROUP", inputDT)
	inputDT <- saveValuesDD(conn, entityType="TREATMENT_GROUP", inputDT)

	return(parentDT)	
}

#' @rdname saveEntitiesDD
saveSubjectDataDD <- function(conn, inputDT, tg_ids, lsTransactionId, recordedDate){
#inputDT <- subject_data
#tg_ids <- outputTgDT

  if (("parentId" %in% names(inputDT)) && all(is.na(inputDT$parentId))) {
    inputDT[, parentId := NULL ]
  }
  if (all(is.na(inputDT$lsTransaction))) {
    inputDT[, lsTransaction := lsTransactionId ]
  }
  inputDT[ lsType=="", lsType := "default" ]
	inputDT[ lsKind=="", lsKind := "default" ]
	
	inputDT <- merge(inputDT, tg_ids, by="tempParentId")

	inputDT[ ,recordedDate := recordedDate ]
	inputDT[ ,ignored := FALSE ]
	inputDT[ ,modifiedBy := as.character("") ]
	if (getDBType() == "Oracle"){
	  inputDT[ ,modifiedDate := as.character("") ] 
	} else {
	  inputDT[ ,modifiedDate := NA ] 			
	}
	inputDT[ ,version := 0 ]
	inputDT[ ,deleted := FALSE ]
	inputDT[ publicData==NA, publicData := FALSE ]

	inputDT <- saveEntitiesDD(conn, entityType="SUBJECT", inputDT)
	parentDT <- unique(subset(inputDT, ,c("id", "tempId")))
	setnames(parentDT,"id", "parentId")
	setnames(parentDT,"tempId", "tempParentId")


	inputDT <- saveStatesDD(conn, entityType="SUBJECT", inputDT)
	inputDT <- saveValuesDD(conn, entityType="SUBJECT", inputDT)

	return(parentDT)	
}

#' Helper functions for saving direct to database
#' 
#' A set of functions used internally, expected to be used by \code{\link{saveDataDirectDatabase}}. Could be expanded later.
#' 
#' @param conn database connection object
#' @param entityType "ANALYSIS_GROUP", "TREATMENT_GROUP", or "SUBJECT"
#' @param inputDT a data.table input. See source for required columns.
#' @param inputStatesDT a data.table input. See source for required columns.
#' @param numberOfIds number of ids to return
#' @param numberOfCodes number of codes to return
#' @param experimentId id of experiment
#' @param lsTransactionId integer of ls transaction id
#' @param recordedDate date of save, correctly string formatted for database
#' @param ag_ids data.table of analysis_group ids with columns tempParentId and id
#' @param tg_ids data.table of treatment_group ids with columns tempParentId and id
#' @param FUN function to apply repeatedly
saveEntitiesDD <- function( conn, entityType, inputDT ){
#entityType <- "ANALYSIS_GROUP"
  
  if (!"parentId" %in% names(inputDT)) {
    inputDT[ , parentId := NA]
  }

  if(entityType == "CONTAINER") {
    entities <- unique(inputDT[, list(tempId, id, codeName, lsKind, lsTransaction, lsType, parentId, recordedBy,
                                      ignored, modifiedBy, modifiedDate, recordedDate, version, locationId, deleted)])
  } else if (entityType == "ITXCONTCONT") {
    entities <- unique(inputDT[, list(tempId, id, codeName, lsKind, lsTransaction, lsType, parentId, recordedBy,
                                      ignored, modifiedBy, modifiedDate, recordedDate, version, firstContainer, secondContainer, deleted)])
  } else {
    entities <- unique(inputDT[, list(tempId, id, codeName, lsKind, lsTransaction, lsType, parentId, recordedBy,
                                      ignored, modifiedBy, modifiedDate, recordedDate, version, deleted)])
  }

	entities[ ,lsTypeAndKind := paste0(lsType, "_", lsKind)]
	entities[ is.na(id), id := getEntityIdsDD(conn, .N)]
	entities[ is.na(codeName) | codeName=="", codeName := getEntityCodesBySqlDD(conn, entityType, length(codeName))]
	merge_ids <- subset(entities, ,c("id", "tempId"))
	setkey(merge_ids, "tempId")
	setkey(inputDT, "tempId")
	if("id" %in% names(inputDT)) {
    inputDT[, id := NULL ]
	}
	inputDT <- inputDT[merge_ids]
	if("tempId" %in% names(entities)) {
	  entities[, tempId := NULL ]
	}
	if("parentId" %in% names(entities)) {
    entities[, parentId := NULL ]
	}
  
  ### Create many-to-many table
  childParentDT <- unique(subset(inputDT, , c("id", "parentId")))
  setkey(childParentDT, "id")
  setkey(inputDT, "id")
  
  ### Setup for saving to database
  setnames(entities, 
           c("id", "codeName", "ignored", "lsKind", "lsTransaction",
             "lsType", "lsTypeAndKind", "modifiedBy", "modifiedDate", 
             "recordedBy","recordedDate", "version", "deleted"),
           c("id", "code_name", "ignored", "ls_kind", "ls_transaction", 
             "ls_type", "ls_type_and_kind", "modified_by", "modified_date", 
             "recorded_by", "recorded_date", "version", "deleted"))
  
  if (entityType == "CONTAINER") {
    setnames(entities, 
             c("locationId"),
             c("location_id"))
  } else if (entityType == "ITXCONTCONT") {
    setnames(entities,
            c("firstContainer"    , "secondContainer"),
            c("first_container_id", "second_container_id"))
  }
  
  entityTable <- switch(entityType,
                        "ANALYSIS_GROUP" = "ANALYSIS_GROUP",
                        "TREATMENT_GROUP" = "TREATMENT_GROUP",
                        "SUBJECT" = "SUBJECT",
                        "CONTAINER" = "CONTAINER",
                        "ITXCONTCONT" = "ITX_CONTAINER_CONTAINER",
                        stop("Unknown Entity Type"))
  
  joinTable <- switch(entityType,
                      "ANALYSIS_GROUP" = "EXPERIMENT_ANALYSISGROUP",
                      "TREATMENT_GROUP" = "ANALYSISGROUP_TREATMENTGROUP",
                      "SUBJECT" = "TREATMENTGROUP_SUBJECT",
                      "CONTAINER" = NA,
                      "ITXCONTCONT" = NA,
                      stop("Unknown Entity Type"))
  
	if (!getDBType() == "Oracle") {
	  entityTable <- tolower(entityTable)
	  joinTable <- tolower(joinTable)
	}
  
  ### Save entity table
  dbWriteTableMatchCol(conn, entityTable, entities, append = T, row.names=FALSE, col.names=FALSE)
  
  if (is.na(joinTable)) {
    return(inputDT)
  }
  
  ### Save join table
	if (entityType == "ANALYSIS_GROUP") {
	  setnames(childParentDT, c("id", "parentId"), c("analysis_group_id", "experiment_id"))
	} else if (entityType == "TREATMENT_GROUP") {
	  setnames(childParentDT, c("id", "parentId"), c("treatment_group_id", "analysis_group_id"))
	} else if (entityType == "SUBJECT") {
	  setnames(childParentDT, c("id", "parentId"), c("subject_id", "treatment_group_id"))
	}

	dbWriteTableMatchCol(conn, joinTable, childParentDT, append = T, row.names=FALSE, col.names=FALSE)

	return(inputDT)
}
#' @rdname saveEntitiesDD
saveStatesDD <- function( conn, entityType, inputStatesDT ){
  stateColumns <- c("tempStateId", "stateId", "stateType", "stateKind", 
                    "lsTransaction", "id", "recordedBy", "ignored", "modifiedBy", 
                    "modifiedDate", "recordedDate", "version", "deleted", "comments")
  setkeyv(inputStatesDT, stateColumns) #Set key to all used colums
  states <- unique(inputStatesDT[!is.na(inputStatesDT$tempStateId), stateColumns, with=FALSE])
  if (nrow(states) == 0) {
    return(inputStatesDT)
  }
  setkey(states, "id")
  numberOfIds <- length(unique(states$tempStateId))
  states[ , stateId := getStateIdsDD(conn, numberOfIds)]
  states[ , lsTypeAndKind := paste0(stateType, "_", stateKind)]
  if("stateId" %in% names(inputStatesDT)) {
    inputStatesDT[ , stateId := NULL ]
  }
  merge_ids <- unique(states[, c("stateId", "tempStateId"), with=FALSE])
  setkey(inputStatesDT, "tempStateId")
  setkey(merge_ids, "tempStateId")
  inputStatesDT <- merge(inputStatesDT, merge_ids, by="tempStateId")
  if("tempStateId" %in% names(states)) {
    states[ , tempStateId := NULL ]
  }
  setnames(states, 
           c("stateId", "comments", "ignored", "stateKind", "lsTransaction", "stateType", 
             "lsTypeAndKind", "modifiedBy", "modifiedDate", "recordedBy", 
             "recordedDate", "version", "id", "deleted"),
           c("id", "comments", "ignored", "ls_kind", "ls_transaction", "ls_type", 
             "ls_type_and_kind", "modified_by", "modified_date", "recorded_by", 
             "recorded_date", "version", "entityId", "deleted"))
  
  #   setcolorder(states, c("stateId", "comments", "ignored", "stateKind","lsTransaction","stateType","lsTypeAndKind","modifiedBy", 
  #                            	"modifiedDate","recordedBy","recordedDate", "version", "id", "deleted"))

	if (entityType == "ANALYSIS_GROUP"){
		stateTable <- "ANALYSIS_GROUP_STATE"
		setnames(states, "entityId", "analysis_group_id")
	} else if (entityType == "TREATMENT_GROUP"){
		stateTable <- "TREATMENT_GROUP_STATE"
		setnames(states, "entityId", "treatment_group_id")
	} else if (entityType == "SUBJECT"){
		stateTable <- "SUBJECT_STATE"
		setnames(states, "entityId", "subject_id")
	} else if (entityType == "CONTAINER") {
    stateTable <- "CONTAINER_STATE"
    setnames(states, "entityId", "container_id")
	}
  
  if (!getDBType() == "Oracle") {
    stateTable <- tolower(stateTable)
  }
  
	dbWriteTableMatchCol(conn, stateTable, states, append = T, row.names=FALSE, col.names=FALSE)
  
	return(inputStatesDT)
}

#' Save values direct database
#' 
#' This function was used for a migration from flat files, and could be used for
#' others later. This would then be reworked to use
#' \code{\link{saveDataDirectDatabase}} and \code{\link{readTsvDataFileDD}}.
saveValuesDD <- function( conn, entityType, inputDT ){
  
  # Helper function for types and kinds
  nullTextIfNa <- function(x) {
    ifelse(is.na(x), "null", x)
  }
  
  # Assign tempValueId's for rows that have value data, defined by having a valueKind
  if(all(is.na(inputDT$tempValueId))) {
    inputDT[!is.na(valueKind), tempValueId := seq(1, sum(!is.na(valueKind))) ]
  }
  
  valueColumns <- c(
    "clobValue", "codeKind", "codeOrigin", "codeType", "codeValue", "comments",
    "concentration", "concUnit", "dateValue", "fileValue", "lsTransaction",
    "numberOfReplicates", "numericValue", "operatorKind", "operatorType", "publicData",
    "recordedBy", "sigFigs", "stateId", "stringValue", "tempValueId", "uncertainty",
    "uncertaintyType", "unitKind", "unitType", "ignored", "modifiedBy", "modifiedDate",
    "recordedDate", "version", "deleted", "urlValue", "valueKind", "valueType")
  setkeyv(inputDT, valueColumns) #Set key to all used colums
  values <- unique(inputDT[!is.na(inputDT$tempValueId), valueColumns, with=FALSE])
  
  if (nrow(values) == 0) {
    return(inputDT)
  }
	
	numberOfIds <- length(unique(values$tempValueId))
	values[, valueId := getValueIdsDD(conn, numberOfIds)]
	values[, valueTypeAndKind := paste0(nullTextIfNa(valueType), "_", nullTextIfNa(valueKind))]
	values[, operatorTypeAndKind := paste0(nullTextIfNa(operatorType), "_", nullTextIfNa(operatorKind))]
	values[, unitTypeAndKind := paste0(nullTextIfNa(unitType), "_", nullTextIfNa(unitKind))]
	values[, codeTypeAndKind := paste0(nullTextIfNa(codeType), "_", nullTextIfNa(codeKind))]
	
	if (getDBType() == "Oracle"){
		values[, blobValue := list("")]
	} else {
		values[, blobValue := NA]
	}
	
	merge_ids <- unique(subset(values, ,c("valueId", "tempValueId")))
  if("valueId" %in% names(inputDT)) {
    inputDT[, valueId := NULL ]
  }
  inputDT <- merge(inputDT, merge_ids, by="tempValueId")
  values$numericValue <- as.numeric(values$numericValue)
  if("tempValueId" %in% names(values)) {
    values[, tempValueId := NULL ]
  }
	values$dateValue <- as.POSIXct(values$dateValue)
	
	setnames(values, 
	         c("valueId", "blobValue", "clobValue", "codeKind", "codeType", 
	           "codeTypeAndKind", "codeValue", "comments", "dateValue", "fileValue", 
	           "ignored", "valueKind", "lsTransaction", "valueType", "valueTypeAndKind", 
	           "modifiedBy", "modifiedDate", "numberOfReplicates", "numericValue", 
	           "operatorKind", "operatorType", "operatorTypeAndKind", "publicData", 
	           "recordedBy", "recordedDate", "sigFigs", "stringValue", "uncertainty", 
	           "uncertaintyType", "unitKind", "unitType", "unitTypeAndKind", 
	           "urlValue", "version", "codeOrigin", "deleted", 
             "concUnit", "concentration"), 
	         c("id", "blob_value", "clob_value", "code_kind", "code_type", 
	           "code_type_and_kind", "code_value", "comments", "date_value", "file_value", 
	           "ignored", "ls_kind", "ls_transaction", "ls_type", "ls_type_and_kind", 
             "modified_by", "modified_date", "number_of_replicates", "numeric_value", 
             "operator_kind", "operator_type", "operator_type_and_kind", "public_data", 
             "recorded_by", "recorded_date", "sig_figs", "string_value", "uncertainty", 
             "uncertainty_type", "unit_kind", "unit_type", "unit_type_and_kind", 
             "url_value", "version", "code_origin", "deleted", 
             "conc_unit", "concentration"))
  
	if (entityType == "ANALYSIS_GROUP") {
		valueTable <- "ANALYSIS_GROUP_VALUE"
		setnames(values, "stateId", "analysis_state_id")
	} else if (entityType == "TREATMENT_GROUP") {
		valueTable <- "TREATMENT_GROUP_VALUE"
		setnames(values, "stateId", "treatment_state_id")
	} else if (entityType == "SUBJECT") {
		valueTable <- "SUBJECT_VALUE"
		setnames(values, "stateId", "subject_state_id")
	} else if (entityType == "CONTAINER") {
    valueTable <- "CONTAINER_VALUE"
    setnames(values, "stateId", "container_state_id")
	}
	
	if (!getDBType() == "Oracle") {
	  valueTable <- tolower(valueTable)
	}
  
	dbWriteTableMatchCol(conn, valueTable, values, append = T, row.names=FALSE)
	
	return(inputDT)

}

#' @rdname saveEntitiesDD
saveLabelsDD <- function( conn, entityType, inputLabelsDT ){
  labelColumns <- c("labelType", "labelKind", "labelValue", "imageFile", "physicallyLabled", "preferred",
                    "lsTransaction", "id", "recordedBy", "ignored", 
                    "modifiedDate", "recordedDate", "version", "deleted")
  labels <- unique(inputLabelsDT[ , labelColumns, with=FALSE])
  setkey(labels, "id")
  numberOfIds <- nrow(labels)
  labels[ , labelId := getLabelIdsDD(conn, numberOfIds)]
  labels[ , lsTypeAndKind := paste0(labelType, "_", labelKind)]
  if("labelId" %in% names(inputLabelsDT)) {
    inputLabelsDT[ , labelId := NULL ]
  }
  setnames(labels, 
           c("labelId", "ignored", "labelKind", "lsTransaction", "labelType", 
             "lsTypeAndKind", "modifiedDate", "recordedBy", 
             "recordedDate", "version", "id", "deleted", 
             "imageFile", "labelValue", "physicallyLabled", "preferred"),
           c("id", "ignored", "ls_kind", "ls_transaction", "ls_type", 
             "ls_type_and_kind", "modified_date", "recorded_by", 
             "recorded_date", "version", "entityId", "deleted", 
             "image_file", "label_text", "physically_labled", "preferred"))
  
  if (entityType == "ANALYSIS_GROUP"){
    labelTable <- "ANALYSIS_GROUP_LABEL"
    setnames(labels, "entityId", "analysis_group_id")
  } else if (entityType == "TREATMENT_GROUP"){
    labelTable <- "TREATMENT_GROUP_LABEL"
    setnames(labels, "entityId", "treatment_group_id")
  } else if (entityType == "SUBJECT"){
    labelTable <- "SUBJECT_LABEL"
    setnames(labels, "entityId", "subject_id")
  } else if (entityType == "CONTAINER") {
    labelTable <- "CONTAINER_LABEL"
    setnames(labels, "entityId", "container_id")
  }
  
  if (!getDBType() == "Oracle") {
    labelTable <- tolower(labelTable)
  }
  
  dbWriteTableMatchCol(conn, labelTable, labels, append = T, row.names=FALSE, col.names=FALSE)
  
  return(inputLabelsDT)
}

#' Save from tsv
#' 
#' This function was used for a migration from flat files, and could be used for
#' others later. This would then be reworked to use
#' \code{\link{saveDataDirectDatabase}} and \code{\link{readTsvDataFileDD}}.
saveTsvData <- function(experimentId, lsTransactionId, agDatafile, tgDataFile, subjectDataFile){	
  errorFLAG <- FALSE
	tryCatch({
		
		conn <- getDatabaseConnection(racas::applicationSettings)

		if (getDBType() == "Oracle"){
			sqlDeferConstraints <- "SET CONSTRAINTS ALL DEFERRED"
			rs1 <- dbSendQuery(conn, sqlDeferConstraints)
			Sys.setenv(ORA_SDTZ = "PST8PDT")
			Sys.setenv(TZ = "PST8PDT")	
			recordedDate <- Sys.time()
		} else { 
			dbSendQuery(conn, "BEGIN TRANSACTION")	
			recordedDate <- as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%OS"))
		}
		
		ag_data <- readTsvDataFileDD(agDatafile)
		outputAgDT <- saveAgDataDD(conn, ag_data, experimentId, lsTransactionId, recordedDate)
		rm(ag_data)
		gc()

		tg_data <- readTsvDataFileDD(tgDataFile)
		outputTgDT <- saveTgDataDD(conn, tg_data, outputAgDT, lsTransactionId, recordedDate)
		rm(tg_data)
		rm(outputAgDT)
		gc()

		subject_data <- readTsvDataFileDD(subjectDataFile)
		outputSubjectDT <- saveSubjectDataDD(conn, subject_data, outputTgDT, lsTransactionId, recordedDate)
		rm(subject_data)
		rm(outputTgDT)
		rm(outputSubjectDT)
		gc()

	},
		warning = function(wrn){
		warning(paste0("Warning message ", wrn))
		errorFLAG <<- TRUE
	},
		error = function(ex) {
		warning(paste0("Error message ", ex))
		errorFLAG <<- TRUE
	})

	if(errorFLAG){
		warning(paste0("Rolling back transaction for entire tsv save"))
		dbRollback(conn)
	} else{
		dbCommit(conn)
	}	

	dbDisconnect(conn)

}

#' Save data tables direct to database
#' 
#' Saves whole sets of data for an experiment to the database, and rolls back 
#' the transaction if anything fails (or deletes on Oracle).
#' 
#' @details This does not work through any roo service, so it will be faster,
#'   but has fewer integrity checks along the way. Use with caution. For 
#'   required columns, see example with \code{data(agData)} or look in code of 
#'   \code{\link{prepareTableForDD}}. If you need to do updates as well as 
#'   saves, use \code{\link{saveAllViaTsv}}. The experimentId and
#'   lsTransactionId will fill in if they are missing in the input data tables.
#' 
#' @param agData data.frame or data.table of analysis group data
#' @param tgData data.frame or data.table of treatment group data
#' @param subjectData data.frame or data.table of subject data
#' @param lsTransactionId integer id of transaction
#' @param experimentId integer id of experiment
saveDataDirectDatabase <- function(agData, tgData, subjectData, lsTransactionId = NA, experimentId = NULL) {
  if (is.na(lsTransactionId)) {
    if (is.null(agData$lsTransaction)) {
      stop("If lsTransactionId is NA, lsTransaction must be defined in input data tables")
    } else {
      lsTransactionId <- unique(agData$lsTransaction)
      if (length(lsTransactionId) > 1) {
        stop("multiple lsTransaction's found in agData")
      }
      if (is.na(lsTransactionId)) {
        stop("lsTransactionId cannot be NA when all lsTransaction in agData are NA")
      }
    }
  }
  
  conn <- getDatabaseConnection(racas::applicationSettings)
  on.exit(dbDisconnect(conn))
  result <- tryCatchLog({
    if (getDBType() == "Oracle"){
      sqlDeferConstraints <- "SET CONSTRAINTS ALL DEFERRED"
      rs1 <- dbSendQuery(conn, sqlDeferConstraints)
      Sys.setenv(ORA_SDTZ = "PST8PDT")
      Sys.setenv(TZ = "PST8PDT")  
      recordedDate <- Sys.time()
    } else { 
      dbSendQuery(conn, "BEGIN TRANSACTION")	
      recordedDate <- as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%OS"))
    }
    
    # Saving each set. The garbage collection may be unnecessary, but won't hurt
    if (!is.null(agData)) {
      agData2 <- prepareTableForDD(agData)
      outputAgDT <- saveAgDataDD(conn, agData2, experimentId, lsTransactionId, recordedDate)
      rm(agData2)
      gc()
    }
    
    if (!is.null(tgData)) {
      tgData2 <- prepareTableForDD(tgData)
      outputTgDT <- saveTgDataDD(conn, tgData2, outputAgDT, lsTransactionId, recordedDate)
      rm(tgData2)
      rm(outputAgDT)
      gc()
    }
    
    if (!is.null(subjectData)) {
      subjectData2 <- prepareTableForDD(subjectData)
      outputSubjectDT <- saveSubjectDataDD(conn, subjectData2, outputTgDT, lsTransactionId, recordedDate)
      rm(subjectData2)
      rm(outputTgDT)
      rm(outputSubjectDT)
      gc()
    }
    TRUE
  })
  
  # If anything fails, roll the transaction back
  if (is.null(result) || is.null(result$value)){
    dbRollback(conn)
    if (getDBType() == "Oracle"){
      # On Oracle, delete everything saved in this transaction
      limitQuery <-  paste("where ls_transaction =", lsTransactionId)
      dbSendQuery(conn, paste("delete from subject_value", limitQuery))
      dbSendQuery(conn, paste("delete from subject_state", limitQuery))
      dbSendQuery(conn, paste("delete from treatment_group_value", limitQuery))
      dbSendQuery(conn, paste("delete from treatment_group_state", limitQuery))
      dbSendQuery(conn, paste("delete from analysis_group_value", limitQuery))
      dbSendQuery(conn, paste("delete from analysis_group_state", limitQuery))
      dbSendQuery(conn, paste("delete from treatmentgroup_subject where treatment_group_id in ", 
                              "(select id from treatment_group", limitQuery, ")"))
      dbSendQuery(conn, paste("delete from analysisgroup_treatmentgroup where analysis_group_id in ", 
                              "(select id from analysis_group", limitQuery, ")"))
      dbSendQuery(conn, paste("delete from subject", limitQuery))
      dbSendQuery(conn, paste("delete from treatment_group", limitQuery))
      dbSendQuery(conn, paste("delete from experiment_analysisgroup where analysis_group_id in ", 
                              "(select id from analysis_group", limitQuery, ")"))
      dbSendQuery(conn, paste("delete from analysis_group", limitQuery))
      dbCommit(conn)
    }
    stop("direct database save failed")
  } else {
    dbCommit(conn)
  }
}

#' Direct Database preparation
#' 
#' Prepares a data.frame or data.table for saving with
#' \code{\link{saveAgDataDD}} or similar. Coerces some columns to the correct
#' class and fills missing columns with NA.
#' Date class is not currently being validated
#' 
#' @param entityData a data.frame or data.table, see source for columns.
#' 
#' @return A data.table corrrectly formatted.
prepareTableForDD <- function(entityData) {
  #   tsvCols <- c(
  #     tempId = "character", tempStateId = "character", stateType = "character", stateKind = "character", 
  #     codeValue = "character", valueType = "character", valueKind = "character", codeType = "character", 
  #     codeKind = "character", lsType = "character", lsKind = "character", codeOrigin = "character", 
  #     recordedBy = "character", lsTransaction="numeric", clobValue = "character", comments = "character",
  #     parentId="numeric", id = "integer", codeName = "character", tempParentId = "character", 
  #     publicData="logical")
  
  entityDataFormatted <- data.table(
    tempValueId = NA_integer_,
    valueType = naIfNull(entityData$valueType),
    valueKind = naIfNull(entityData$valueKind),
    numericValue = naIfNull(entityData$numericValue),
    sigFigs = naIfNull(entityData$sigFigs),
    uncertainty = naIfNull(entityData$uncertainty),
    numberOfReplicates = naIfNull(entityData$numberOfReplicates),
    uncertaintyType = naIfNull(entityData$uncertaintyType),
    stringValue = naIfNull(entityData$stringValue),
    dateValue = naIfNull(entityData$dateValue),
    clobValue = naIfNull(entityData$clobValue),
    urlValue = naIfNull(entityData$urlValue),
    fileValue = naIfNull(entityData$fileValue),
    codeOrigin = naIfNull(entityData$codeOrigin),
    codeType = naIfNull(entityData$codeType),
    codeKind = naIfNull(entityData$codeKind),
    codeValue = naIfNull(entityData$codeValue),
    concentration = naIfNull(entityData$concentration),
    concUnit = naIfNull(entityData$concUnit),
    unitType = NA_character_,
    unitKind = naIfNull(entityData$unitKind),
    operatorType = NA_character_,
    operatorKind = naIfNull(entityData$operatorKind),
    publicData = as.logical(entityData$publicData),
    comments = naIfNull(entityData$comments),
    stateType = naIfNull(entityData$stateType),
    stateKind = naIfNull(entityData$stateKind),
    tempStateId = naIfNull(entityData$tempStateId),
    stateId = NA_integer_,
    id = NA_integer_,
    tempId = entityData$tempId,
    parentId = naIfNull(entityData$parentId),
    tempParentId = naIfNull(entityData$tempParentId),
    lsTransaction = as.numeric(naIfNull(entityData$lsTransaction)),
    recordedBy = naIfNull(entityData$recordedBy),
    codeName = naIfNull(entityData$codeName, NA_character_),
    lsType = entityData$lsType,
    lsKind = entityData$lsKind,
    locationId = naIfNull(entityData$locationId),
    firstContainer = naIfNull(entityData$firstContainer),
    secondContainer = naIfNull(entityData$secondContainer),
    labelType = naIfNull(entityData$labelType, NA_character_),
    labelKind = naIfNull(entityData$labelKind, NA_character_),
    labelValue = naIfNull(entityData$labelValue, NA_character_),
    imageFile = naIfNull(entityData$imageFile, NA_character_),
    physicallyLabled = naIfNull(entityData$imageFile, NA_integer_),
    preferred = naIfNull(entityData$preferred, NA_integer_)
  )
  return(entityDataFormatted)
}
#' Save data frames using direct database load
#' 
#' Saves data frames all in one transaction
#' 
#' @param analysisGroupData A data frame of analysis group values
#' @param treatmentGroupData A data frame of treatment group values
#'@param subjectData A data frame of subject values
#'@param appendCodeName A list with entries "analysisGroupData", 
#'  "treatmentGroupData", "subjectData", each has a vector of valuekinds that 
#'  should have the code name appended to the front
#'  
#'@details each of the data frames must have these columns: unitKind, valueType,
#'  valueKind, numericValue, publicData, stateType, stateKind, tempStateId,
#'  tempId, lsType, lsKind. Other optional columns can be found in the source code for
#'  \code{\link{formatEntityAsTsvAndUpload}}. You can get updating and more 
#'  checks using \code{\link{saveAllViaTsv}}, but it will be slower.
#'@export
saveAllViaDirectDatabase <- function(analysisGroupData, treatmentGroupData, subjectData, appendCodeName = list()) {
  
  sendFiles <- list()
  
  if (!(is.null(appendCodeName$analysisGroup))) {
    analysisGroupData <- appendCodeNames(analysisGroupData, appendCodeName$analysisGroup, "analysis group")
  }
  if (!(is.null(appendCodeName$treatmentGroup))) {
    treatmentGroupData <- appendCodeNames(treatmentGroupData, appendCodeName$treatmentGroup, "treatment group")
  }
  if (!(is.null(appendCodeName$subject))) {
    subjectData <- appendCodeNames(subjectData, appendCodeName$subject, "subject")
  }
  
  response <- saveDataDirectDatabase(as.data.table(analysisGroupData), 
                                     as.data.table(treatmentGroupData), 
                                     as.data.table(subjectData))
  return(response)
}

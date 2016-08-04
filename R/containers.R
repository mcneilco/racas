containerRemappingRequest <- list(
  "containerRemaps" = list(
    "containerRemap" = list("originContainerCode" = "C1129952",
                            "destinationContainerCode" = "E0017158",
                            "direction" = "down",
                            "quadrant" = 1,
                            "user" = "mperez"),
    "containerRemap" = list("originContainerCode" = "C1129952",
                            "destinationContainerCode" = "E0017159",
                            "direction" = "down",
                            "quadrant" = 2,
                            "user" = "mperez"),
    "containerRemap" = list("originContainerCode" = "C1129952",
                            "destinationContainerCode" = "E0017160",
                            "direction" = "down",
                            "quadrant" = 3,
                            "user" = "mperez"),
    "containerRemap" = list("originContainerCode" = "C1129952",
                            "destinationContainerCode" = "E0017161",
                            "direction" = "down",
                            "quadrant" = 4,
                            "user" = "mperez")
  ),
  dryRun = "true"
)
# containerRemappingRequestJSON <- toJSON(containerRemappingRequest)
# containerRemapJSON <- toJSON(list(originContainerCode = "C1113036",
#                                   direction = "up",
#                                   destinationContainerCode = "E0017108",
#                                   quadrant = 1)
# )

ContainerRemap <- setRefClass("ContainerRemap", 
                              fields = list(originContainerCode = "character",
                                            destinationContainerCode = "character",
                                            direction = "character",
                                            user = "character",
                                            quadrant = "integer",
                                            originContainerData = "data.table"),
                              methods = list(
                                initialize = function(..., json = NA) {
                                  callSuper(...)
                                  if(quadrant > 4 | quadrant < 1) stopUser("quadrant must be between 1-4")
                                  if(originContainerCode == "") stopUser("Origin container code cannot be blank")
                                  if(destinationContainerCode == "") stopUser("Destination container code cannot be blank")
                                  if(!direction %in% c("up", "down")) stopUser("direction must be up or down")
                                },
                                fromJSON = function(json) {
                                  containerRemapList <- rjson::fromJSON(json)
                                  originContainerCode <<- containerRemapList$originContainerCode
                                  destinationContainerCode <<- containerRemapList$destinationContainerCode
                                  direction <<- containerRemapList$direction
                                  user <<- containerRemapList$user
                                  quadrant <<- as.integer(containerRemapList$quadrant)
                                  
                                  return(.self)
                                },
                                fetchOriginContainerData = function() {
                                  #TODO implement acas schema call here to get plate data
                                },
                                fetchExternalOriginContainerData = function(func) {
                                  originContainerDat <- func(originContainerCode)
                                  if(nrow(originContainerDat) == 0) {
                                    warnUser("Call to external container data function returned 0 rows")
                                  } else {
                                    originContainerData <<- as.data.table(originContainerDat)
                                    return(originContainerData)
                                  }
                                },
                                remap = function() {
                                  if(is.null(originContainerData)) {return(NULL)}
                                  if(originContainerData$platesize == 1536 && direction == "up") {warnUser("Sorry cannot remap to plate size larger than 1536"); return(NULL)}
                                  originContainerData[,
                                                      c("quadrant","destinationWellRef") := resizeContainerRowColumns(paste0("R",formatC(wellrow,width=3, format="d", flag="0"),"_C",formatC(wellcol,width=3, format="d", flag="0")),
                                                                                                                     originContainerSize = as.integer(unique(platesize)),
                                                                                                                     direction = direction,
                                                                                                                     destinationQuadrant = quadrant
                                                                                                                     
                                                      ),
                                                      by = containercode]
                                  originContainerData[,c("wellrow") := as.integer(substr(destinationWellRef,2,4))]
                                  originContainerData[,c("wellcol") := as.integer(substr(destinationWellRef,7,9))]
                                  
                                  remapDataFrame <- data.frame(toDataTable())
                                  destinationContainerData <- data.table(merge(data.frame(originContainerData), remapDataFrame, by.y = c("originContainerCode", "quadrant"), by.x = c("containercode", "quadrant"), all = TRUE))
                                  
                                  destinationContainerData[, direction := NULL]
                                  destinationContainerData[, platesize := destinationContainerData$platesize/(switch(direction, up = 1/4, down = 4))]
                                  destinationContainerData[, containercode := destinationContainerCode]
                                  destinationContainerData[, destinationContainerCode := NULL]
                                  destinationContainerData[, destinationWellRef := NULL]
                                  return(destinationContainerData)
                                },
                                toJSON = function() {
                                  return(rjson::toJSON(list(originContainerCode = originContainerCode,
                                                            destinationContainerCode = destinationContainerCode,
                                                            direction = direction,
                                                            user = user,
                                                            quadrant = quadrant)))
                                },
                                toList = function() {
                                  return(list(originContainerCode = originContainerCode,
                                              destinationContainerCode = destinationContainerCode,
                                              direction = direction,
                                              user = user,
                                              quadrant = quadrant))
                                },
                                toDataTable = function(){
                                  return(data.table(originContainerCode = originContainerCode,
                                                    destinationContainerCode = destinationContainerCode,
                                                    direction = direction,
                                                    user = user,
                                                    quadrant = quadrant))
                                }
                                
                              )
)
setMethod ("as.list", signature="ContainerRemap", definition= function (x, ...) x$toList() )
setMethod ("toJSON", signature="ContainerRemap", definition= function (x) x$toJSON() )
setMethod ("as.data.table", signature="ContainerRemap", definition= function (x) x$toDataTable() )

ContainerRemaps <- setRefClass("ContainerRemaps",
                               fields = list(containerRemaps = "list",
                                             dryRun = "logical"),
                               methods = list(
                                 fromJSON = function(json) {
                                   containerRemapsList <- rjson::fromJSON(json)
                                   user <- containerRemapsList$user
                                   direction <- containerRemapsList$direction
                                   containerRemaps <<- lapply(containerRemapsList$containerRemaps, function(x, user, direction) ContainerRemap$new(originContainerCode = x$originContainerCode,
                                                                                                                                  destinationContainerCode = x$destinationContainerCode,
                                                                                                                                  direction = ifelse(is.null(x$direction), direction, x$direction),
                                                                                                                                  user = ifelse(is.null(x$user), user, x$user),
                                                                                                                                  quadrant = as.integer(x$quadrant)
                                                                                                                                        ),
                                                              direction = direction,
                                                              user = user
                                   )
                                   dryRun <<- as.logical(containerRemapsList$dryRun)
                                   
                                   return(.self)
                                   
                                 },
                                 toJSON = function() {
                                   return(rjson::toJSON(list(containerRemaps = lapply(containerRemaps, as.list),
                                                             dryRun = dryRun)))
                                   
                                 },
                                 getOriginContainerCodes = function() {
                                   return(as.character((lapply(containerRemaps, function(x) x$originContainerCode))))
                                 },
                                 fetchExternalOriginContainerData = function(func) {
                                   lapply(containerRemaps, function(x) x$fetchExternalOriginContainerData(func))
                                 },
                                 remap = function() {
                                   remaps <- lapply(containerRemaps, function(x) x$remap())
                                   return(rbindlist(remaps))
                                 }
                                 
                               )
)
setMethod ("as.data.table", signature="ContainerRemaps", definition= function (x) x$toDataTable() )

resizeContainerRowColumns <- function(originRowColumns = c("R001_C001","R001_C002"), originContainerSize, direction, destinationQuadrant = NULL) {
  originContainerColumns <- sqrt(originContainerSize/(2/3))
  originContainerRows <- sqrt(originContainerSize*2/3)
  destinationContainerSize <- originContainerSize/switch(direction, up = 1/4, down = 4)
  destinationContainerColumns <- sqrt(destinationContainerSize/(2/3))
  destinationContainerRows <- sqrt(destinationContainerSize*2/3)  
  rows <- switch(direction, up = destinationContainerRows, down = originContainerRows)
  columns <- switch(direction, up = destinationContainerColumns, down = originContainerColumns)
  size <- switch(direction, up = originContainerSize, down = destinationContainerSize)
  destinationquadrant1 <- matrix(paste(rep("Q1_",size), matrix("R",rows,columns), matrix(formatC(1:rows,width=3, format="d", flag="0"),rows,columns, byrow=F), matrix("_C",rows,columns), matrix(formatC(1:columns,width=3, format="d", flag="0"),destinationContainerRows,destinationContainerColumns, byrow=T), sep=""),rows,columns, dimnames = list(paste("R", 1:rows, sep=""), paste("C", 1:columns, sep="")))
  destinationquadrant2 <- matrix(paste(rep("Q2_",size), matrix("R",rows,columns), matrix(formatC(1:rows,width=3, format="d", flag="0"),rows,columns, byrow=F), matrix("_C",rows,columns), matrix(formatC(1:columns,width=3, format="d", flag="0"),destinationContainerRows,destinationContainerColumns, byrow=T), sep=""),rows,columns, dimnames = list(paste("R", 1:rows, sep=""), paste("C", 1:columns, sep="")))
  destinationquadrant3 <- matrix(paste(rep("Q3_",size), matrix("R",rows,columns), matrix(formatC(1:rows,width=3, format="d", flag="0"),rows,columns, byrow=F), matrix("_C",rows,columns), matrix(formatC(1:columns,width=3, format="d", flag="0"),destinationContainerRows,destinationContainerColumns, byrow=T), sep=""),rows,columns, dimnames = list(paste("R", 1:rows, sep=""), paste("C", 1:columns, sep="")))
  destinationquadrant4 <- matrix(paste(rep("Q4_",size), matrix("R",rows,columns), matrix(formatC(1:rows,width=3, format="d", flag="0"),rows,columns, byrow=F), matrix("_C",rows,columns), matrix(formatC(1:columns,width=3, format="d", flag="0"),destinationContainerRows,destinationContainerColumns, byrow=T), sep=""),rows,columns, dimnames = list(paste("R", 1:rows, sep=""), paste("C", 1:columns, sep="")))
  mid_1 <- matrix(rbind(destinationquadrant1, destinationquadrant2),rows,columns)
  mid_2 <- matrix(rbind(destinationquadrant3, destinationquadrant4),rows,columns)
  destinationQuadrants <- matrix(t(cbind(mid_1, mid_2)),rows,columns, dimnames = list(paste("R", 1:rows, sep=""), paste("C", 1:columns, sep="")), byrow=T)
  
  if(direction == "down") {
    originContainer <- matrix(paste(matrix("R",originContainerRows,originContainerColumns), matrix(formatC(1:originContainerRows,width=3, format="d", flag="0"),originContainerRows,originContainerColumns, byrow=F), matrix("_C",originContainerRows,originContainerColumns), matrix(formatC(1:originContainerColumns,width=3, format="d", flag="0"),originContainerRows,originContainerColumns, byrow=T), sep=""),originContainerRows,originContainerColumns, dimnames = list(paste("R", 1:originContainerRows, sep=""), paste("C", 1:originContainerColumns, sep="")))
    originToDestination <- data.frame(ORIGIN=as.vector(originContainer), DESTINATION=as.vector(destinationQuadrants))
    matched <- originToDestination$DESTINATION[match(as.character(originRowColumns),originToDestination$ORIGIN)]
    destinationRowColumns <- list(originQuadrant = as.integer(substr(matched, 2, 2)), destinationRowColumns = substr(matched, 4, 1000000L))
  } else {
    destinationContainer <- matrix(paste(matrix("R",destinationContainerRows,destinationContainerColumns), matrix(formatC(1:destinationContainerRows,width=3, format="d", flag="0"),destinationContainerRows,destinationContainerColumns, byrow=F), matrix("_C",destinationContainerRows,destinationContainerColumns), matrix(formatC(1:destinationContainerColumns,width=3, format="d", flag="0"),destinationContainerRows,destinationContainerColumns, byrow=T), sep=""),destinationContainerRows,destinationContainerColumns, dimnames = list(paste("R", 1:destinationContainerRows, sep=""), paste("C", 1:destinationContainerColumns, sep="")))
    originToDestination <- data.frame(ORIGIN=as.vector(destinationQuadrants), DESTINATION=as.vector(destinationContainer))
    matched <- originToDestination$DESTINATION[match(paste0("Q",destinationQuadrant,"_",as.character(originRowColumns)),originToDestination$ORIGIN)]
    destinationRowColumns <- list(destinationQuadrant = destinationQuadrant, destinationRowColumns = as.character(matched))
  }
  if(any(is.na(matched))) {
    warnUser("TODO MESSAGE")
  }
  return(destinationRowColumns)
}

getWellCodeByPlateBarcodeAndWellName <- function(plateBarcode, wellName) {
  answer <- query(paste0("select c2.code_name
        from container c1
        join container_label cl1
        on c1.id=cl1.container_id
        join itx_container_container itxcc
        on c1.id=itxcc.first_container_id
        join container c2
        on c2.id=itxcc.second_container_id
        join container_label cl2
        on c2.id=cl2.container_id
        where c1.ls_type='container'
        and c1.ls_kind = 'plate'
        and c1.deleted = '0'
        and c1.ignored = '0'
        and cl1.ls_type = 'barcode'
        and cl1.ls_kind = 'barcode'
        and cl1.deleted = '0'
        and cl1.ignored = '0'
        and itxcc.ls_type = 'has member'
        and itxcc.ls_kind = 'container_well'
        and itxcc.deleted = '0'
        and itxcc.ignored = '0'
        and c2.ls_type = 'well'
        and c2.ls_kind = 'default'
        and c2.deleted = '0'
        and c2.ignored = '0'
        and cl2.ls_type = 'name'
        and cl2.ls_kind = 'well name'
        and cl2.deleted = '0'
        and cl2.ignored = '0'
        and cl1.label_text = '",plateBarcode,"'
        and cl2.label_text = '",wellName,"'"))
  if(nrow(answer) == 0) {
    return(NA)
  } else {
    return(answer[1,1])
  }
}

getBreadCrumbByContainerCode <- function(containerCodes, sep = ">") {
  outputExample <- data.table(containerID = NA_integer_,
                              containerCode = NA_character_, 
                              currentLocationID = NA_integer_, 
                              currentLocationCode = NA_character_, 
                              currentLocationLabel = NA_character_,
                              labelBreadCrumb = NA_character_)
  breadCrumbDT <- data.table(containerCode = containerCodes, originalOrder = 1:length(containerCodes), labelBreadCrumb = NA_character_)
  namesBreadCrumbDT <- copy(names(breadCrumbDT))
  movedToContainerLocation <- getCurrentLocationByCodeName(containerCodes)
  setkey(breadCrumbDT, containerCode)
  setkey(movedToContainerLocation, containerCode)
  breadCrumbDT <- movedToContainerLocation[breadCrumbDT]
  if(nrow(breadCrumbDT[!is.na(currentLocationLabel)]) > 0) {
    locationIds <- breadCrumbDT[!is.na(currentLocationLabel)]$currentLocationID
    breadCrumbDT[!is.na(currentLocationLabel), c('labelBreadCrumb','lastLocationID') := list(currentLocationLabel, currentLocationID)]
    while(length(locationIds) > 0) {
      movedToLocationLocation <- rbindlist(query_replace_string_with_values("SELECT container.id container_id,
                                                                            container.code_name container_code,
                                                                            location.id location_id,
                                                                            location.code_name location_code,
                                                                            locationLabel.label_text location_label
                                                                            FROM container
                                                                            JOIN itx_container_container itxContainerLocation
                                                                            ON itxContainerLocation.first_container_id=container.id
                                                                            AND itxContainerLocation.ls_type          = 'moved to'
                                                                            AND itxContainerLocation.ls_kind          = 'location_location'
                                                                            AND itxContainerLocation.ignored         <> '1'
                                                                            AND itxContainerLocation.deleted         <> '1'
                                                                            JOIN container location
                                                                            ON itxContainerLocation.second_container_id=location.id
                                                                            AND location.deleted <> '1'
                                                                            AND location.ignored <> '1'
                                                                            JOIN container_label locationLabel
                                                                            ON location.id=locationLabel.container_id
                                                                            AND locationLabel.deleted <> '1'
                                                                            AND locationLabel.ignored <> '1'
                                                                            AND locationLabel.preferred = '1'
                                                                            AND container.id in (<REPLACEME>)
                                                                            WHERE container.deleted <> '1'
                                                                            AND container.ignored <> '1'", "<REPLACEME>", locationIds))
      
      if(nrow(movedToLocationLocation) > 0) {
        locationIds <- movedToLocationLocation$LOCATION_ID
        setkey(breadCrumbDT, lastLocationID)
        setkey(movedToLocationLocation, CONTAINER_ID)
        breadCrumbDT[movedToLocationLocation, c('labelBreadCrumb', 'lastLocationID') := list(paste0(LOCATION_LABEL,sep,labelBreadCrumb), LOCATION_ID)]
      } else {
        locationIds <- NULL
      }
    }
  }
  setkey(breadCrumbDT, originalOrder)
  breadCrumbDT <- breadCrumbDT[ , names(outputExample), with = FALSE]
  return(breadCrumbDT)
}

getCurrentLocationByCodeName <- function(containerCodes, ...) {
  movedToContainerLocation <- rbindlist(query_replace_string_with_values("SELECT container.id container_id,
                                                                         container.code_name container_code,
                                                                         location.id current_location_id,
                                                                         location.code_name current_location_code,
                                                                         locationLabel.label_text current_location_label
                                                                         FROM container
                                                                         JOIN itx_container_container itxContainerLocation
                                                                         ON itxContainerLocation.first_container_id=container.id
                                                                         AND itxContainerLocation.ls_type          = 'moved to'
                                                                         AND itxContainerLocation.ls_kind          = 'container_location'
                                                                         AND itxContainerLocation.ignored         <> '1'
                                                                         AND itxContainerLocation.deleted         <> '1'
                                                                         JOIN container location
                                                                         ON itxContainerLocation.second_container_id=location.id
                                                                         AND location.deleted <> '1'
                                                                         AND location.ignored <> '1'
                                                                         JOIN container_label locationLabel
                                                                         ON location.id=locationLabel.container_id
                                                                         AND locationLabel.deleted <> '1'
                                                                         AND locationLabel.ignored <> '1'
                                                                         AND locationLabel.preferred = '1'
                                                                         AND container.code_name in (<REPLACEME>)
                                                                         WHERE container.deleted <> '1'
                                                                         AND container.ignored <> '1'
                                                                         ", "<REPLACEME>", containerCodes, ...))
  setnames(movedToContainerLocation, c("containerID", "containerCode", "currentLocationID", "currentLocationCode", "currentLocationLabel"))
  return(movedToContainerLocation)
}
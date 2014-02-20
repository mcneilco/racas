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
                                  if(quadrant > 4 | quadrant < 1) stop("quadrant must be between 1-4")
                                  if(originContainerCode == "") stop("Origin container code cannot be blank")
                                  if(destinationContainerCode == "") stop("Destination container code cannot be blank")
                                  if(!direction %in% c("up", "down")) stop("direction must be up or down")
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
                                    warning("Call to external container data function returned 0 rows")
                                  } else {
                                    originContainerData <<- as.data.table(originContainerDat)
                                    return(originContainerData)
                                  }
                                },
                                remap = function() {
                                  if(is.null(originContainerData)) {return(NULL)}
                                  if(originContainerData$platesize == 1536 && direction == "up") {warning("Sorry cannot remap to plate size larger than 1536"); return(NULL)}
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
    warning("TODO MESSAGE")
  }
  return(destinationRowColumns)
}
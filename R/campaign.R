objectUtilities <- setRefClass(Class = "objectUtilities",
                               #fields = list("test" = "character",
                                #              "this" = "integer"),
                               methods = list(
                                 getFields = function() {
                                   return(.self$getRefClass()$fields())
                                 },
                                 toList = function() {
                                   fieldNames <- names(.self$getFields())
                                   values <- lapply(fieldNames, function(x, self = .self) self$field(x))
                                   names(values) <- fieldNames
                                   return(values)
                                 },
                                 toJSON = function() {
                                   fields <- getFields()
                                   myList <- toList()
                                   myList[fields == "environment"] <- NULL
                                   return(rjson::toJSON(myList))
                                 },
                                 fromJSON = function(json) {
                                   myList <- rjson::fromJSON(json)
                                   fields <- .self$getFields()
                                   fieldNames <- names(fields)
                                   matches <- match(names(myList), fieldNames)
                                   matches <- matches[!is.na(matches)]
                                   lapply(matches, function(x) {
                                     x <- fields[x]
                                     value <- myList[names(x)][[1]]
                                     if(length(value) > 0) {
                                       .self$field(names(x), as(myList[names(x)], Class = x))
                                     } else {
                                       .self$field(names(x), as(NULL, Class = x))
                                     }
                                   }
                                   )
                                   return(.self)
                                 },
                                 as.data.table = function(){
                                   return(data.table::as.data.table((toList())))
                                 }
                                 
                               )
)

PrimaryCriteria <- setRefClass("PrimaryCriteria", 
                               fields = list(id = "integer",
                                             campaign_id = "integer",
                                             campaign_name = "character",
                                             experiment_id = "integer",
                                             experiment_name = "character",
                                             threshold_value = "numeric",
                                             threshold_operator = "character",
                                             threshold_type = "character",
                                             type = "character"),
                               contains = list("objectUtilities"),
                               methods = list(
                                 initialize = function(..., json = NA) {
                                   callSuper(...)
                                   if(length(threshold_type !=0)) {
                                     if(!threshold_type %in% c("EFF", "SD")) stop("threshold_type must be EFF or SD")
                                   }
                                 },
                                 fetchOriginCampaignData = function() {
                                   #TODO implement acas schema call here to get campaign data
                                 },
                                 fetchExternalCampaignData = function(func) {
                                   campaignData <- func("campaign_id")
                                   if(nrow(campaignData) == 0) {
                                     warnUser("Call to external campaign data function returned 0 rows")
                                   } else {
                                     return(campaignData)
                                   }
                                 }
          
                               )
)
PrimaryCriteriaList <- setRefClass("PrimaryCriteriaList", 
                                   fields = list(primaryCriteria = "list"
                                   ),
                                   contains = list("objectUtilities"),
                                   methods = list(
                                     fromJSON = function(json) {
                                       myList <- rjson::fromJSON(json)$primaryCriteria
                                       primaryCriteria <<- lapply(myList, function(x) PrimaryCriteria$new()$fromJSON(rjson::toJSON(x)))
                                       return(.self)
                                     },
                                     toJSON = function() {
                                       myList <- list()
                                       myList$primaryCriteria <- lapply(primaryCriteria, function(x) x$toList())
                                       return(rjson::toJSON(myList))
                                     },
                                     as.data.table = function () {
                                       return( rbindlist(lapply(primaryCriteria, function(x) x$as.data.table())))
                                     }
                                   )
)



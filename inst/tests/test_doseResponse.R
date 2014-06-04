context("doseResponse.R")

if(!exists("updateResults")) updateResults <- FALSE

rdaTest <- function(newResults, acceptedResultsPath, updateResults = FALSE) {
  if(updateResults) {
    acceptedResults <- newResults
    return(save(acceptedResults , file = acceptedResultsPath))
  } else {    
    load(system.file("tests", acceptedResultsPath, package = "racas"))
    return(expect_that(newResults,
                equals(acceptedResults)))
  }
}

# if(updateResults) {
#   experimentCode <- loadDoseResponseTestData()
#   fitData <- getFitData(experimentCode)
#   save(fitData, file = file.path("data","doseResponse", "data", "fitData_ll4.rda"))
#   file <- system.file("tests","data", "doseResponse","conf","default_fitSettings_ll4.json", package = "racas")
#   fitSettings <- fromJSON(readChar(file, file.info(file)$size))
#   fitData <- doseResponse.fitData(fitSettings, fitData)
#   save(fitData, file = file.path("data","doseResponse","data","fitData_ll4_fitted.rda"))
# }

test_that("LL4 doseResponse.fitData output has not changed",{
  file <- system.file("tests","data", "doseResponse","conf","default_fitSettings_ll4.json", package = "racas")
  fitSettings <- fromJSON(readChar(file, file.info(file)$size))
  load(system.file("tests","data","doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedresults", "doseResponse.fitData_ll4.rda")
  newResults <- doseResponse.fitData(fitSettings, fitData)
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("simpleToAdvancedFitSettings for ll4",{
  file <- system.file("tests","data", "doseResponse","conf","example_simple_fitsettings_ll4.json", package = "racas")
  simpleSettingsJSON <- readChar(file, file.info(file)$size)
  simpleSettings <- fromJSON(simpleSettingsJSON)
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedresults","simpleToAdvancedFitSettings_ll4.rda")
  newResults <- simpleToAdvancedFitSettings(simpleSettings)
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("getDefaultFitSettings for ll4",{
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","getDefaultFitSettings_ll4.rda")
  newResults <- getDefaultFitSettings("LL.4")
  rdaTest(newResults, acceptedResultsPath, updateResults = TRUE)
})

test_that("doseResponse basic test",{
  file <- system.file("tests","data", "doseResponse", "conf", "default_fitSettings_ll4.json", package = "racas")
  fitSettings <- fromJSON(readChar(file, file.info(file)$size))
  load(system.file("tests","data", "doseResponse", "data","fitData_ll4.rda", package = "racas"))
  newResults <- doseResponse(fitSettings = fitSettings, fitData = fitData)
  acceptedResultsRDAPath <- file.path("data","doseResponse","acceptedresults","doseResponse_ll4.rda")
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                is_a("list"))
    expect_that("fitData" %in% names(newResults),
                is_true())
    expect_that("sessionID" %in% names(newResults),
                is_true())
    expect_that(newResults$sessionID,
                is_a("character"))
    expect_that(newResults$fitData,
                is_a("data.table"))
    expect_that(nrow(newResults$fitData) > 0,
                is_true())
    expect_that(newResults$fitData,
                equals(acceptedResults$fitData))    
  }
})

test_that("predictPoints basic test",{
  load(system.file("tests","data", "doseResponse","data", "fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedresults", "predictPoints.rda")
  newResults <- predictPoints(fitData[1]$points[[1]], fitData[1]$model[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("plotWindow basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","plotWindow.rda")
  newResults <- predictPoints(fitData[1]$points[[1]], fitData[1]$model[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("captureOutput basic test",{
  load(system.file("tests","data", "doseResponse","data", "fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","captureOutput.rda")
  newResults <- captureOutput(summary(fitData[1]$model[[1]]), collapse = "<br>")
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("objToHTMLTableString basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","objToHTMLTableString.rda")
  newResults <- objToHTMLTableString(fitData[1]$points[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("getReportedParameters basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","getReportedParameters.rda")
  newResults <- fitData[ , list(getReportedParameters(modelHint[[1]],
                                                           results.parameterRules[[1]],
                                                           inactive[[1]],
                                                           fitConverged[[1]],
                                                           insufficientRange[[1]],
                                                           potent[[1]],
                                                           fixedParameters[[1]],
                                                           fittedParameters[[1]],
                                                           pointStats[[1]],
                                                           goodnessOfFit.parameters[[1]],
                                                           goodnessOfFit.model[[1]],
                                                           flag_user, 
                                                           flag_algorithm)), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("doseResponseFit basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","doseResponseFit.rda")
  newResults <- doseResponseFit(fitData)
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("applyParameterRules.limits basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","applyParameterRules.limits.rda")
  newResults <- fitData[ 1, list(list(applyParameterRules.limits(fittedParameters[[1]],
                                                                      pointStats[[1]],
                                                                      parameterRules[[1]]$limits))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("applyParameterRules.goodnessOfFits basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults", "applyParameterRules.goodnessOfFits.rda")
  newResults <- fitData[ 1, list(list(applyParameterRules.goodnessOfFits(goodnessOfFit.parameters[[1]],
                                                                 parameterRules[[1]]$parameterRules))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})
test_that("applyInactiveRule basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","applyInactiveRule.rda")
  newResults <- fitData[ 1, list(list(applyInactiveRule(pointStats[[1]],
                                                        points[[1]],
                                                        inactiveRule[[1]],
                                                        TRUE))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("categorizeFitData basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","categorizeFitData.rda")
  newResults <-   fitData[ , categorizeFitData(modelHint, results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]]), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("getDRCModel basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","getDRCModel.rda")
  newResults <- fitData[model.synced == FALSE, list(model = list(switch(modelHint,
                                                                       "LL.4" = getDRCModel(points[[1]], drcFunction = LL.4, paramNames = c("slope", "min", "max", "ec50"), fixed = fixedParameters[[1]]),
                                                                       "MM.3" = getDRCModel(points[[1]], drcFunction = MM.3, paramNames = c("slope","max", "kd"), fixed = fixedParameters[[1]]),
                                                                       "MM.2" = getDRCModel(points[[1]], drcFunction = MM.2, paramNames = c("max", "kd"), fixed = fixedParameters[[1]])
  ))
  ), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("getPointStats basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","getPointStats.rda")
  newResults <- getPointStats(fitData[1]$points[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("drcObject.getParameters basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","drcObject.getParameters.rda")
  newResults <- fitData[ , list(list(drcObject.getParameters(model[[1]]))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("drcObject.getDRCFitStats basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","drcObject.getDRCFitStats.rda")
  newResults <- fitData[ , list(list(drcObject.getDRCFitStats(model[[1]], points[[1]]))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("drcObject.getGoodnessOfFitParameters basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))    
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","drcObject.getGoodnessOfFitParameters.rda")
  newResults <- fitData[ , list(list(drcObject.getGoodnessOfFitParameters(model[[1]]))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("fitDataToResponse.acas basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","fitDataToResponse.acas.rda")
  newResults <- fitDataToResponse.acas(fitData, status = "completed", hasWarning = FALSE, hasError = FALSE)
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("knit2html.bugFix basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","knit2html.bugFix.rda")
  rmd <- system.file("rmd", "fitDataToResponse_acas.rmd", package="racas")
  newResults <- knit2html.bugFix(input = rmd, 
                                  options = c("base64_images", "mathjax"),
                                  template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                  stylesheet = system.file("rmd", "racas_container.css", package="racas"))  
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("knit2html.bugFix basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))    
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","knit2html.bugFix.rda")
  rmd <- system.file("rmd", "fitDataToResponse_acas.rmd", package="racas")
  newResults <- knit2html.bugFix(input = rmd, 
                                 options = c("base64_images", "mathjax"),
                                 template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                 stylesheet = system.file("rmd", "racas_container.css", package="racas"))  
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("flattenListToDataTable basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))    
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","flattenListToDataTable.rda")
  newResults <- flattenListToDataTable(fitData[1]$reportedParameters[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("biphasic compounds still biphasic",{
  file <- system.file("tests","data", "doseResponse","conf","default_fitSettings_ll4.json", package = "racas")
  fitSettings <- fromJSON(readChar(file, file.info(file)$size))
  load(system.file("tests","data", "doseResponse", "data","fitData_ll4.rda", package = "racas")) 
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","biphasic.rda")
  newResults <- rbindlist(doseResponse.fitData(fitSettings, fitData)$points)[ , c("dose", "response", "flag_algorithm"), with = FALSE]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

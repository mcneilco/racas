context("doseResponse.R")

test_that("doseResponse.fitData for ec50",{
  file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
  fitSettingsJSON <- readChar(file, file.info(file)$size)
  fitSettings <- fromJSON(fitSettingsJSON)
  data("example-ec50-fitData")
  acceptedResultsRDAPath <- "data/doseResponse/doseResponse.fitData_acceptedResults.rda"
  newResults <- doseResponse.fitData(fitSettings, fitData)
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("simpleToAdvancedFitSettings for ec50",{
  file <- system.file("docs", "example-ec50-simple-fitSettings.json", package = "racas")
  simpleSettingsJSON <- readChar(file, file.info(file)$size)
  simpleSettings <- fromJSON(simpleSettingsJSON)
 
  acceptedResultsRDAPath <- "data/doseResponse/simpleToAdvancedFitSettings.EC50_acceptedResults.rda"
  newResults <- simpleToAdvancedFitSettings(simpleSettings)
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
    
  }
})

test_that("getDefaultFitSettings for ec50",{
  acceptedResultsRDAPath <- "data/doseResponse/getDefaultFitSettings_LL.4.rda"
  newResults <- getDefaultFitSettings("LL.4")
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
    
  }
})

test_that("doseResponse basic test",{
  file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
  fitSettingsJSON <- readChar(file, file.info(file)$size)
  fitSettings <- fromJSON(fitSettingsJSON)
  data("example-ec50-fitData")
  newResults <- doseResponse(fitSettings = fitSettings, fitData = fitData)
  acceptedResultsRDAPath <- "data/doseResponse/doseResponse_acceptedResults.rda"
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
  data("example-ec50-fitData-fitted")
  acceptedResultsRDAPath <- "data/doseResponse/predictPoints_acceptedResults.rda"
  newResults <- predictPoints(fitData[1]$points[[1]], fitData[1]$model[[1]])
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("plotWindow basic test",{
  data("example-ec50-fitData")
  
  acceptedResultsRDAPath <- "data/doseResponse/plotWindow_acceptedResults.rda"
  newResults <- predictPoints(fitData[1]$points[[1]], fitData[1]$model[[1]])
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("captureOutput basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/captureOutput_acceptedResults.rda"
  newResults <- captureOutput(summary(fitData[1]$model[[1]]), collapse = "<br>")
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("objToHTMLTableString basic test",{
  data("example-ec50-fitData")
  
  acceptedResultsRDAPath <- "data/doseResponse/objToHTMLTableString_acceptedResults.rda"
  newResults <- objToHTMLTableString(fitData[1]$points[[1]])
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("getReportedParameters basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/getReportedParameters_acceptedResults.rda"
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
                                                           goodnessOfFit.model[[1]])), by = curveid]
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("doseResponseFit basic test",{
  data("example-ec50-fitData")
  
  acceptedResultsRDAPath <- "data/doseResponse/doseResponseFit_acceptedResults.rda"
  newResults <- doseResponseFit(fitData)
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("applyParameterRules.limits basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/applyParameterRules.limits_acceptedResults.rda"
  newResults <- fitData[ 1, list(list(applyParameterRules.limits(fittedParameters[[1]],
                                                                      pointStats[[1]],
                                                                      parameterRules[[1]]$limits))), by = curveid]
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("applyParameterRules.goodnessOfFits basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/applyParameterRules.goodnessOfFits_acceptedResults.rda"
  newResults <- fitData[ 1, list(list(applyParameterRules.goodnessOfFits(goodnessOfFit.parameters[[1]],
                                                                 parameterRules[[1]]$parameterRules))), by = curveid]
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})
test_that("applyInactiveRule basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/applyInactiveRule_acceptedResults.rda"
  newResults <- fitData[ 1, list(list(applyInactiveRule(pointStats[[1]],
                                                        points[[1]],
                                                        inactiveRule[[1]]))), by = curveid]
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("categorizeFitData basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/categorizeFitData_acceptedResults.rda"
  newResults <-   fitData[ , categorizeFitData(results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]]), by = curveid]

  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("getDRCModel basic test",{
  data("example-ec50-fitData")
  
  acceptedResultsRDAPath <- "data/doseResponse/getDRCModel_acceptedResults.rda"
  newResults <- fitData[model.synced == FALSE, list(model = list(switch(modelHint,
                                                                       "LL.4" = getDRCModel(points[[1]], drcFunction = LL.4, paramNames = c("slope", "min", "max", "ec50"), fixed = fixedParameters[[1]]),
                                                                       "MM.3" = getDRCModel(points[[1]], drcFunction = MM.3, paramNames = c("slope","max", "kd"), fixed = fixedParameters[[1]]),
                                                                       "MM.2" = getDRCModel(points[[1]], drcFunction = MM.2, paramNames = c("max", "kd"), fixed = fixedParameters[[1]])
  ))
  ), by = curveid]
  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("getPointStats basic test",{
  data("example-ec50-fitData")
  
  acceptedResultsRDAPath <- "data/doseResponse/getPointStats_acceptedResults.rda"
  newResults <- getPointStats(fitData[1]$points[[1]])
  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("drcObject.getParameters basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/drcObject.getParameters_acceptedResults.rda"
  newResults <- fitData[ , list(list(drcObject.getParameters(model[[1]]))), by = curveid]
  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("drcObject.getDRCFitStats basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/drcObject.getDRCFitStats_acceptedResults.rda"
  newResults <- fitData[ , list(list(drcObject.getDRCFitStats(model[[1]], points[[1]]))), by = curveid]
  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("drcObject.getGoodnessOfFitParameters basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/drcObject.getGoodnessOfFitParameters_acceptedResults.rda"
  newResults <- fitData[ , list(list(drcObject.getGoodnessOfFitParameters(model[[1]]))), by = curveid]
  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("fitDataToResponse.acas basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/fitDataToResponse.acas_acceptedResults.rda"
  newResults <- fitDataToResponse.acas(fitData, status = "completed", hasWarning = FALSE, hasError = FALSE)
  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("knit2html.bugFix basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/knit2html.bugFixs_acceptedResults.rda"
  rmd <- system.file("rmd", "fitDataToResponse_acas.rmd", package="racas")
  newResults <- knit2html.bugFix(input = rmd, 
                                  options = c("base64_images", "mathjax"),
                                  template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                  stylesheet = system.file("rmd", "racas_container.css", package="racas"))  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("knit2html.bugFix basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/knit2html.bugFix_acceptedResults.rda"
  rmd <- system.file("rmd", "fitDataToResponse_acas.rmd", package="racas")
  newResults <- knit2html.bugFix(input = rmd, 
                                 options = c("base64_images", "mathjax"),
                                 template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                 stylesheet = system.file("rmd", "racas_container.css", package="racas"))  
  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("flattenListToDataTable basic test",{
  data("example-ec50-fitData-fitted")
  
  acceptedResultsRDAPath <- "data/doseResponse/flattenListToDataTable_acceptedResults.rda"
  newResults <- flattenListToDataTable(fitData[1]$reportedParameters[[1]])

  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})

test_that("listToDataTable basic test",{
  file <- system.file("docs", "default-ec50-fitSettings.json", package = "racas")
  fitSettingsJSON <- readChar(file, file.info(file)$size)
  fitSettings <- fromJSON(fitSettingsJSON)
  
  acceptedResultsRDAPath <- "data/doseResponse/listToDataTable_acceptedResults.rda"
  newResults <- listToDataTable(fitSettings)

  if(updateResults) {
    acceptedResults <- newResults
    save(acceptedResults , file = acceptedResultsRDAPath)
  } else {
    load(system.file(file.path("tests",acceptedResultsRDAPath), package = "racas"))
    expect_that(newResults,
                equals(acceptedResults))
  }
})



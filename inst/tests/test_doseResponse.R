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
#   experimentCode <- load_dose_response_test_data()
#   fitData <- get_fit_data_experiment_code(experimentCode, full_object = TRUE)
#   save(fitData, file = file.path("data","doseResponse", "data", "fitData_ll4.rda"))
#   file <- system.file("tests","data", "doseResponse","conf","default_fitSettings_ll4.json", package = "racas")
#   fitSettings <- fromJSON(readChar(file, file.info(file)$size))
#   fitData <- dose_response(fitSettings, fitData)
#   save(fitData, file = file.path("data","doseResponse","data","fitData_ll4_fitted.rda"))
# }
importantFitDataColumns <- c("fitConverged", "pointStats", "fittedParameters", "goodnessOfFit.model", "goodnessOfFit.parameters", "results.parameterRules", "inactive", "insufficientRange", "potent", "category", "reportedParameters")


test_that("LL4 dose_response output has not changed",{
  file <- system.file("tests","data", "doseResponse","conf","default_fitSettings_ll4.json", package = "racas")
  fitSettings <- fromJSON(readChar(file, file.info(file)$size))
  load(system.file("tests","data","doseResponse","data","fitData_ll4.rda", package = "racas"))
  newResults <- dose_response(fitSettings, fitData)
  newResults <- newResults[ , importantFitDataColumns,]
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedresults", "dose_response_ll4.rda")
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("simple_to_advanced_fit_settings for ll4",{
  file <- system.file("tests","data", "doseResponse","conf","example_simple_fitsettings_ll4.json", package = "racas")
  simpleSettingsJSON <- readChar(file, file.info(file)$size)
  simpleSettings <- fromJSON(simpleSettingsJSON)
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedresults","simple_to_advanced_fit_settings_ll4.rda")
  newResults <- simple_to_advanced_fit_settings(simpleSettings)
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_default_fit_settings for ll4",{
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","get_default_fit_settings_ll4.rda")
  newResults <- get_default_fit_settings("LL.4")
  rdaTest(newResults, acceptedResultsPath, updateResults = TRUE)
})

test_that("doseResponse basic test",{
  file <- system.file("tests","data", "doseResponse", "conf", "default_fitSettings_ll4.json", package = "racas")
  fitSettings <- fromJSON(readChar(file, file.info(file)$size))
  load(system.file("tests","data", "doseResponse", "data","fitData_ll4.rda", package = "racas"))
  newResults <- dose_response_session(fitSettings = fitSettings, fitData = fitData)
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

test_that("predict_drm_points basic test",{
  load(system.file("tests","data", "doseResponse","data", "fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedresults", "predict_drm_points.rda")
  newResults <- predict_drm_points(fitData[1]$points[[1]], fitData[1]$model[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_plot_window basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","get_plot_window.rda")
  newResults <- get_plot_window(fitData[1]$points[[1]], logDose = TRUE)
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("capture_output basic test",{
  load(system.file("tests","data", "doseResponse","data", "fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","capture_output.rda")
  newResults <- capture_output(summary(fitData[1]$model[[1]]), collapse = "<br>")
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("data.table_to_html_table basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","data.table_to_html_table.rda")
  newResults <- data.table_to_html_table(fitData[1]$points[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_reported_parameters basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","get_reported_parameters.rda")
  newResults <- fitData[ , list(get_reported_parameters(modelHint[[1]],
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

test_that("dose_response_fit basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","dose_response_fit.rda")
  newResults <- dose_response_fit(fitData)
  newResults <- newResults[ , importantFitDataColumns,]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("apply_parameter_rules_limits basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","apply_parameter_rules_limits.rda")
  fittedParams <- list("ec50" = 10000000)
  newResults <- fitData[ 1, list(list(apply_parameter_rules_limits(fittedParams,
                                                                      pointStats[[1]],
                                                                      parameterRules[[1]]$limits))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})
test_that("apply_parameter_rules_goodness_of_fits basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults", "apply_parameter_rules_goodness_of_fits.rda")
  goodnessOFFITS <- list(max.stdErr = 1000)
  newResults <- fitData[ 1, list(list(apply_parameter_rules_goodness_of_fits(goodnessOFFITS,
                                                                 parameterRules[[1]]$goodnessOfFits))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})
test_that("apply_inactive_rules basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","apply_inactive_rules_inactive.rda")
  fitData$points[[1]][ , response := 0]
  newResults <- fitData[ 1, list(list(apply_inactive_rules(pointStats[[1]],
                                                        points[[1]],
                                                        inactiveRule[[1]],
                                                        TRUE))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","apply_inactive_rules_potent.rda")
  fitData$points[[1]][ , response := 10000]
  newResults <- fitData[ 1, list(list(apply_inactive_rules(pointStats[[1]],
                                                           points[[1]],
                                                           inactiveRule[[1]],
                                                           TRUE))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("categorize_fit_data basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","categorize_fit_data.rda")
  newResults <-   fitData[ , categorize_fit_data(modelHint, results.parameterRules[[1]], fitSettings[[1]], inactive[[1]], fitConverged[[1]], insufficientRange[[1]], potent[[1]], pointStats[[1]]), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_drc_model basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","get_drc_model.rda")
  newResults <- fitData[model.synced == FALSE, list(model = list(switch(modelHint,
                                                                       "LL.4" = get_drc_model(points[[1]], drcFunction = LL.4, paramNames = c("slope", "min", "max", "ec50"), fixed = fixedParameters[[1]]),
                                                                       "MM.3" = get_drc_model(points[[1]], drcFunction = MM.3, paramNames = c("slope","max", "kd"), fixed = fixedParameters[[1]]),
                                                                       "MM.2" = get_drc_model(points[[1]], drcFunction = MM.2, paramNames = c("max", "kd"), fixed = fixedParameters[[1]])
  ))
  ), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_point_stats basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4.rda", package = "racas"))
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","get_point_stats.rda")
  newResults <- get_point_stats(fitData[1]$points[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_parameters_drc_object basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","get_parameters_drc_object.rda")
  newResults <- fitData[ , list(list(get_parameters_drc_object(model[[1]]))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_fit_stats_drc_object basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","get_fit_stats_drc_object.rda")
  newResults <- fitData[ , list(list(get_fit_stats_drc_object(model[[1]], points[[1]]))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("get_goodness_of_fit_parameters_drc_object basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))    
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","get_goodness_of_fit_parameters_drc_object.rda")
  newResults <- fitData[ , list(list(get_goodness_of_fit_parameters_drc_object(model[[1]]))), by = curveid]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("fit_data_to_acas_experiment_response basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","fit_data_to_acas_experiment_response.rda")
  newResults <- fit_data_to_acas_experiment_response(fitData, experimentCode = "EXPT-00001", status = "completed", hasWarning = FALSE, hasError = FALSE)
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("knit2html_bug_fix basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))  
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","knit2html_bug_fix.rda")
  rmd <- system.file("rmd", "fitDataToResponse_acas.rmd", package="racas")
  experimentCode <- "blah"
  newResults <- knit2html_bug_fix(input = rmd, 
                                  options = c("base64_images", "mathjax"),
                                  template =  system.file("rmd", "fitDataToResponse_acas.html", package="racas"),
                                  stylesheet = system.file("rmd", "racas_container.css", package="racas"))  
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("flatten_list_to_data.table basic test",{
  load(system.file("tests","data", "doseResponse","data","fitData_ll4_fitted.rda", package = "racas"))    
  acceptedResultsPath <- file.path("data","doseResponse", "acceptedResults","flatten_list_to_data.table.rda")
  newResults <- flatten_list_to_data.table(fitData[1]$reportedParameters[[1]])
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

test_that("biphasic compounds still biphasic",{
  file <- system.file("tests","data", "doseResponse","conf","default_fitSettings_ll4.json", package = "racas")
  fitSettings <- fromJSON(readChar(file, file.info(file)$size))
  load(system.file("tests","data", "doseResponse", "data","fitData_ll4.rda", package = "racas")) 
  acceptedResultsPath <- file.path("data","doseResponse","acceptedResults","biphasic.rda")
  newResults <- rbindlist(dose_response(fitSettings, fitData)$points)[ , c("dose", "response", "flag_algorithm"), with = FALSE]
  rdaTest(newResults, acceptedResultsPath, updateResults = updateResults)
})

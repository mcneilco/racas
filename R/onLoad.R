.onLoad <- function(libname, pkgname) {
  appConfName <- paste0(applicationSettings$appName,"_CONFIG")
  appHomeName <- paste0(applicationSettings$appName,"_HOME")
  relativeToAppHome <- file.path(normalizePath(file.path(libname,"..")))
  relativeConf <- file.path(normalizePath(relativeToAppHome),"conf","compiled","conf.properties")
  appConf <- Sys.getenv(appConfName)
  appHome <- Sys.getenv(appHomeName)
  if(appHome == ""  && appConf == "" && !file.exists(relativeConf)) {
    warning(paste0("Config file does not exist relative to package: ",relativeConf,"\n",
                    appHomeName," not set\n",
                    appConfName," not set\n\n",
                    "Not reading configuration file.\n\n",
                   "Install the racas package in a directory one level below the base ",applicationSettings$appName," install \n",
                   "OR\n",
                    "set one of the following environment variables prior to loading this package by adding them to your shell or by running:\n",
                   "\tSys.setenv(",appHomeName," = \"/path/to/",applicationSettings$appName,"\")\n",
                   "\tSys.setenv(",appConfName," = \"/path/to/",applicationSettings$appName,"_config_file\")\n\n",
                   "The priority of these setings are:\n",
                   "1) ACAS_CONFIG environment variable\n",
                   "2) ACAS_HOME environment variable\n",
                   "3) relative path to the config file ",relativeConf),call.=FALSE)
  } else {
    tryCatch({
      #if app configuration location
      if(appConf!="") {
        configFileLocation <- file.path(appConf)        
      } else {
        #If app home environment variable is set
        if(appHome!="") {
          configFileLocation <- file.path(appHome,"conf/compiled/conf.properties")
        } else {
          #if relative path then set home accoridingly
          appHome <- relativeToAppHome
          configFileLocation <- relativeConf
        }
      }
      packageStartupMessage(paste("Using configuration file:",configFileLocation ))
      output <- readConfigFile(configFileLocation, appHome = appHome)
      
    },error = function(ex) {
      #error <- capture.output(print(ex))
      warning(paste0("Could not load configuration file at:\n\t",
                     configFileLocation,"\n\n",
                     "This was the captured error:\n",
                     "\t",ex),call.=FALSE)
    })
  }
  racasLogger <- createLogger()
  assignInNamespace("racasLogger",racasLogger, ns="racas")
  racasMessenger <- Messenger$new(envir = environment())
  racasMessenger$logger <- racasLogger
  assignInNamespace("racasMessenger",racasMessenger, ns="racas")

  if(is.null(applicationSettings$server.database.r.driver)) {
    dbType <- "Postgres"
  } else {
    dbType <- tryCatch({
      getDBType(applicationSettings$server.database.r.driver)
    }, error = function(ex ){
      "Postgres"
    })
  }
  definition <- read_json_file(system.file("conf", "definition-ll4.json", package = "racas"))
  queriesAndTypeMap <- entityDefinitionToQueriesAndTypeMap(definition, dbType)
  ll4 <- ModelFit$new(drc_function = drc::LL.4, 
                      paramNames = c("slope", "min", "max", "ec50"),
                      categorization_function = categorize.LL4,
                      get_reported_parameters = get_reported_parameters.LL4,
                      apply_limits = apply_limits.LL4,
                      default_fit_settings = get_default_fit_settings("4 parameter D-R"),
                      simple_to_advanced_fittings_function = updateFitSettings.LL4,
                      model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "ll4.txt", package = "racas")),
                      sortOptions = sortOptions.LL4,
                      get_curve_attributes = get_curve_attributes.LL4,
                      get_saved_fitted_parameters = get_saved_fitted_parameters.LL4,
                      curveid_query = queriesAndTypeMap$curveSQL,
                      experiment_query = queriesAndTypeMap$experimentSQL,
                      raw_results_persistence_path = 'curvefit/rawdata',
                      typeMap = queriesAndTypeMap$typeMap
  )
  assignInNamespace("ll4",ll4, ns="racas")

  definition <- read_json_file(system.file("conf", "definition-ll4IC50.json", package = "racas"))
  queriesAndTypeMap <- entityDefinitionToQueriesAndTypeMap(definition, dbType)
  ll4IC50 <- ModelFit$new(drc_function = drc::LL.4, 
                      paramNames = c("slope", "min", "max", "ic50"),
                      categorization_function = categorize.LL4IC50,
                      get_reported_parameters = get_reported_parameters.LL4IC50,
                      apply_limits = apply_limits.LL4IC50,
                      default_fit_settings = get_default_fit_settings("4 parameter D-R IC50"),
                      simple_to_advanced_fittings_function = updateFitSettings.LL4IC50,
                      model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "ll4IC50.txt", package = "racas")),
                      sortOptions = sortOptions.LL4IC50,
                      get_curve_attributes = get_curve_attributes.LL4IC50,
                      get_saved_fitted_parameters = get_saved_fitted_parameters.LL4IC50,
                      curveid_query = queriesAndTypeMap$curveSQL,
                      experiment_query = queriesAndTypeMap$experimentSQL,
                      typeMap = queriesAndTypeMap$typeMap
  )
  assignInNamespace("ll4IC50",ll4IC50, ns="racas")

  definition <- read_json_file(system.file("conf", "definition-ll4IC50.json", package = "racas"))
  dMax <- structure(list(name = "dmax", ls_kind = "DMax", select = list(
    structure(list(name = "dmax", field = "numeric_value"), .Names = c("name",
                                                                        "field")), structure(list(name = "dmax", field = "string_value"), .Names = c("name",
                                                                                                                                                      "field")),
    structure(list(name = "dmaxUncertainty", field = "uncertainty"), .Names = c("name",
                                                                                 "field")), structure(list(name = "dmaxUncertaintyType", field = "uncertainty_type"), .Names = c("name",
                                                                                                                                                                                  "field")), structure(list(name = "dmaxOperatorKind", field = "operator_kind"), .Names = c("name",
                                                                                                                                                                                                                                                                             "field")))), .Names = c("name", "ls_kind", "select"))
  addLocation <- length(definition$analysis_group[[1]]$analysis_group_state[[1]]$analysis_group_value)+1
  definition$analysis_group[[1]]$analysis_group_state[[1]]$analysis_group_value[[addLocation]] <- dMax
  queriesAndTypeMap <- entityDefinitionToQueriesAndTypeMap(definition, dbType)
  ll4IC50DMax <- ModelFit$new(drc_function = drc::LL.4, 
                          paramNames = c("slope", "min", "max", "ic50"),
                          categorization_function = categorize.LL4IC50,
                          get_reported_parameters = get_reported_parameters.LL4IC50DMax,
                          apply_limits = apply_limits.LL4IC50,
                          default_fit_settings = get_default_fit_settings("4 parameter D-R IC50"),
                          simple_to_advanced_fittings_function = updateFitSettings.LL4IC50,
                          model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "ll4IC50.txt", package = "racas")),
                          sortOptions = sortOptions.LL4IC50DMax,
                          get_curve_attributes = get_curve_attributes.LL4IC50DMax,
                          get_saved_fitted_parameters = get_saved_fitted_parameters.LL4IC50,
                          curveid_query = queriesAndTypeMap$curveSQL,
                          experiment_query = queriesAndTypeMap$experimentSQL,
                          typeMap = queriesAndTypeMap$typeMap
  )
  assignInNamespace("ll4IC50DMax",ll4IC50DMax, ns="racas")
  
  definition <- read_json_file(system.file("conf", "definition-kifit.json", package = "racas"))
  queriesAndTypeMap <- entityDefinitionToQueriesAndTypeMap(definition, dbType)
  kifit <- ModelFit$new(drc_function = ki_fct.5, 
                        paramNames = c("min", "max", "ki", "ligandConc", "kd"), 
                        categorization_function = categorize.ki, 
                        get_reported_parameters = get_reported_parameters.ki,
                        apply_limits = apply_limits.ki,
                        default_fit_settings = get_default_fit_settings("Ki Fit"),
                        simple_to_advanced_fittings_function = updateFitSettings.Ki,
                        model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "ki.txt", package = "racas")),
                        sortOptions = sortOptions.ki,
                        get_curve_attributes = get_curve_attributes.ki,
                        get_saved_fitted_parameters = get_saved_fitted_parameters.ki,
                        curveid_query = queriesAndTypeMap$curveSQL,
                        experiment_query = queriesAndTypeMap$experimentSQL,
                        raw_results_persistence_path = 'curvefit/rawdata',
                        typeMap = queriesAndTypeMap$typeMap
  )
  assignInNamespace("kifit",kifit, ns="racas")
  
  definition <- read_json_file(system.file("conf", "definition-mm2.json", package = "racas"))
  queriesAndTypeMap <- entityDefinitionToQueriesAndTypeMap(definition, dbType)
  kifit <- ModelFit$new(drc_function = drc::MM.2, 
                        paramNames = c("vmax", "km"), 
                        categorization_function = categorize.MM2, 
                        get_reported_parameters = get_reported_parameters.MM2,
                        apply_limits = apply_limits.MM2,
                        default_fit_settings = get_default_fit_settings("Michaelis-Menten"),
                        simple_to_advanced_fittings_function = updateFitSettings.MM2,
                        model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "mm2.txt", package = "racas")),
                        sortOptions = sortOptions.MM2,
                        get_curve_attributes = get_curve_attributes.MM2,
                        get_saved_fitted_parameters = get_saved_fitted_parameters.MM2,
                        curveid_query = queriesAndTypeMap$curveSQL,
                        experiment_query = queriesAndTypeMap$experimentSQL,
                        raw_results_persistence_path = 'curvefit/rawdata',
                        typeMap = queriesAndTypeMap$typeMap
  )
  assignInNamespace("mm2",kifit, ns="racas")
  
  definition <- read_json_file(system.file("conf", "definition-substrateInhibition.json", package = "racas"))
  queriesAndTypeMap <- entityDefinitionToQueriesAndTypeMap(definition, dbType)
  substrateInhibition <- ModelFit$new(drc_function = substrateInhibition.3, 
                        paramNames = c("vmax", "km", "ki"), 
                        categorization_function = categorize.substrateInhibition, 
                        get_reported_parameters = get_reported_parameters.substrateInhibition,
                        apply_limits = apply_limits.substrateInhibition,
                        default_fit_settings = get_default_fit_settings("Substrate Inhibition"),
                        simple_to_advanced_fittings_function = updateFitSettings.substrateInhibition,
                        model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "substrateInhibition.txt", package = "racas")),
                        sortOptions = sortOptions.substrateInhibition,
                        get_curve_attributes = get_curve_attributes.substrateInhibition,
                        get_saved_fitted_parameters = get_saved_fitted_parameters.substrateInhibition,
                        curveid_query = queriesAndTypeMap$curveSQL,
                        experiment_query = queriesAndTypeMap$experimentSQL,
                        raw_results_persistence_path = 'curvefit/rawdata',
                        typeMap = queriesAndTypeMap$typeMap
  )
  assignInNamespace("substrateInhibition",substrateInhibition, ns="racas")
  
}

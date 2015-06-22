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
    dbType <- getDBType(applicationSettings$server.database.r.driver)
  }
  queryDefinition <- read_json_file(system.file("conf", "definition-ll4.json", package = "racas"))
  curveQueryDefinition <- queryDefinition
  experimentQueryDefinition <- queryDefinition
  curveQueryDefinition$entryPoint <- list(analysis_group = "ag", analysis_group_state = "ags1",analysis_group_value = "curveId", field = "string_value")
  experimentQueryDefinition$entryPoint <- list(experiment = "e", field = "code_name")
  states <- Reduce(function(x,y) rbind(x,y, fill = TRUE, use.names = TRUE), lapply(queryDefinition$analysis_group[[1]]$analysis_group_state, as.data.table))
  values <- flatten_list_in_data_table(states, "analysis_group_value", c("ls_type", "ls_kind"), c("state_type", "state_kind"))
  typeMap <- flatten_list_in_data_table(values, "select", c("state_type", "state_kind","ls_kind"))
  typeMap[field == 'clob_value', lsType := 'clobValue']
  typeMap[field == 'string_value', lsType := 'stringValue']
  typeMap[field == 'code_value', lsType := 'codeValue']
  typeMap[field == 'numeric_value', lsType := 'numericValue']
  ll4 <- ModelFit$new(drc_function = drc::LL.4, 
                      paramNames = c("slope", "min", "max", "ec50"),
                      categorization_function = categorize.LL4,
                      apply_limits = apply_limits.LL4,
                      default_fit_settings = get_default_fit_settings("4 parameter D-R"),
                      simple_to_advanced_fittings_function = updateFitSettings.LL4,
                      model_equation_img = get_text_file_contents(system.file(file.path("rmd","equations"), "ll4.txt", package = "racas")),
                      sortOptions = sortOptions.LL4,
                      get_curve_attributes = get_curve_attributes.LL4,
                      get_saved_fitted_parameters = get_saved_fitted_parameters.LL4,
                      curveid_query = query_definition_list_to_sql(curveQueryDefinition, dbType = dbType),
                      experiment_query = query_definition_list_to_sql(experimentQueryDefinition, dbType = dbType),
                      typeMap = typeMap
  )
  assignInNamespace("ll4",ll4, ns="racas")
  
  queryDefinition <- read_json_file(system.file("conf", "definition-kifit.json", package = "racas"))
  curveQueryDefinition <- queryDefinition
  experimentQueryDefinition <- queryDefinition
  curveQueryDefinition$entryPoint <- list(analysis_group = "ag", analysis_group_state = "ags1",analysis_group_value = "curveId", field = "string_value")
  experimentQueryDefinition$entryPoint <- list(experiment = "e", field = "code_name")
  states <- Reduce(function(x,y) rbind(x,y, fill = TRUE, use.names = TRUE), lapply(queryDefinition$analysis_group[[1]]$analysis_group_state, as.data.table))
  values <- flatten_list_in_data_table(states, "analysis_group_value", c("ls_type", "ls_kind"), c("state_type", "state_kind"))
  typeMap <- flatten_list_in_data_table(values, "select", c("state_type", "state_kind","ls_kind"))
  typeMap[field == 'clob_value', lsType := 'clobValue']
  typeMap[field == 'string_value', lsType := 'stringValue']
  typeMap[field == 'code_value', lsType := 'codeValue']
  typeMap[field == 'numeric_value', lsType := 'numericValue']
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
                        curveid_query = query_definition_list_to_sql(curveQueryDefinition, dbType = dbType),
                        experiment_query = query_definition_list_to_sql(experimentQueryDefinition, dbType = dbType),
                        typeMap = typeMap
  )
  assignInNamespace("kifit",kifit, ns="racas")
  
  mm2 <- ModelFit$new(drc_function = drc::MM.2, 
                      paramNames = c("max", "kd"), 
                      categorization_function = categorize.MM2, 
                      get_reported_parameters = get_reported_parameters.MM2,
                      apply_limits = apply_limits.MM2,
                      default_fit_settings = get_default_fit_settings("MM.2"),
                      simple_to_advanced_fittings_function = updateFitSettings.MM2
  )
  assignInNamespace("mm2",mm2, ns="racas")
  
}

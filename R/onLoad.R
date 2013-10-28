.onLoad <- function(libname, pkgname) {
  appConfName <- paste0(applicationSettings$appName,"_CONFIG")
  appHomeName <- paste0(applicationSettings$appName,"_HOME")
  appConf <- Sys.getenv(appConfName)
  appHome <- Sys.getenv(appHomeName)
  if(appHome == ""  && appConf == "") {
    warning(paste0(appHomeName," and ",appConfName," environment variables not set, not reading application settings.\n\n",
                   "You can set this prior to loading this package by running:\n",
                   "\tSys.setenv(",appHomeName," = \"/path/to/",applicationSettings$appName,"\")\n",
                   "Or\n",
                   "\tSys.setenv(",appConfName," = \"/path/to/",applicationSettings$appName,"_config_file\")\n\n",
                   "Or, you can set either of these in your system environment before loading R. The config file location will take priority over home"),call.=FALSE)
  } else {
    tryCatch({
      if(appHome!="") {
        configFileLocation <- file.path(appHome,"conf/compiled/conf.properties")
        output <- readConfigFile(configFileLocation)
      } else {
        configFileLocation <- file.path(appConf)
        output <- readConfigFile(configFileLocation)
      }
    },error = function(ex) {
      #error <- capture.output(print(ex))
      warning(paste0("Could not load configuration file at:\n\t",
                     configFileLocation,"\n\n",
                     "This was the captured error:\n",
                     "\t",ex),call.=FALSE)
    })
  }
}

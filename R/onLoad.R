.onLoad <- function(libname, pkgname) {
  library("methods") 
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

  
  
}

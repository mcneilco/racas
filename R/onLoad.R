.onLoad <- function(libname, pkgname) {
  appHomeName <- paste0(applicationSettings$appName,"_HOME")
  appHome <- Sys.getenv(appHomeName)
  if(appHome == "") {
    warning(paste0(appHomeName," environment variable not set, not reading application settings.\n\n",
                  "You can set this prior to loading this package by running:\n",
                   "\tSys.setenv(",appHomeName," = \"/path/to/SeuratAddOns\")\n\n",
                   "Or, you can set this in your system environment before loading R"),call.=FALSE)
  } else {
    tryCatch({
        configFileLocation <- paste0(appHome,"/public/src/conf/configurationNode.js")
        output <- readConfigFile(configFileLocation)
      },error = function(ex) {
        #error <- capture.output(print(ex))
        warning(paste0("Could not load configuration file at:\n\t",
                       configFileLocation,"\n\n",
                       "This was the captured error:\n",
                       "\t",ex),call.=FALSE)
      })
  }
}

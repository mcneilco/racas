#' Configuration settings for the applicaiton
#'
#' This data frame is used throughout the applicaiton to point to the database, to get the correct applicaiton name...etc.
#' 
#' 
#' \itemize{
#'   \item appName display name for the applicaiton (ACAS)  
#'   \item db_driver a character string that when evaluated is an object that inherits from \code{\link{dbDriver}} ('PostgreSQL()', 'Oracle()', 'MySQL()', 'JDBC("oracle.jdbc.driver.OracleDriver", "/Users/bbolt/Documents/ojdbc6-11.2.0.2.jar")') 
#'   \item db_user a character string username for the database connection
#'   \item db_password a character string password for the database connection
#'   \item db_name a character string SID/database name for the database connection
#'   \item db_host a character string hostname of database connection
#'   \item db_port a character string port of database connection
#'   \item db_driver_package a character string of the package to instantiate
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with a column for each setting
#' @examples
#' 
#' # Here is an example data frame
#' applicationSettings <- data.frame(
#' appName = "ACAS",               #Application Display Name
#' db_driver = "PostgreSQL()",     #Must be supplied in your own package load (MySQL(), Oracle() supported)
#' db_user = "username",           #ACAS Schema db user
#' db_password = "password",       #ACAS Schema db password
#' db_name = "databasename",       #ACAS Schema db name
#' db_host = "mcneilco.com",       #ACAS Host Name
#' db_port = "5432",               #ACAS Port Number
#' stringsAsFactors = FALSE
#' )
#' # You must assign it to the package namespace in order for it to be used by the package
#' assignInNamespace("applicationSettings",applicationSettings, ns="racas")
applicationSettings <- data.frame(
  appHome = "",
  appName = "ACAS",               #Application Display Name
  db_driver = "PostgreSQL()",     #Must be supplied in your own package load (MySQL(), Oracle() supported)
  db_user = "username",           #ACAS Schema db user
  db_password = "password",       #ACAS Schema db password
  db_name = "databasename",       #ACAS Schema db name
  db_host = "mcneilco.com",       #ACAS Host Name
  db_port = "5432",               #ACAS Port Number
  logDir = as.character(NA),               #ACAS Port Number
  stringsAsFactors = FALSE
)
#' Read a configuration file to racas::applicationSettings
#'
#' @param configLocation The location of the file to read
#' @keywords applicationSettings, config, configuration, configurationNode.js
#' @export
readConfigFile <- function(configLocation, ...) {
  #This function reads a config file and sets the applicationSettings
  replacement <- "\t"
  l <-readLines(file.path(configLocation))
  l <- lapply(l, sub, pattern = "=", replacement = replacement)
  t <- tempfile()
  writeLines(unlist(l), t)
  applicationSettings <- utils::read.table(t, header=FALSE, sep=replacement, row.names=1, strip.white=TRUE, na.strings="NA", stringsAsFactors=FALSE, quote = "")
  unlink(t)
  applicationSettings <- as.data.frame(t(applicationSettings), stringsAsFactors=FALSE)
  
  #Convert "true", "false" to logicals
  logicals <- suppressWarnings(unlist(lapply(applicationSettings, as.logical)))
  applicationSettings[!is.na(logicals)] <- logicals[!is.na(logicals)]
  
  #Convert coercible "8080" values to integers and coercible "1.234" to numeric
  coercibleNumeric <- suppressWarnings(unlist(sapply(applicationSettings, as.numeric)))
  numerics <- which(!is.na(coercibleNumeric) & is.na(logicals) & coercibleNumeric%%1!=0)
  intergers <- which(!is.na(coercibleNumeric) & is.na(logicals) & coercibleNumeric%%1==0)
  applicationSettings[numerics] <- as.numeric(applicationSettings[numerics])
  applicationSettings[intergers] <- as.integer(applicationSettings[intergers])
  
  
  #Convert "null" to ""
  nulls <- applicationSettings=="null"
  applicationSettings[nulls] <- ""
  
  row.names(applicationSettings) <- 1
  
  if (applicationSettings$server.database.r.package != "") {
    if(!suppressWarnings(require(applicationSettings$server.database.r.package, character.only=TRUE))) {
      if(is.null(options("racasInstallDep")[[1]])) {
        installDep <- FALSE
      } else {
        installDep <- as.logical(options("racasInstallDep")[[1]])
      }
      if(installDep) {
        cat(paste0("Attempting to install ",applicationSettings$server.database.r.package))
        method <- ifelse(is.null(options("method")$method),"auto",options("method")$method)
        try(install.packages(applicationSettings$server.database.r.package, repos = options("repos"), method = method))
        try(require(applicationSettings$server.database.r.package, character.only=TRUE))
      } else {
        warnUser(paste0("The database r package \'",applicationSettings$server.database.r.package,"\' is not installed\n",
                      "The query functionality of racas may not work properly\n",
                      "\n\nTo fix this, do one of the following:\n",
                       "restart R and run this line \'options(racasInstallDep = TRUE)\' prior to loading the racas package and racas will attempt to install dependency\n",         
                       "or\n",
                       "install racas again by running install.R located in the conf directory\n",
                        "or\n",
                        "install the package yourself\n"))
      }
    }
  }
  if(!is.null(applicationSettings$server.r.dependencies)) {
    rDependencies <- strsplit(applicationSettings$server.r.dependencies,",")[[1]]
    missing <- unlist(sapply(rDependencies, function(x) length(find.package(x, quiet = TRUE))==0))
    if(any(missing)) {
      warnUser(paste0("Found missing packages in server.r.dependencies list that may cause loss of some racas functionality: ", paste0(names(missing)[missing == TRUE], collapse = ", ")))
    }
  }
  applicationSettings <- validateApplicationSettings(applicationSettings = applicationSettings)
  
  #Add additional settings passed into readConfigFile
  additionalSettings <- list(...)
  applicationSettings[[names(additionalSettings)]] <- unlist(additionalSettings)
  
  utils::assignInNamespace("applicationSettings",applicationSettings, ns="racas")
}

validateApplicationSettings <- function(applicationSettings = racas::applicationSettings) {
  #server.log.path validation
  #Check if set
  currentWD <-  getwd()
  if(is.null(applicationSettings$server.log.path)) {
    warnUser(paste0("applicationSettings$server.log.path is null. Setting to current working directory: ", currentWD))
    applicationSettings$server.log.path <- currentWD
  }
  #Check if exits
  if(!file.exists(applicationSettings$server.log.path)) {
    warnUser(paste0("applicationSettings$server.log.path: \'",applicationSettings$server.log.path, "\' does not exist.  Setting to current working directory: ", currentWD))
    applicationSettings$server.log.path <- currentWD
  }
  #Check writeable
  if(file.access(applicationSettings$server.log.path, mode = 2) != 0) {
    warnUser(paste0("applicationSettings$server.log.path: \'",applicationSettings$server.log.path, "\' is not writeable.  Setting to current directory: ", currentWD))
    applicationSettings$server.log.path <- currentWD
  }
  return(applicationSettings)
}

#' Returns "http://" for blank, null, or FALSE inputs; "https://" for TRUE inputs. 
#'
#' @param clientUseSSL Boolean, default is applicationSettings$client.use.ssl
#' @return Outputs either "http://" or "https://"
#' @keywords applicationSettings, config, configuration, SSL
#' @export
getSSLString <- function(clientUseSSL = applicationSettings$client.use.ssl) {
  # Pulls in clientUseSSL from applicationSettings to see if we heed http:// or https:// in URL
  # Input:   clientUseSSL (boolean)
  # Output:  http:// or https:// (character)
  # Default: http://
  
  if (is.null(clientUseSSL) || clientUseSSL == "" || !clientUseSSL) {
    sslString <- "http://"
  } else if (clientUseSSL) {
    sslString <- "https://"
  } 
  
  return(sslString)
}

cleanPackages <- function(applicationSettings = racas::applicationSettings) {
  output <- capture.output(tryCatch({
    defaultPackages <- c(getOption("defaultPackages"),"base","methods","utils")
    packagesToBeLoaded <- c(unlist(applicationSettings$server.rapache.preloadedpackages),c("racas","rjson", applicationSettings$server.database.r.package))
    currentlyLoadedPackages <- (.packages())
    packagesToUnload <- currentlyLoadedPackages[!currentlyLoadedPackages %in% unique(c(defaultPackages, packagesToBeLoaded))]
    if(length(packagesToUnload) > 0) {
      suppressWarnings(lapply(packagesToUnload, function(k) detach( paste('package:', k, sep='', collapse=''), char=TRUE)))
    }
  }, error  = function(mesage) {
    
  }))
  return(invisible(output))
}
cleanObjects <- function() {
  output <- capture.output(tryCatch({
    allGlobalObjects <- ls(envir = .GlobalEnv, )
    objectsToRemove <- allGlobalObjects[!allGlobalObjects %in% "conn"]
    if(length(objectsToRemove) > 0) {
      rm(list = objectsToRemove, envir = .GlobalEnv)
    }
  }, error  = function(mesage) {
    
  }))
  return(invisible(output))
}
cleanEnvironment <- function(applicationSettings = racas::applicationSettings) {
  cleanPackages(applicationSettings)
  cleanObjects()
  return(invisible(TRUE))
}
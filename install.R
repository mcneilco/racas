#Require
require(tools)

## Collect arguments
args <- commandArgs(TRUE)

#Make sure we are running from the racas source directory
packageDirectory <- read.dcf("DESCRIPTION", fields = c("Package"))
if(packageDirectory != "racas") {
  cat(paste0("Could not find 'racas' DESCRIPTION file in '",getwd(),"', this script must be run from the source directory"))
  q(save = "no")
}
## Default setting when no arguments passed
if(length(args) < 1) {
  args <- c("--help")
}
usage <- function() {
  cat("
      install.R
 
      Arguments:
      --acas_home=path/to/acas_home   - string, path to acas_home
      --help              - print this text
 
      Example:
      Rscript install.R --acas_home=1 \n\n")
  
  q(save="no")
}
## Help section
if("--help" %in% args) {
  usage()
}

## Parse arguments (we expect the form --arg=value)
parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))
names(argsL) <- argsDF$V1

## acas_home default
if(is.null(argsL$acas_home)) {
  usage()
} else {
  acas_home <- path.expand(argsL$acas_home)
  cat(paste0("Using acas_home '",acas_home,"'\n"))
}

#Function to install package from source
install_source <- function(source_file, install_location) {
  originalWD <- getwd()
  on.exit(setwd(originalWD))
  setwd(tempdir())
  build_command <- paste0("R CMD INSTALL --build --preclean -l ",install_location," ",source_file," 2>&1")
  cat(paste0("Building dependent package with command '",build_command,"'\n"))
  system(build_command)
}

#Create lib to install racas
install_location <- path.expand(file.path(acas_home,"r_libs"))
created <- dir.create(install_location,showWarnings = FALSE)

#Build acas source
racas_source_dir <- getwd()
setwd(tempdir())
build_command <- paste0("R CMD INSTALL --build --preclean -l ",install_location," ",racas_source_dir," 2>&1")
cat(paste0("Building racas with command '",build_command,"'\n"))
output <- system(build_command, intern = TRUE)
setwd(racas_source_dir)
racas_build_line <- grep("racas_.*.tgz",output)
acas_build_text <- output[racas_build_line]
matches <- regexpr("racas_.*.tgz", acas_build_text)
racas_file <- regmatches(acas_build_text,matches)
cat(paste0("Built package '", racas_file, "'\n"))

#Get dependent package source locations and build them into to temp repo
package_sources <- list.files(file.path(getwd(),"packrat/src"), pattern = ".tar.gz", recursive = TRUE, full.names = TRUE)
installed_packages <- installed.packages(install_location, fields = c("Package","Version"))
installed_packages <- paste0(installed_packages[, "Package"],"_",installed_packages[, "Version"])
valid_database_packages <- c("RPostgreSQL","RJDBC", "ROracle", "RMySQL")
database_package_sources <- lapply(package_sources, 
                                   function(x) {
                                     contents <- untar(x, list = TRUE)
                                     description <- contents[which(basename(contents) == "DESCRIPTION")]
                                     description <- description[which.min(nchar(description))]
                                     untar(x, files = description, exdir = tempdir())
                                     package_version <- read.dcf(file.path(tempdir(),description), fields = c("Package","Version"))
                                     package <- package_version[,"Package"]
                                     package_version <- paste0(package_version[,"Package"],"_",package_version[,"Version"])
                                     database_package <- NULL
                                     if(package %in% valid_database_packages) {
                                       database_package <- list(package = package, package_version = package_version, source = x, already_installed = package_version %in% installed_packages)
                                     } else {
                                       if(!package_version %in% installed_packages) {
                                         install_source(x, install_location)
                                       } else {
                                         cat(paste0("Skipping ",package_version, " already installed\n"))
                                       }
                                     }
                                     return(database_package)
                                   }
)

database_package_sources[which(unlist(lapply(database_package_sources, is.null)))] <- NULL
library(racas, lib = install_location)
if(!is.null(applicationSettings$server.database.r.package)) {
  if(applicationSettings$server.database.r.package %in% valid_database_packages) {
    matching_source_package <- unlist(lapply(database_package_sources, function(x) x$package == applicationSettings$server.database.r.package))
    if(any(matching_source_package)) {
      source_package <- database_package_sources[which(matching_source_package)][[1]]
      if(!source_package$already_installed) {
        cat("Installing database package ", source_package$package, "\n")
        install_source(source_package$source, install_location)
    } else {
      cat(paste0("Skipping database package '", source_package$package_version, "' already installed\n"))
    }
    } else {
      cat(paste0("Could not find source package for '", applicationSettings$server.database.r.package,"'\n"))
    }

  } else {
    cat(paste0("Database package '", applicationSettings$server.database.r.package, "' is not in valid database packages: ", paste(valid_database_packages, collapse = ", "),"\n"))
  }
} else {
  cat("Config setting server.database.r.package is NULL not installing database package\n")
}

cat("Finished installation of racas\n")

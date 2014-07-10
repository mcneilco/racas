#Require
require(tools)

## Collect arguments
args <- commandArgs(TRUE)

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
      ./install.R --acas_home=1 \n\n")
  
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
  cat(paste0("Using acas_home \'",acas_home,"\'\n"))
}

#Create lib to install racas
installLib <- file.path(acas_home,"r_libs")
created <- dir.create(installLib)

#Build acas source
build_command <- "R CMD INSTALL --build --preclean . 2>&1"
cat(paste0("Building racas with command \'",build_command,"\'"))
output <- system("R CMD INSTALL --build --preclean . 2>&1", intern = TRUE)
racasBuildLine <- grep("racas_.*.tgz",output)
acasBuildText <- output[racasBuildLine]
matches <- regexpr("racas_.*.tgz", acasBuildText)
racasFile <- regmatches(acasBuildText,matches)
cat(paste0("Built package \'", racasFile, "\'\n"))

#Create Temporary R REPO
tempRepoDir <- tempdir()
cat(paste0("Creating temporary R repo \'", tempRepoDir, "\'\n"))
contribDirPath <- file.path(contrib.url(tempRepoDir))
contribDirCreated <- dir.create(path = contribDirPath, recursive = TRUE)

#Copy racas source to temporary R Repo
copied <- file.copy(racasFile,contribDirPath)

#Get dependent package source locations and build them into to temp repo
packageSources <- list.files(file.path(getwd(),"packrat/src"), pattern = ".tar.gz", recursive = TRUE, full.names = TRUE)
copied <- lapply(packageSources, 
                 function(x) {
                   cat(x,"\n")
                   originalWD <- getwd()
                   on.exit(setwd(originalWD))
                   setwd(contribDirPath)
                   system(paste0("R CMD INSTALL -l ",tempRepoDir," --build ",x))
                 }
)

#Write packages file in temp repo
tools::write_PACKAGES(dir = contribDirPath, subdirs=TRUE, type=getOption("pkgType"), verbose=TRUE)

#Install racas
contriburl <- paste0("file://",contribDirPath)
cat(paste0("Installing racas to \'",installLib,"\'\n"))
system("sleep 10000")
dir.create(file.path(tempRepoDir,"src/contrib"), recursive = TRUE)
cat(tempRepoDir,"\n")
install.packages("racas", repos = NULL, contriburl = contribDirPath, lib = installLib)


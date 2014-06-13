getRacasPackageDependencies <- function() {
  library(tools)
  recommended_and_base_packages <- as.character(installed.packages()[installed.packages()[,"Priority"] %in% c("recommended", "base"),][, "Package"])
  racasDirectDependenciesAll <- package_dependencies("racas", installed.packages(), which = c("Depends", "Imports", "LinkingTo", "Suggests"), recursive = FALSE)$racas
  racasDirectDependenciesAll <- racasDirectDependenciesAll[! (racasDirectDependenciesAll %in% recommended_and_base_packages)]
  directDependencies_no_suggests <- unique(as.character((unlist(package_dependencies(racasDirectDependenciesAll, installed.packages(), which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))))
  racasReverseDependencies <- directDependencies_no_suggests[! (directDependencies_no_suggests %in% recommended_and_base_packages)]
  allRacasDependencies <- unique(c(racasDirectDependenciesAll, racasReverseDependencies))
  allRacasDependenciesWithVersions <- installed.packages()[row.names(installed.packages()) %in% allRacasDependencies,]
  return(allRacasDependenciesWithVersions)
}

downloadRacasDependencies <- function(repos = "http://cran.rstudio.com") {  
  racasDepends <- getRacasPackageDependencies()
  
  pkgs <- row.names(racasDepends)
  versions <- racasDepends[ ,"Version"]
  TempPackages <- data.frame(pkgs, versions)
  
  available <- available.packages(contriburl = 
                                    contrib.url(repos = repos, type = "source"))
  available <- data.frame(unique(available[, c("Package", "Version")]))
  names(available) <- c("pkgs", "versions")
  available$pkgs <- as.character(available$pkgs)
  available$versions <- as.character(available$versions)
  
  getPackage <- function(x){
    Matched <- merge(x, available, all = FALSE)
    
    if (nrow(Matched) == 1){
      from <- paste0(repos, "/src/contrib/", x[, 1], "_", x[,2], ".tar.gz")
    } else {
      from <- paste0(repos, "/src/contrib/Archive/", x[, 1], "/", x[, 1], "_", x[,2], ".tar.gz")
    }
    TempFile <- file.path(tempdir(),paste0(x[, 1], "_", x[,2], ".tar.gz"))
    download.file(url = from, destfile = TempFile)
    return(TempFile)
  }
  
  packageSources <- unlist(lapply(1:nrow(TempPackages), function(x) getPackage(TempPackages[x,])))
  #install.packages(packageSources, repos = NULL, type = "source", lib = lib)
  #unlink(packageSources)
  return(packageSources)
}

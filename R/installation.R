getPackageDependencies <- function(packages) {
  library(tools)
  recommended_and_base_packages <- as.character(installed.packages()[installed.packages()[,"Priority"] %in% c("recommended", "base"),][, "Package"])
  directDependenciesAll <- unlist(package_dependencies(packages, installed.packages(), which = c("Depends", "Imports", "LinkingTo", "Suggests"), recursive = FALSE))
  directDependenciesAll <- directDependenciesAll[! (directDependenciesAll %in% recommended_and_base_packages)]
  directDependencies_no_suggests <- unique(as.character((unlist(package_dependencies(directDependenciesAll, installed.packages(), which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))))
  reverseDependencies <- directDependencies_no_suggests[! (directDependencies_no_suggests %in% recommended_and_base_packages)]
  allDependencies <- unique(c(directDependenciesAll, reverseDependencies))
  allDependenciesWithVersions <- installed.packages()[row.names(installed.packages()) %in% allDependencies,]
  return(allDependenciesWithVersions)
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

plotDependencies <- function(packages = c("racas")) {
  library(igraph)
  library(tools)
  library(miniCRAN)
  pkgdata <- available.packages()
  pkgList <- unlist(package_dependencies(packages, installed.packages(), which = c("Depends", "Imports"), recursive = FALSE))
  p <- makeDepGraph(pkgList, availPkgs=pkgdata)
  
  plotColours <- c("grey80", "orange")
  topLevel <- as.numeric(V(p)$name %in% packages)
  
  par(mai=rep(0.25, 4))
  
  set.seed(50)
  vColor <- plotColours[1 + topLevel]
  plot(p, vertex.size=8, edge.arrow.size=0.5, 
       vertex.label.cex=0.7, vertex.label.color="black", 
       vertex.color=vColor,
       main = NULL)
  legend(x=0.9, y=-0.9, legend=c("Dependencies", "Initial list"), 
         col=c(plotColours, NA), pch=19, cex=0.9)
  text(0.9, -0.75, expression(xts %->% zoo), adj=0, cex=0.9)
  text(0.9, -0.8, "xts depends on zoo", adj=0, cex=0.9)
  title("Package dependency graph")
}

makeRepo <- function(path = "./repo", description = "DESCRIPTION", racasPath = ".") {
  pkgs <- descriptionDeps(description)
  pkgs <- unique(c(unlist(tools::package_dependencies(pkgs, available.packages(type = "source"), recursive = TRUE)), pkgs))
  pkgs <- pkgs[order(pkgs)]
  if(file.exists(path)) {
    unlink(path, force = TRUE, recursive = TRUE)
  }
  dir.create(path)
  makeRep(pkgs, path = path, type = "source")
  originalWD <- getwd()
  on.exit(setwd(originalWD))
  setwd(file.path(normalizePath(path), "src", "contrib"))
  system(paste0("R CMD build ", normalizePath(racasPath)))
  setwd(originalWD)
  updateRepoIndex(path)
}

descriptionDeps <- function(descriptionPath) {
  description <- as.data.frame(read.dcf(descriptionPath))
  dependencies <- list(as.character(description$Depends), as.character(description$Suggests), as.character(description$Imports))
  dependencies[unlist(lapply(dependencies, length))==0] <- NULL
  dependencies <- paste0(dependencies, collapse = "\n")
  dependencies <- gsub(",","",dependencies)
  dependencies <- strsplit(dependencies, "\n")[[1]]
  dependencies <- dependencies[dependencies != ""]
  dependencies <- dependencies[order(dependencies)]
  return(dependencies)
}


makeRep <- function(pkgs, path, repos=getOption("repos"), type="source",
                    Rversion=R.version, download=TRUE, writePACKAGES=TRUE, quiet=FALSE) {
  if(!file.exists(path)) stop("Download path does not exist")
  pkgPath <- repoBinPath(path=path, type=type, Rversion=Rversion)
  if(!file.exists(pkgPath)) {
    result <- dir.create(pkgPath, recursive=TRUE, showWarnings = FALSE)
    if(result) {
      if(!quiet) message("Created new folder: ", pkgPath)
    } else {
      stop("Unable to create repo path: ", pkgPath)
    }
  }
  
  pdb <- pkgAvail(repos = repos, type=type, Rversion = Rversion)
  
  if(download) utils::download.packages(pkgs, destdir=pkgPath, available=pdb, repos=repos, 
                                        contriburl = contribUrl(repos, type, Rversion),
                                        type=type, quiet=quiet)
  if(writePACKAGES) updateRepoIndex(path=path, type=type, Rversion=Rversion)
}
repoBinPath <- function(path, type, Rversion){
  normalizePath(file.path(path, repoPrefix(type, Rversion)), mustWork = FALSE, winslash = "/")
}

pkgAvail <- function(repos=getOption("repos"), type="source", Rversion = R.version){
  if(!grepl("^http://|file:///", repos[1]) && file.exists(repos[1])) {
    repos <- paste0("file:///", normalizePath(repos[1], mustWork = FALSE, winslash = "/"))
  } else {
    if(!is.null(names(repos)) && repos["CRAN"] == "@CRAN@"){
      repos <- c(CRAN="http://cran.revolutionanalytics.com")
    }
  }
  utils::available.packages(contribUrl(repos, type=type, Rversion = Rversion), type=type, filters=list())
}
updateRepoIndex <- function(path, type="source", Rversion=R.version) {
  lapply(type, function(type){
    pkgPath <- repoBinPath(path=path, type=type, Rversion=Rversion)
    if(grepl("mac.binary", type)) type <- "mac.binary"
    tools::write_PACKAGES(dir=pkgPath, type=type)
  })
}

repoPrefix <- function(type, Rversion){
  Rversion = twodigitRversion(Rversion)
  switch(
    type,
    "source" = "src/contrib",
    "win.binary" = sprintf("bin/windows/contrib/%s", Rversion),
    "mac.binary" = sprintf("bin/macosx/contrib/%s", Rversion),
    "mac.binary.mavericks" =  sprintf("bin/macosx/mavericks/contrib/%s", Rversion),
    "mac.binary.leopard"= sprintf("bin/macosx/leopard/contrib/%s", Rversion),
    stop("Type ", type, "not recognised.")
  )
}
twodigitRversion <- function(R=R.version){
  if ("simple.list" %in% class(R)) {
    paste(R$major, strsplit(R$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
  } else if ("R_system_version" %in% class(R)) {
    paste(strsplit(as.character(R), ".", fixed=TRUE)[[1L]][1L:2L], collapse=".")
  } else if (is.character(R)) {
    paste(strsplit(R, ".", fixed=TRUE)[[1L]][1L:2L], collapse=".")
  }
}

contribUrl <- function (repos, type = getOption("pkgType"), Rversion = R.version) {
  Rversion <- twodigitRversion(Rversion)
  if (type == "both") 
    type <- "source"
  if (type == "binary") 
    type <- .Platform$pkgType
  if (is.null(repos)) 
    return(NULL)
  if ("@CRAN@" %in% repos && interactive()) {
    cat(gettext("--- Please select a CRAN mirror for use in this session ---"), 
        "\n", sep = "")
    flush.console()
    chooseCRANmirror()
    m <- match("@CRAN@", repos)
    nm <- names(repos)
    repos[m] <- getOption("repos")["CRAN"]
    if (is.null(nm)) 
      nm <- rep("", length(repos))
    nm[m] <- "CRAN"
    names(repos) <- nm
  }
  if ("@CRAN@" %in% repos) 
    stop("trying to use CRAN without setting a mirror")
  ver <- Rversion
  mac.path <- "macosx"
  if (substr(type, 1L, 11L) == "mac.binary.") {
    mac.path <- paste(mac.path, substring(type, 12L), sep = "/")
    type <- "mac.binary"
  }
  res <- switch(type, 
                source = paste(gsub("/$", "", repos), "src", "contrib", sep = "/"), 
                mac.binary = paste(gsub("/$", "", repos), "bin", mac.path, "contrib", ver, sep = "/"), 
                win.binary = paste(gsub("/$", "", repos), "bin", "windows", "contrib", ver, sep = "/"))
  res
}

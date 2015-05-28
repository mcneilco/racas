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
  miniCRAN::makeRepo(pkgs, path = path, type = "source")
  originalWD <- getwd()
  on.exit(setwd(originalWD))
  setwd(file.path(normalizePath(path), "src", "contrib"))
  system(paste0("R CMD build ", normalizePath(racasPath)))
  setwd(originalWD)
  miniCRAN::updateRepoIndex(path)
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

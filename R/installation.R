descriptionDeps <- function(descriptionPath, dependencySections = c("Imports")) {
  description <- as.data.frame(read.dcf(descriptionPath), stringsAsFactors = FALSE)
  dependencies <- as.list(description[c(dependencySections)])
  dependencies[unlist(lapply(dependencies, length))==0] <- NULL
  dependencies <- paste0(dependencies, collapse = "\n")
  dependencies <- gsub(",","",dependencies)
  dependencies <- strsplit(dependencies, "\n")[[1]]
  dependencies <- dependencies[dependencies != ""]
  dependencies <- dependencies[order(dependencies)]
  return(dependencies)
}

installDeps <- function(description = "DESCRIPTION", skipInstalled = TRUE, type = getOption("pkgType"), sections = c("Imports")) {
  if (identical(type, "both")) {
    type <- "binary"
  }
  pkgs <- descriptionDeps(description, dependencySections = sections)
  pkgs <- pkgs[order(pkgs)]
  if(skipInstalled) {
    installed <- row.names(installed.packages())
    pkgs <- pkgs[!pkgs %in% installed]
  }
  if(length(pkgs) > 0) {
    install.packages(pkgs, repos = "https://cran.microsoft.com/snapshot/2016-12-20", type = type, method='wget')
  } else
    message("skipping, dependencies already installed")
}

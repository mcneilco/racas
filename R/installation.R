getRacasPackageDependencies <- function() {
  recommended_and_base_packages <- as.character(installed.packages()[installed.packages()[,"Priority"] %in% c("recommended", "base"),][, "Package"])
  racasDirectDependenciesAll <- package_dependencies("racas", installed.packages(), which = c("Depends", "Imports", "LinkingTo", "Suggests"), recursive = FALSE)$racas
  racasDirectDependenciesAll <- racasDirectDependenciesAll[! (racasDirectDependenciesAll %in% recommended_and_base_packages)]
  directDependencies_no_suggests <- unique(as.character((unlist(package_dependencies(racasAll, installed.packages(), which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))))
  racasReverseDependencies <- directDependencies_no_suggests[! (directDependencies_no_suggests %in% recommended_and_base_packages)]
  allRacasDependencies <- unique(c(racasDirectDependenciesAll, racasReverseDependencies))
  return(allRacasDependencies)
}
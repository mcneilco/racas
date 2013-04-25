applicationSettings <- data.frame(
  appName = "ACAS",               #Application Display Name
  db_driver = "PostgreSQL()",     #Must be supplied in your own package load (MySQL(), Oracle() supported)
  db_user = "username",           #ACAS Schema db user
  db_password = "password",       #ACAS Schema db password
  db_name = "databasename",       #ACAS Schema db name
  db_host = "mcneilco.com",       #ACAS Host Name
  db_port = "5432",               #ACAS Port Number
  stringsAsFactors = FALSE
)

readConfigFile <- function(configLocation) {
  #This function reads a config file and sets the applicationSettings
  configFile <- readLines(configLocation)
  configurations <- configFile[grepl("^SeuratAddOns\\.configuration\\.",configFile)]
  configList <- gsub("SeuratAddOns\\.configuration\\.(.*) = (.*)", "\\2", configurations)
  applicationSettings <- as.data.frame(as.list(gsub("\"","",configList)), stringsAsFactors=FALSE)
  names(applicationSettings) <- gsub("SeuratAddOns\\.configuration\\.(.*) = (.*)", "\\1", configurations)
  if (!is.null(applicationSettings$db_driver_package)) {
    eval(parse(text = applicationSettings$db_driver_package))
  }
  assignInNamespace("applicationSettings",applicationSettings, ns="racas")
}

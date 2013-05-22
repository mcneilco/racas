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
  appName = "ACAS",               #Application Display Name
  db_driver = "JDBC('oracle.jdbc.driver.OracleDriver','/Users/bbolt/Documents/clients/nexval/SeuratAddOns/public/src/modules/GenericDataParser/src/server/ojdbc6.jar')",     #Must be supplied in your own package load (MySQL(), Oracle() supported)
  db_user = "acas_dev",           #ACAS Schema db user
  db_password = "acas_dev_password",       #ACAS Schema db password
  db_name = "osl",       #ACAS Schema db name
  db_host = "ora.labsynch.com",       #ACAS Host Name
  db_port = "1521",               #ACAS Port Number
  db_driver_package = "require(RJDBC)",
  stringsAsFactors = FALSE
)
#' Query the application data server.
#'
#' This function queries the acas databse specified in the variable \code{\link{applicationSettings}} and
#' returns the result as a data.frame
#'
#' @param qu a sql query character string
#' @param globalConnect should the query assume a global conn variable and return one to the global namespace?
#' @param ... expressions fed to \code{\link{dbGetQuery}} or \code{\link{dbDisconnect}}
#' @return A data frame result from the query
#' @keywords query
#' @export
#' @examples
#' result <- query("select * from api_curve_params")
#' # conn
#' # Error: object 'conn' not found
#' result <- query("select * from api_curve_params", globalConnect=TRUE)
#' # conn
#' # <PostgreSQLConnection:(96699,1)> 
#' 
#' # The globalConnect option:
#' # The first query using the global connect option will create a conn varable in the global namespace
#' system.time(result <- query("select * from api_curve_params", globalConnect=TRUE))
#' # user  system elapsed 
#' # 0.010   0.002   0.784 
#' # The second query will use this global variable instead of opening a new connection, which makes subsequent queryies faster
#' # If the connection is expired or closes, the globalConnect option will create a new connection
#' system.time(result <- query("select * from api_curve_params", globalConnect=TRUE))
#' # user  system elapsed 
#' # 0.007   0.001   0.468 
#' 
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

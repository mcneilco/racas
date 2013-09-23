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
query <- function(qu, globalConnect=FALSE, ...) {
  isSend <- grepl("^UPDATE|^CREATE|^DELETE|^DROP|^INSERT|^ALTER",toupper(sub("^\\s+", "", qu)))
  if(!globalConnect) {
    conn <- getDatabaseConnection(...)
    jdbcConn <- class(conn)=="JDBCConnection"
  }
  result <- tryCatch({
    if(isSend && jdbcConn) {
      result <- RJDBC::dbSendUpdate(conn, qu)
      return(TRUE)
    } else {
      result <- DBI::dbGetQuery(conn,qu)
      return(result)
    }
  },
  error = function(ex) {
    if(globalConnect) {
      conn <<- getDatabaseConnection(...)
      jdbcConn <- class(conn)=="JDBCConnection"
      tryCatch({
        if(isSend && jdbcConn) {
          result <- RJDBC::dbSendUpdate(conn, qu)
          return(TRUE)
        } else {
         result <- DBI::dbGetQuery(conn,qu)
         return(result)
        }
      },
      error = function(ex) {
        errorHandler(ex, conn, driver)
      })
    } else {
      errorHandler(ex, conn, driver)
    }
  }, finally = {
    if(!globalConnect) {
      DBI::dbDisconnect(conn, ...)
    }
  })
  return(result)
}
getDatabaseConnection <- function(applicationSettings = racas::applicationSettings) {
  driver <- eval(parse(text = applicationSettings$db_driver))
  conn <- switch(class(driver),
                 "OraDriver" = DBI::dbConnect(driver, dbname=paste0(applicationSettings$db_host,":",applicationSettings$db_port,"/",applicationSettings$db_name), user=applicationSettings$db_user, pass=applicationSettings$db_password),
                 "PostgreSQLDriver" = DBI::dbConnect(driver , user= applicationSettings$db_user, password=applicationSettings$db_password, dbname=applicationSettings$db_name, host=applicationSettings$db_host, port=applicationSettings$db_port),
                 "MySQLDriver" = DBI::dbConnect(driver , user= applicationSettings$db_user, password=applicationSettings$db_password, dbname=applicationSettings$db_name, host=applicationSettings$db_host, port=applicationSettings$db_port),
                 "JDBCDriver" = DBI::dbConnect(driver, paste0(getDBString(applicationSettings$db_driver),applicationSettings$db_host,":",applicationSettings$db_port,":",applicationSettings$db_name), user=applicationSettings$db_user, pass=applicationSettings$db_password),
                 class(driver)
  )
  return(conn)
}
errorHandler <- function(ex, conn, driver) {
  if(class(conn)=="character") {
    if(conn==class(driver)) {
      print("Unrecognized driver class '",class(driver),"', racas::applicationSettings$db_driver '",parse(text = racas::applicationSettings$db_driver), "' evals to class ", class(driver),", must evaluate to a known driver class\n see ?racas:::query")
      return(list(success = FALSE, error = ex))
    }
  } else{
    print(ex)
    return(list(success = FALSE, error = ex))
  }
}

getDBType <- function(db_driver = racas::applicationSettings$db_driver) {
  
  driver <- eval(parse(text = db_driver))
  dbType <- switch(class(driver),
                   "OraDriver" = "Oracle",
                   "PostgreSQLDriver" = "Postgres",
                   "MySQLDriver" = "MySQL",
                   "JDBCDriver" = "JDBC",
  )
  if(!is.null(dbType)){
    if(dbType=="JDBC") {
      supportedDBs <- c("oracle", "postgres", "mysql")
      db <- supportedDBs[unlist(lapply(supportedDBs, grepl, x = paste(unlist(capture.output(driver)), collapse = " "), ignore.case = TRUE))][1]
      dbType <- switch(db,
                       "oracle" = "Oracle",
                       "postgres" = "Postgres",
                       "mysql" = "MySQL"
      )
    }
  }
  return(dbType)
}
getDBString <- function(driverString) {
  dbString <- switch(getDBType(driverString),
                     "Oracle" = "jdbc:oracle:thin:@",
                     "Postgres" = "jdbc:postgresql://",
                     "MySQ" = "jdbc:mysql://"
  )
  return(dbString)
}

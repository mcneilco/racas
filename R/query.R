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
  driver <- eval(parse(text = applicationSettings$server.database.r.driver))
  conn <- switch(class(driver),
                 "OraDriver" = DBI::dbConnect(driver, dbname=paste0(applicationSettings$server.database.host,":",applicationSettings$server.database.port,"/",applicationSettings$server.database.name), user=applicationSettings$server.database.username, pass=applicationSettings$server.database.password),
                 "PostgreSQLDriver" = DBI::dbConnect(driver , user= applicationSettings$server.database.username, password=applicationSettings$server.database.password, dbname=applicationSettings$server.database.name, host=applicationSettings$server.database.host, port=applicationSettings$server.database.port),
                 "MySQLDriver" = DBI::dbConnect(driver , user= applicationSettings$server.database.username, password=applicationSettings$server.database.password, dbname=applicationSettings$server.database.name, host=applicationSettings$server.database.host, port=applicationSettings$server.database.port),
                 "JDBCDriver" = DBI::dbConnect(driver, paste0(getDBString(applicationSettings$server.database.r.driver),applicationSettings$server.database.host,":",applicationSettings$server.database.port,":",applicationSettings$server.database.name), user=applicationSettings$server.database.username, pass=applicationSettings$server.database.password),
                 class(driver)
  )
  return(conn)
}
errorHandler <- function(ex, conn, driver) {
  if(class(conn)=="character") {
    if(conn==class(driver)) {
      print("Unrecognized driver class '",class(driver),"', racas::applicationSettings$server.database.r.driver '",parse(text = racas::applicationSettings$server.database.r.driver), "' evals to class ", class(driver),", must evaluate to a known driver class\n see ?racas:::query")
      return(list(success = FALSE, error = ex))
    }
  } else{
    print(ex)
    return(list(success = FALSE, error = ex))
  }
}

getDBType <- function(server.database.r.driver = racas::applicationSettings$server.database.r.driver) {
  
  driver <- eval(parse(text = server.database.r.driver))
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
sqliz <- function(vector) {
  if (class(vector)=="numeric") {
    sql <- paste(vector, collapse = ',')
    return(sql)
  }  else {
    sql <- paste0("'",paste(vector, collapse = "','"), "'")
    return(sql)
  }
}

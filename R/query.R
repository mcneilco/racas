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
  isUpdate <- grepl("^UPDATE",toupper(sub("^\\s+", "", qu)))
  if(!globalConnect) {
    conn <- getDabaseConnection()
    jdbcConn <- class(conn)=="JDBCConnection"
  }
  result <- tryCatch({
    if(isUpdate && jdbcConn) {
      result <- RJDBC::dbSendUpdate(conn, qu, ...)
      return(TRUE)
    } else {
      result <- DBI::dbGetQuery(conn,qu, ...)
      return(result)
    }
  },
  error = function(ex) {
    if(globalConnect) {
      conn <<- getDabaseConnection()
      jdbcConn <- class(conn)=="JDBCConnection"
      tryCatch({
        if(isUpdate && jdbcConn) {
          result <- RJDBC::dbSendUpdate(conn, qu, ..)
          return(TRUE)
        } else {
         result <- DBI::dbGetQuery(conn,qu, ...)
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
getDabaseConnection <- function() {
  getDBString <- function(driverString) {
    supportedDBs <- c("oracle", "postgres", "mysql")
    db <- supportedDBs[unlist(lapply(supportedDBs, grep, x = driverString))[1]]
    dbString <- switch(db,
                       "oracle" = "jdbc:oracle:thin:@",
                       "postgres" = "jdbc:postgresql://",
                       "mysql" = "jdbc:mysql://"
    )
    return(dbString)
  }
  driver <- eval(parse(text = racas::applicationSettings$db_driver))
  conn <- switch(class(driver),
                 "OraDriver" = DBI::dbConnect(driver, dbname=paste0(racas::applicationSettings$db_host,":",racas::applicationSettings$db_port,"/",racas::applicationSettings$db_name), user=racas::applicationSettings$db_user, pass=racas::applicationSettings$db_password),
                 "PostgreSQLDriver" = DBI::dbConnect(driver , user= racas::applicationSettings$db_user, password=racas::applicationSettings$db_password, dbname=racas::applicationSettings$db_name, host=racas::applicationSettings$db_host, port=racas::applicationSettings$db_port),
                 "MySQLDriver" = DBI::dbConnect(driver , user= racas::applicationSettings$db_user, password=racas::applicationSettings$db_password, dbname=racas::applicationSettings$db_name, host=racas::applicationSettings$db_host, port=racas::applicationSettings$db_port),
                 "JDBCDriver" = DBI::dbConnect(driver, paste0(getDBString(racas::applicationSettings$db_driver),racas::applicationSettings$db_host,":",racas::applicationSettings$db_port,":",racas::applicationSettings$db_name), user=racas::applicationSettings$db_user, pass=racas::applicationSettings$db_password),
                 class(driver)
  )
  return(conn)
}
errorHandler <- function(ex, conn, driver) {
  if(class(conn)=="character") {
    if(conn==class(driver)) {
      print("Unrecognized driver class '",class(driver),"', racas::applicationSettings$db_driver '",parse(text = racas::applicationSettings$db_driver), "' evals to class ", class(driver),", must evaluate to a known driver class\n see ?racas:::query")
      return(NULL)
    }
  } else{
    print(ex)
    return(NULL)
  }
}
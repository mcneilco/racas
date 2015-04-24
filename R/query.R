#' Query the application data server.
#'
#' This function queries the acas databse specified in the variable \code{\link{applicationSettings}} and
#' returns the result as a data.frame
#'
#' @param qu a sql query character string
#' @param globalConnect should the query assume a global conn variable and return one to the global namespace?
#' @param conn a database connection to use for the query instead of opening a new connection
#' @param catchError logical on error throw error or return list object with success = false, message = character
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
query <- function(qu, globalConnect=FALSE, conn = NULL, catchError = TRUE, send, ...) {
  if(missing(send)) {
    isSend <- grepl("^UPDATE|^CREATE|^DELETE|^DROP|^INSERT|^ALTER",toupper(sub("^\\s+", "", qu)))
  } else {
    isSend <- send
  }
  closeConnectionOnWaytOut <- !globalConnect & is.null(conn)
  on.exit({if(closeConnectionOnWaytOut) {DBI::dbDisconnect(conn)}})
  if(is.null(conn)) {
    if(!globalConnect) {
      conn <- getDatabaseConnection(...)
    } else {
      conn <- try(get("conn",envir = .GlobalEnv), silent = TRUE)
      isAliveConnection <- try(dbGetInfo(conn),silent = TRUE)
      if(class(isAliveConnection) == "try-error") {
        conn <- getDatabaseConnection(...)
        conn <<- conn
      }
    }
  }
  jdbcConn <- class(conn)=="JDBCConnection"
  result <- tryCatch({
    if(isSend) {
      if(jdbcConn) {
        result <- RJDBC::dbSendUpdate(conn, qu)
        return(TRUE)
      } else {
        result <- DBI::dbSendQuery(conn, qu)
        return(TRUE)
      }
    } else {
      result <- DBI::dbGetQuery(conn,qu)
      return(result)
    }
  },
  error = function(ex) {
    if(catchError) {
      return(list(success = FALSE, error = ex))
    } else {
      stop(ex)
    }
  })
  return(result)
}
getAllDatabaseConnections <- function(dbDriver = racas::applicationSettings$server.database.r.driver) {
  driver <- eval(parse(text = applicationSettings$server.database.r.driver))
  dbConnectionsList <- DBI::dbListConnections(driver)
  return(dbConnectionsList)
}
closeAllDatabaseConnections <- function(...) {
  databaseConnections <- getAllDatabaseConnections(...)
  lapply(databaseConnections, DBI::dbDisconnect)
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

getDBType <- function(server.database.r.driver = racas::applicationSettings$server.database.r.driver, conn = NULL) {
  if(is.null(conn)) {
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
  } else {
    dbType <- switch(class(conn),
           "OraConnection" = "Oracle",
           "PostgreSQLConnection" = "Postgres",
           "MySQLConnection" = "MySQL",
           "JDBCConnection" = "JDBC"
           )
    if(is.null(dbType)){
      stop("determination of db connection type not implmented for JDBC connections")
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
  if (class(vector) %in% c("numeric","integer")) {
    sql <- paste(vector, collapse = ',')
    return(sql)
  }  else {
    sql <- paste0("'",paste(vector, collapse = "','"), "'")
    return(sql)
  }
}
#' Replace character in query string with values over n times ceiling(values/limit) and return query results for each resulting query
#'
#' Function originally created to get around oracle in clause query (1000).  Calls \code{\link{query}} after replacing a specified character with a subset of values
#'
#' @param qu a sql query character string that contains the character given in string parameter
#' @param string a character string to replace with values parameter
#' @param values a list of values to replace in string parameter
#' @param ... expressions fed to \code{\link{query}}
#' @return A list objects with results from query
#' @keywords query, inclause, oracle, workaround
#' @export
#' @examples
#' query_replace_string_with_values(qu = "select * from analysis_group_value where id in (REPLACEME)", string = "REPLACEME", values = c(1:1001))
#' 
query_replace_string_with_values <- function(qu, string, values, limit = 999, ...) {
  # Queries DNET instead of exampleClient's ACAS
  if(!missing(string)) {
    subString <- string
    valueList <- values
    split <- split(valueList, ceiling(seq_along(valueList)/limit))
    results <- lapply(split, function(x) query(qu = gsub(subString,sqliz(x), qu), ...))
  } else {
    results <- list(query(qu, ...))
  }
  return(results)
}

dbExistsTable <- function(conn, name, schema = NA) {
  dbType <- getDBType(conn = conn)
  exists <- switch(dbType,
         "Postgres" = {
           if(is.na(schema)) schema <- dbGetQuery(conn, "SELECT current_schema()")[[1]]  
           dbGetQuery(conn,paste0("SELECT EXISTS (
                                        SELECT 1
                                        FROM   information_schema.tables 
                                        WHERE  lower(table_schema) = '",tolower(schema),"'
                                        AND    lower(table_name) = '",tolower(name),"')"
                                  ))[[1]]
         },
         "Oracle" = {
           if(is.na(schema)) schema <- dbGetQuery(conn, "select sys_context( 'userenv', 'current_schema' ) from dual")[[1]]  
           DBI::dbExistsTable(conn, name, schema) || DBI::dbExistsTable(conn, toupper(name), toupper(schema)) || DBI::dbExistsTable(conn, tolower(name), tolower(schema))
         },
         "MySQL" = {
           if(is.na(schema)) schema <- dbGetQuery(conn, "SELECT SCHEMA()")[[1]]  
           DBI::dbExistsTable(conn, name, schema) || DBI::dbExistsTable(conn, toupper(name), toupper(schema)) || DBI::dbExistsTable(conn, tolower(name), tolower(schema))           
         }      
  )
  return(exists)
}

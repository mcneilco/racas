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
query <- function(qu, globalConnect=FALSE, ...) {
  if(!globalConnect) {
    conn <- getDabaseConnection()
  }
  result <- tryCatch({
    result <- DBI::dbGetQuery(conn,qu)
    return(result)
  },
  error = function(ex) {
    if(globalConnect) {
      conn <<- getDabaseConnection()
      tryCatch({
        result <- DBI::dbGetQuery(conn,qu)
        return(result)
      },
      error = function(ex) {
        errorHandler(ex, conn, driver)
      })
    } else {
      errorHandler(ex, conn, driver)
    }
  }, finally = {
    if(!globalConnect) {
      DBI::dbDisconnect(conn)
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
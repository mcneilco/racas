query <- function(qu) {
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
  driver <- eval(parse(text = applicationSettings$db_driver))
  conn <- switch(class(driver),
        "OraDriver" = DBI::dbConnect(driver, dbname=paste0(applicationSettings$db_host,":",applicationSettings$db_port,"/",applicationSettings$db_name), user=applicationSettings$db_user, pass=applicationSettings$db_password),
        "PostgreSQLDriver" = DBI::dbConnect(driver , user= applicationSettings$db_user, password=applicationSettings$db_password, dbname=applicationSettings$db_name, host=applicationSettings$db_host, port=applicationSettings$db_port),
        "MySQLDriver" = DBI::dbConnect(driver , user= applicationSettings$db_user, password=applicationSettings$db_password, dbname=applicationSettings$db_name, host=applicationSettings$db_host, port=applicationSettings$db_port),
        "JDBCDriver" = DBI::dbConnect(driver, paste0(getDBString(applicationSettings$db_driver),applicationSettings$db_host,":",applicationSettings$db_port,":",applicationSettings$db_name), user=applicationSettings$db_user, pass=applicationSettings$db_password),
        class(driver)
  )
  tryCatch({
    result <- DBI::dbGetQuery(conn,qu)
  },
  error = function(ex) {
    if(class(conn)=="character") {
      if(conn==class(driver)) {
        stop("Unrecognized driver class '",class(driver),"', applicationSettings$db_driver '",parse(text = applicationSettings$db_driver), "' evals to class ", class(driver),", must evaluate to a known driver class\n see ?racas:::query")
      }
    } else{
      print(ex)
      return(NULL)
    }
  }, finally = {
  })
  DBI::dbDisconnect(conn)
  return(result)
}

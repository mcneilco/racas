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
  driver <- eval(parse(text = racas::applicationSettings$db_driver))
  conn <- switch(class(driver),
        "OraDriver" = DBI::dbConnect(driver, dbname=paste0(racas::applicationSettings$db_host,":",racas::applicationSettings$db_port,"/",racas::applicationSettings$db_name), user=racas::applicationSettings$db_user, pass=racas::applicationSettings$db_password),
        "PostgreSQLDriver" = DBI::dbConnect(driver , user= racas::applicationSettings$db_user, password=racas::applicationSettings$db_password, dbname=racas::applicationSettings$db_name, host=racas::applicationSettings$db_host, port=racas::applicationSettings$db_port),
        "MySQLDriver" = DBI::dbConnect(driver , user= racas::applicationSettings$db_user, password=racas::applicationSettings$db_password, dbname=racas::applicationSettings$db_name, host=racas::applicationSettings$db_host, port=racas::applicationSettings$db_port),
        "JDBCDriver" = DBI::dbConnect(driver, paste0(getDBString(racas::applicationSettings$db_driver),racas::applicationSettings$db_host,":",racas::applicationSettings$db_port,":",racas::applicationSettings$db_name), user=racas::applicationSettings$db_user, pass=racas::applicationSettings$db_password),
        class(driver)
  )
  tryCatch({
    result <- DBI::dbGetQuery(conn,qu)
  },
  error = function(ex) {
    if(class(conn)=="character") {
      if(conn==class(driver)) {
        stop("Unrecognized driver class '",class(driver),"', racas::applicationSettings$db_driver '",parse(text = racas::applicationSettings$db_driver), "' evals to class ", class(driver),", must evaluate to a known driver class\n see ?racas:::query")
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

query <- function(qu) {
  driver <- eval(parse(text = applicationSettings$db_driver))
  conn <- switch(class(driver),
         "OraDriver" = dbConnect(driver, dbname=paste0(applicationSettings$db_host,":",applicationSettings$db_port,"/",applicationSettings$db_name), user=applicationSettings$db_user, pass=applicationSettings$db_password),
          "PostgreSQLDriver" = DBI::dbConnect(driver , user= applicationSettings$db_user, password=applicationSettings$db_password, dbname=applicationSettings$db_name, host=applicationSettings$db_host, port=applicationSettings$db_port),
          "MySQLDriver" = DBI::dbConnect(driver , user= applicationSettings$db_user, password=applicationSettings$db_password, dbname=applicationSettings$db_name, host=applicationSettings$db_host, port=applicationSettings$db_port)
  )
  result <- DBI::dbGetQuery(conn,qu)
  DBI::dbDisconnect(conn)
  return(result)
}

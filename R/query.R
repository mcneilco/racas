query <- function(qu) {
  conn <- dbConnect(eval(parse(text = applicationSettings$db_driver)), user= applicationSettings$db_user, password=applicationSettings$db_password, dbname=applicationSettings$db_name, host=applicationSettings$db_host, port=applicationSettings$db_port)
  result <- dbGetQuery(conn,qu)
  dbDisconnect(conn)
  return(result)
}

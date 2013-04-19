applicationSettings <- data.frame(
  appName = "ACAS",               #Application Display Name
  db_driver = "PostgreSQL()",     #Must be supplied in your own package load (MySQL(), Oracle() supported)
  db_user = "username",           #ACAS Schema db user
  db_password = "password",       #ACAS Schema db password
  db_name = "databasename",       #ACAS Schema db name
  db_host = "mcneilco.com",       #ACAS Host Name
  db_port = "5432",               #ACAS Port Number
  stringsAsFactors = FALSE
)
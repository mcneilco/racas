applicationSettings <- data.frame(
  appName = "ACAS",               #Application Display Name
  db_driver = "PostgreSQL()",     #Must be supplied in your own package load (MySQL(), Oracle() supported)
  db_user = "labseer",            #ACAS Schema db user
  db_password = "labseer",        #ACAS Schema db password
  db_name = "compound",           #ACAS Schema db name
  db_host = "host3.labsynch.com", #ACAS Host Name
  db_port = "5432",               #ACAS Port Number
  stringsAsFactors = FALSE
)
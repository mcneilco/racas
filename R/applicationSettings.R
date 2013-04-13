applicationSettings <- data.frame(
  appName = "ACAS",
  db_driver = "PostgreSQL()", #Must be supplied in the package load above (RMySQL, and ROracle also supported)
  db_user = "labseer",
  db_password = "labseer",
  db_name = "compound",
  db_host = "host3.labsynch.com",
  db_port = "5432",
  stringsAsFactors = FALSE
)
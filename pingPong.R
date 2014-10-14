

system.time(
  racas::pingPong(
    originView =  list(schema = "ACAS", name = "API_ALL_DATA"),
    intermediateTablePrefix = list(schema = racas::applicationSettings$server.database.username, name = "API_ALL_DATA", tableSpace = "ACAS_NOLOG", options = c("NOLOGGING")),
    destinationViewName = list(schema = "acas", name = "PP_API_ALL_DATA"),
    indexes = lapply(list("AGV_ID"), function(x) list(name = x, tableSpace = "ACAS_NOLOG", options = "NOLOGGING", "compute statistics"))
  )
)

pingPong(originView = list(schema = "ACAS", name = "API_EXPERIMENT"), destinationViewName = list(schema = "ACAS", name = "SOMEBLAH"))

i = 1
while(TRUE) {
  cat(i)
  cat(" ncol: ")
  cat(ncol(query("select * from PP_API_ALL_DATA where  ROWNUM = 1")))
  cat("\n")
  Sys.sleep(1)
  i = i + 1
}
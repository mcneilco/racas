system.time(
  pingPong(
    originView =  list(schema = "ACAS", name = "API_ALL_DATA"),
    intermediateTablePrefix = list(schema = racas::applicationSettings$server.database.username, name = "API_ALL_DATA", tableSpace = "KALYPSYSADMIN_NOLOG", options = c("NOLOGGING")),
    destinationViewName = list(schema = "acas", name = "PP_API_ALL_DATA"),
    #indexes = lapply(list("AGV_ID", "AG_ID", "AG_PUBLIC_DATA", "AG_TESTED_LOT", "CV_ID", "C_ID", "C_PUBLIC_DATA", "C_STATE_ID", "PROJECT", "PROTOCOL_NAME", "SV_ID", "S_ID", "S_PUBLIC_DATA", "S_STATE_ID", "TGV_ID", "TG_ID", "TG_PUBLIC_DATA", "TG_STATE_ID", "TG_TESTED_LOT"), function(x) list(name = x, tableSpace = "KALYPSYSADMIN_NOLOG", options = "NOLOGGING", "compute statistics"))
    indexes = lapply(list("AGV_ID"), function(x) list(name = x, tableSpace = "KALYPSYSADMIN_NOLOG", options = "NOLOGGING", "compute statistics"))
  )
)


i = 1
while(TRUE) {
  cat(i)
  cat(" ncol: ")
  cat(ncol(query("select * from PP_API_ALL_DATA where  ROWNUM = 1")))
  cat("\n")
  Sys.sleep(1)
  i = i + 1
}

pingPong(originView = list(schema = "ACAS", name = "API_EXPERIMENT"), destinationViewName = list(schema = "ACAS", name = "SOME BLAH"))

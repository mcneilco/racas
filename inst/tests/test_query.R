context("query")

testQuery <- ifelse(is.null(getOption("test_query")), FALSE, getOption("test_query"))
if(testQuery) {
  myTestConn <- try(getDatabaseConnection(), silent = TRUE)
  runTests <- class(myTestConn) != "try-error"
} else {
  runTests <- FALSE
}

if(runTests) {
  dbDisconnect(myTestConn)
    
  test_that("Query to protocol_label should return a data.fame",{
    rs <- query("select * from protocol_label limit 1")
    expect_that(class(rs), equals("data.frame"))
  })
  test_that("Basic query should not leave open a database connection in the global environment",{
    rs <- query("select * from protocol_label limit 1")
    expect_false(exists("conn", envir = .GlobalEnv))
  })
  test_that("Query using the globalConnect option should leave a global conn object",{
    qu <- "select * from protocol_label limit 1"
    rs <- query(qu, globalConnect = TRUE)
    expect_true(exists("conn", envir = .GlobalEnv))
    dbDisconnect(get("conn", envir = .GlobalEnv))
    rm(conn, envir = .GlobalEnv)
  })
  test_that("The first query using global connect should take longer because it has to establish a connection",{
    qu <- "select * from protocol_label limit 1"
    First_Query_Speed <- system.time(rs <- query(qu, globalConnect = TRUE))
    Second_Query_Speed <- system.time(rs <- query(qu, globalConnect = TRUE))
    dbDisconnect(get("conn", envir = .GlobalEnv))
    rm(conn, envir = .GlobalEnv)
    expect_true(First_Query_Speed["elapsed"] > Second_Query_Speed["elapsed"])
  })
  test_that("Providing query with an established connection should be faster than when query has to make a new connection",{
    qu <- "select * from protocol_label limit 1"
    conn <- getDatabaseConnection()
    First_Query_Speed <- system.time(rs <- query(qu, conn = conn))
    Second_Query_Speed <- system.time(rs <- query(qu))
    dbDisconnect(conn)
    rm(conn)
    expect_true(First_Query_Speed["elapsed"] < Second_Query_Speed["elapsed"])
  })
  test_that("If using globalConnect options and if there is a bad connection in the global namespace then query should replace it with a good one",{
    qu <- "select * from protocol_label limit 1"
    conn <- getDatabaseConnection()
    dbDisconnect(conn)
    assign("conn", conn, .GlobalEnv)
    expect_that(dbGetInfo(get("conn",envir = .GlobalEnv)),throws_error())
    rs <- query(qu, globalConnect = TRUE)
    expect_that(class(try(dbGetInfo(get("conn",envir = .GlobalEnv)))),equals("list"))
    dbDisconnect(get("conn", envir = .GlobalEnv))
    rm(conn, envir = .GlobalEnv)
  })

} else {
  cat("Not Testing Query functions because databse connection cannot be established with call to 'getDatabaseConnection()'")
}

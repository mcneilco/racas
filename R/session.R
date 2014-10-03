
saveSession <- function(id = NA) {
  if(is.na(id)) {
    id <- basename(tempfile(pattern = "rSe-"))
    temps <- lapply(c('TMPDIR', 'TMP', 'TEMP', '/tmp'), Sys.getenv)
    for(t in temps) { 
      if( t != "")
        id <- file.path(t, id)
        break()
    }
    
  }
  if(!is.null(dev.list()))
    warning("Open graphics devices will not be saved or restored.")
  
  #.save.session.search <- search()
  #.save.session.packages <- .packages()
  #assign(".save.session.search", .save.session.search, envir = parent.frame())
  #assign(".save.session.packages", .save.session.packages, envir = parent.frame())
  save(list=ls(envir = parent.frame()), envir = parent.frame(), file=id)
  return(id)
}

loadSession <- function(id, envir = parent.frame()) {
  if(is.null(id)) {
    stop("id cannot be null")
  }
  #Check if exits
  if(!file.exists(id)) {
    stop(paste0("\'", id, "\' cannot be found.  Session may have been deleted"))
  }
  #Check writeable
  if(file.access(id, mode = 2) != 0) {
    stop(paste0("\'", id , "\' is not writeable"))
  }
  load(id, envir)
  #blah <- sapply( rev(get(".save.session.packages", envir=envir)), library, character.only=TRUE )
  #pad <- function(x,n) c( rep(NA,n-length(x)), x )
  #current.search <- search()[-1]
  #saved.search <- get(".save.session.search", envir=envir)[-1]
  #identical <- pad(id, length(saved.search)) == saved.search
  #identical <- identical[!is.na(identical)]
#   for( i in saved.search[!identical] )
#   {
#     if( charmatch( "file:", i, nomatch=FALSE) )
#       attach(sub( "file:", "", i ) )
#     else if (charmatch( "package:", i, nomatch=FALSE)  )
#       stop(paste("Somehow we missed loading package",i))
#     else
#     {
#       do.call("attach",list(as.name(i)))
#     }
#     
#   }
  
#   rm(list=c(".save.session.packages",
#             ".save.session.search"), envir = envir)
  return(id)
}


deleteSession <- function(id) {
  if(is.null(id)) {
    stop("id cannot be null")
  }
  #Check if exits
  if(!file.exists(id)) {
    stop(paste0("\'", id, "\' cannot be found.  Session may have been deleted"))
  }
  #Check writeable
  if(file.access(id, mode = 2) != 0) {
    stop(paste0("\'", id , "\' is not writeable"))
  }
  unlink(id)
  return(paste0(id, " deleted"))
}


saveSession <- function(id = NA) {
  if(is.na(id)) {
    id <- tempfile(pattern = "rSe-")
  }
  save.image(file = id)
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

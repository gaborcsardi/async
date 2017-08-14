
#' @export

sequence <- function(...) {
  funcs <- list(...)
  force(funcs)
  i <- 1

  function(..., callback) {
    force(callback)
    task <- get_default_event_loop()$run_generic(callback)
    mycallback <- function(err, ...) {
      if (!is.null(err)) return(task$callback(err, ...))
      i <<- i + 1
      if (i > length(funcs)) return(task$callback(NULL ,...))
      funcs[[i]](..., callback = mycallback)
    }
    funcs[[i]](..., callback = mycallback)
    task$id
  }
}

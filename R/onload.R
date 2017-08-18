
async_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  async_env$default_loop <- event_loop$new()
}

.onUnload <- function(libpath) {
  message("Unloading async package, waiting for all tasks...")
  get_default_event_loop()$wait_for_all()
}

#' Default event loop of the R session.
#'
#' This event loop is created when the `async` package is loaded, and
#' all asyncronous constructs use this event loop by default.
#'
#' @return The default event loop of the R session.
#'
#' @export
#' @seealso [event_loop]

get_default_event_loop <- function() {
  async_env$default_loop
}

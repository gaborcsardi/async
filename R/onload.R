
async_env <- new.env(parent = emptyenv())

## nocov start

.onLoad <- function(libname, pkgname) {
  async_env$default_loop <- event_loop$new()

  ## How many frames to drop from event loop call stacks?
  error_callback_drop_num()
}

.onUnload <- function(libpath) {
  message("Unloading async package, waiting for all tasks...")
  ## TODO
  ## get_default_event_loop()$run()
}

## nocov end

#' Default event loop of the R session.
#'
#' This event loop is created when the `async` package is loaded, and
#' all asyncronous constructs use this event loop by default.
#'
#' @return The default event loop of the R session.
#'
#' @seealso [event_loop]
#' @keywords internal

get_default_event_loop <- function() {
  async_env$default_loop
}

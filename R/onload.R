
async_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  async_env$default_loop <- event_loop$new()
}

.onUnload <- function(libpath) {
  message("Unloading async package, awaiting all tasks...")
  get_default_event_loop()$await_all()
}

get_default_event_loop <- function() {
  async_env$default_loop
}

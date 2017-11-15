
## nocov start

.onLoad <- function(libname, pkgname) {
  ## Nothing to do for now
}

.onUnload <- function(libpath) {
  message("Unloading async package, waiting for all tasks...")
  ## TODO
  ## get_default_event_loop()$run()
}

## nocov end

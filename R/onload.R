
## nocov start

.onLoad <- function(libname, pkgname) {
  ## How many frames to drop from event loop call stacks?
  error_callback_drop_num()
}

.onUnload <- function(libpath) {
  message("Unloading async package, waiting for all tasks...")
  ## TODO
  ## get_default_event_loop()$run()
}

## nocov end

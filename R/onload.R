
## nocov start

.onLoad <- function(libname, pkgname) {
  ## This is the current max
  backports::import(pkgname, "strrep")
  options(warning.length = 8170)
}

.onUnload <- function(libpath) {
  message("Unloading async package, waiting for all tasks...")
  ## TODO
  ## get_default_event_loop()$run()
}

## nocov end


## nocov start

.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("DEBUGME") != "" &&
    requireNamespace("debugme", quietly = TRUE)) {
    debugme::debugme()
  }
}

## nocov end

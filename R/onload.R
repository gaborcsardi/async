
## nocov start

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "strrep")
  ## This is the current max
  options(warning.length = 8170)
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
}

## nocov end


## nocov start

.onLoad <- function(libname, pkgname) {
  lazyrmd$onload_hook(
    local = "if-newer",
    ci = TRUE,
    cran = FALSE
  )
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
}

## nocov end

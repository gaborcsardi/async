
## nocov start

.onLoad <- function(libname, pkgname) {
  ## This is the current max
  options(warning.length = 8170)
  if (requireNamespace("debugme", quietly = TRUE)) debugme::debugme()
}

#' @export
#' @rdname async_debug

async_debug_shortcuts <- function() {
  as <- function(name, fun) {
    makeActiveBinding(name, fun, .GlobalEnv)
  }
  as(".an", async_next)
  as(".as", async_step)
  as(".asb", async_step_back)
  as(".al", async_list)
  as(".at", async_tree)
  as(".aw", async_where)
}

## nocov end

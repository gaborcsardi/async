
#' @export
#' @rdname async_every

async_some <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  deferred$new(
    type = "async_some",
    parents = defs,
    action = function(resolve) if (nx == 0) resolve(FALSE),
    parent_resolve = function(value, resolve, reject) {
      if (!done && isTRUE(value)) {
        done <<- TRUE
        resolve(TRUE)
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(FALSE)
      }
    }
  )
}

async_some <- mark_as_async(async_some)


#' @export

when_all <- function(..., .list = list()) {
  defs <- c(list(...), .list)

  deferred$new(function(resolve, reject) {
    num_todo <- length(defs)

    handle_fulfill <- function(value) {
      num_todo <<- num_todo - 1
      if (num_todo == 0) resolve(lapply(defs, get_value_x))
    }

    handle_reject <- function(reason) {
      reject(reason)
    }

    for (i in seq_along(defs)) {
      if (!is.deferred(defs[[i]])) {
        num_todo <- num_todo - 1
      } else {
        defs[[i]]$then(handle_fulfill, handle_reject)
      }
    }

    if (num_todo == 0) resolve(defs)
  })
}

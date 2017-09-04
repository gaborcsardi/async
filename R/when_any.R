
#' @export

when_any <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  if (length(defs) == 0) stop("Empty list in `when_any` is not allowed")

  deferred$new(function(resolve, reject) {
    num_done <- 0

    is_defs <- vlapply(defs, is_deferred)
    if (!all(is_defs)) return(resolve(defs[!is_defs][[1]]))

    handle_fulfill <- function(value) {
      num_done <<- num_done + 1
      if (num_done == 1) resolve(value)
    }

    handle_reject <- function(reason) {
      num_done <<- num_done + 1
      if (num_done == 1) reject(reason)
    }

    for (i in seq_along(defs)) {
      defs[[i]]$then(handle_fulfill, handle_reject)
    }
  })
}

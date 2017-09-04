
#' Resolve a deferred as soon as one deferred in a list resolves
#'
#' Create a deferred value that is resolved as soon as one deferred value
#' resolves.
#'
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @seealso [when_all()]
#' @export
#' @examples
#' ## Use the URL that returns first
#' u1 <- http_get("https://httpbin.org/get")
#' u2 <- http_get("https://eu.httpbin.org/get")
#' dx <- when_any(u1, u2)$then(~ .$url)
#' await(dx)

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


#' Resolve a deferred as soon as some deferred from a list resolve
#'
#' `when_some` creates a deferred value that is resolved as soon as the
#' specified number of deferred values resolve.
#'
#' `when_any` is a special case for a single.
#'
#' If the specified number of deferred values cannot be resolved, then
#' the returned deferred value is rejected.
#'
#' @param count Number of deferred values that need to resolve.
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @seealso [when_all()]
#' @export
#' @examples
#' ## Use the URL that returns first
#' afun <- async(function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_any(u1, u2)$then(~ .$url)
#' })
#' sync_wrap(afun())

when_some <- function(count, ..., .list = list()) {
  force(count)
  defs <- c(list(...), .list)
  num_defs <- length(defs)

  deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)

    num_failed <- 0

    ## Maybe we don't have that many deferred
    if (num_defs < count) {
      return(reject("Cannot resolve enough deferred values"))
    }

    ## We already have this many
    is_defs <- vlapply(defs, is_deferred)
    resolved <- defs[!is_defs]

    ## Maybe we already have enough
    if (length(resolved) >= count) {
      return(resolve(resolved[seq_len(count)]))
    }

    handle_fulfill <- function(value) {
      resolved <<- c(resolved, list(value))
      if (length(resolved) == count) resolve(resolved)
    }

    handle_reject <- function(reason) {
      num_failed <<- num_failed + 1
      if (num_failed + count == num_defs + 1) reject(reason)
    }

    for (i in seq_along(defs)) {
      defs[[i]]$then(handle_fulfill, handle_reject)
    }
  })
}

#' @export
#' @rdname when_some

when_any <- function(..., .list = list()) {
  when_some(1, ..., .list = .list)$then(~ .[[1]])
}

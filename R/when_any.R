
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
#' @param cancel Whether to cancel the deferred computations that are
#'   not needed to finish `when_some()` or `when_any()`, including the
#'   case when one of them throws an error.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @seealso [when_all()]
#' @export
#' @examples
#' ## Use the URL that returns first
#' afun <- function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_any(u1, u2)$then(~ .$url)
#' }
#' synchronise(afun())

when_some <- function(count, ..., .list = list(), cancel = TRUE) {
  force(count); force(cancel)
  defs <- c(list(...), .list)
  num_defs <- length(defs)

  deferred$new(lazy = FALSE, function(resolve, reject) {
    force(resolve)
    force(reject)

    num_failed <- 0

    ## Maybe we don't have that many deferred
    if (num_defs < count) {
      def__cancel_pending(defs, cancel)
      return(reject(async_constant("Cannot resolve enough deferred values")))
    }

    ## We already have this many
    is_defs <- vlapply(defs, is_deferred)
    resolved <- defs[!is_defs]

    ## Maybe we already have enough
    if (length(resolved) >= count) {
      def__cancel_pending(defs, cancel)
      return(resolve(async_constant(resolved[seq_len(count)])))
    }

    handle_fulfill <- function(value) {
      resolved <<- c(resolved, list(value))
      if (length(resolved) == count) {
        def__cancel_pending(defs, cancel)
        resolve(resolved)
      }
    }

    handle_reject <- function(reason) {
      num_failed <<- num_failed + 1
      if (num_failed + count == num_defs + 1) {
        def__cancel_pending(defs, cancel)
        reject(reason)
      }
    }

    for (i in seq_along(defs)) {
      defs[[i]]$then(handle_fulfill)$catch(handle_reject)$null()
    }
  })
}

#' @export
#' @rdname when_some

when_any <- function(..., .list = list(), cancel = TRUE) {
  when_some(1, ..., .list = .list, cancel = cancel)$then(~ .[[1]])
}

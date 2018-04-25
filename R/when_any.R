
#' Resolve a deferred as soon as some deferred from a list resolve
#'
#' `when_some` creates a deferred value that is resolved as soon as the
#' specified number of deferred values resolve.
#'
#' `when_any` is a special case for a single.
#'
#' If the specified number of deferred values cannot be resolved, then
#' `when_any` throws an error.
#'
#' async has auto-cancellation, so if the required number of deferred values
#' are resolved, or too many of them throw error, the rest of the are
#' cancelled.
#'
#' If `when_any` throws an error, then all the underlying error objects
#' are returned in the `errors` member of the error object thrown by
#' `when_any`.
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
#' afun <- function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_any(u1, u2)$then(function(x) x$url)
#' }
#' synchronise(afun())

when_some <- function(count, ..., .list = list()) {
  force(count)
  defs <- c(list(...), .list)
  num_defs <- length(defs)
  num_failed <- 0L
  ifdef <- vlapply(defs, is_deferred)
  resolved <- defs[!ifdef]
  errors <- list()

  cancel_all <- function() lapply(defs[ifdef], function(x) x$cancel())

  deferred$new(
    type = "when_some", call = sys.call(),
    parents = defs[ifdef],
    action = function(resolve) {
      if (num_defs < count) {
        stop("Cannot resolve enough deferred values")
      } else if (length(resolved) >= count) {
        resolve(resolved[seq_len(count)])
      }
    },
    parent_resolve = function(value, resolve) {
      resolved <<- c(resolved, list(value))
      if (length(resolved) == count) {
        resolve(resolved)
      }
    },
    parent_reject = function(value, resolve) {
      num_failed <<- num_failed + 1L
      errors <<- c(errors, list(value))
      if (num_failed + count == num_defs + 1L) {
        err <- structure(
          list(errors = errors, message = "when_some / when_any failed"),
          class = c("async_rejected", "error", "condition"))
        stop(err)
      }
    }
  )
}

when_some <- mark_as_async(when_some)

#' @export
#' @rdname when_some

when_any <- function(..., .list = list()) {
  when_some(1, ..., .list = .list)$then(function(x) x[[1]])
}

when_any <- mark_as_async(when_any)

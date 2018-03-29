
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
#' afun <- function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_any(u1, u2)$then(~ .$url)
#' }
#' synchronise(afun())

when_some <- function(count, ..., .list = list(), .cancel = TRUE) {
  force(count); force(.cancel)
  defs <- c(list(...), .list)
  num_defs <- length(defs)
  num_failed <- 0L
  ifdef <- vlapply(defs, is_deferred)
  resolved <- defs[!ifdef]
  errors <- list()

  cancel_all <- function() {
    for (x in defs[ifdef]) x$cancel("Not needed for when_any")
  }

  deferred$new(
    type = "when_some",
    parents = defs[ifdef],
    action = function(resolve, reject) {
      if (num_defs < count) {
        if (.cancel) cancel_all()
        reject("Cannot resolve enough deferred values")
      } else if (length(resolved) >= count) {
        if (.cancel) cancel_all()
        resolve(resolved[seq_len(count)])
      }
    },
    parent_resolve = function(value, resolve, reject) {
      resolved <<- c(resolved, list(value))
      if (length(resolved) == count) {
        if (.cancel) cancel_all()
        resolve(resolved)
      }
    },
    parent_reject = function(value, resolve, reject) {
      num_failed <<- num_failed + 1L
      errors <<- c(errors, list(value))
      if (num_failed + count == num_defs + 1L) {
        err <- structure(
          list(errors = errors, message = "when_some / when_any failed"),
          class = c("async_rejected", "error", "condition"))
        if (.cancel) cancel_all()
        reject(err)
      }
    }
  )
}

#' @export
#' @rdname when_some

when_any <- function(..., .list = list(), .cancel = FALSE) {
  when_some(1, ..., .list = .list, .cancel = .cancel)$then(~ .[[1]])
}


#' Deferred value for a set of deferred values
#'
#' Create a deferred value that is resolved when all listed deferred values
#' are resolved. Note that the rejection of an input deferred value
#' triggers the rejection of the deferred value returned by `when_all` as
#' well.
#'
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @param cancel Whether to cancel the deferred computations if one of
#'   them throws an error.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @seealso [when_any()], [when_some()]
#' @export
#' @examples
#' ## Check that the contents of two URLs are the same
#' afun <- async(function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_all(u1, u2)$
#'     then(~ identical(.[[1]]$content, .[[2]]$content))
#' })
#' synchronise(afun())

when_all <- function(..., .list = list(), cancel = TRUE) {
  force(cancel)
  defs <- c(list(...), .list)

  deferred$new(lazy = FALSE, type = "when-all", function(resolve, reject) {
    num_todo <- length(defs)

    handle_fulfill <- function(value) {
      num_todo <<- num_todo - 1
      if (num_todo == 0) {
        resolve(lapply(defs, get_value_x))
      }
    }

    handle_reject <- function(reason) {
      if (cancel) async_cancel_pending(.list = defs)
      reject(reason)
    }

    for (i in seq_along(defs)) {
      if (!is_deferred(defs[[i]])) {
        num_todo <- num_todo - 1
      } else {
        defs[[i]]$then(handle_fulfill)$catch(handle_reject)$null()
      }
    }

    if (num_todo == 0) resolve(async_constant(defs))
  })
}

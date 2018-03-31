
#' Asynchronous function call with a timeout
#'
#' If the deferred value is not resolved before the timeout expires,
#' `async_timeout()` throws an `async_timeout` error.
#'
#' @param task Asynchronous function.
#' @param timeout Timeout as a `difftime` object, or number of seconds.
#' @param ... Additional arguments to `task`.
#' @return A deferred value. An `async_timeout` error is thrown if it is
#'   not resolved within the specified timeout.
#'
#' @family async utilities
#' @export
#' @examples
#' ## You can catch the error, asynchronously
#' synchronise(
#'   async_timeout(function() delay(1/10)$then(~ "OK"), 1/1000)$
#'     catch(async_timeout = ~ "Timed out",
#'           error = ~ "Other error")
#' )
#'
#' ## Or synchronously
#' tryCatch(
#'   synchronise(
#'     async_timeout(function() delay(1/10)$then(~ "OK"), 1/1000)
#'   ),
#'   async_timeout = function(e) "Timed out. :(",
#'   error = function(e) paste("Other error:", e$message)
#' )

async_timeout <- function(task, timeout, ...) {
  task <- async(task)
  force(timeout)
  done <- FALSE

  deferred$new(
    type = "timeout",
    parents = list(d1 <- task(...), d2 <- delay(timeout)),
    parent_resolve = function(value, resolve, reject, id) {
      if (!done) {
        done <<- TRUE
        if (id == d1$get_id()) {
          resolve(value)
        } else {
          reject("Timed out")
        }
      }
    }
  )
}

async_timeout <- mark_as_async(async_timeout)

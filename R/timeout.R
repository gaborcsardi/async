
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
#'   async_timeout(function() delay(1/10)$then(function() "OK"), 1/1000)$
#'     catch(async_timeout = function(e) "Timed out",
#'           error = function(e) "Other error")
#' )
#'
#' ## Or synchronously
#' tryCatch(
#'   synchronise(
#'     async_timeout(function() delay(1/10)$then(function() "OK"), 1/1000)
#'   ),
#'   async_timeout = function(e) "Timed out. :(",
#'   error = function(e) paste("Other error:", e$message)
#' )

async_timeout <- function(task, timeout, ...) {
  task <- async(task)
  force(timeout)
  list(...)
  done <- FALSE

  self <- deferred$new(
    type = "timeout", call = sys.call(),
    action = function(resolve) {
      task(...)$then(function(x) list("ok", x))$then(self)
      delay(timeout)$then(function() list("timeout"))$then(self)
    },
    parent_resolve = function(value, resolve) {
      if (!done) {
        done <<- TRUE
        if (value[[1]] == "ok") {
          resolve(value[[2]])
        } else {
          stop("Timed out")
        }
      }
    }
  )
}

async_timeout <- mark_as_async(async_timeout)

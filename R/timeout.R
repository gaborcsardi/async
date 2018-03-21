
#' Asynchronous function call with a timeout
#'
#' If the deferred value is not resolved before the timeout expires,
#' it will be rejected, with an `async_timeout` error.
#'
#' Note that currently the task is not cancelled, we just don't wait for
#' its return value, and we ignore the return value when it is resolved
#' later.
#'
#' @param task Asynchronous function.
#' @param timeout Timeout as a `difftime` object, or number of seconds.
#' @param ... Additional arguments to `task`.
#' @param cancel Whether to cancel the task if a timeout happens.
#' @return A deferred value that is rejected if it is not resolved
#'   within the specified timeout.
#'
#' @family async utilities
#' @export
#' @examples
#' ## You can catch the error, asynchronously
#' synchronise(
#'   async_timeout(function() delay(1/10)$then(~ "OK"), 1/1000)$
#'     catch(~ "Timed out")
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

async_timeout <- function(task, timeout, ..., cancel = TRUE) {
  task <- async(task)
  force(timeout); force(cancel)
  done <- FALSE

  deferred$new(function(resolve, reject) {
    task(...)$
      then(function(value) { if (!done) resolve(value); done <<- TRUE })$
      catch(function(reason) { if (!done) reject(reason); done <<- TRUE })$
      null()

    delay(timeout)$then(
      function(value) {
        if (!done) {
          def__cancel_pending(list(task), cancel)
          reject(make_error("Timed out", "async_timeout"))
        }
        done <<- TRUE
      }
    )$null()
  })
}

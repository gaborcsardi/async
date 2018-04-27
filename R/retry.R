
#' Retry an asynchronous function a number of times
#'
#' Keeps trying until the function's deferred value resolves without
#' error, or `times` tries have been performed.
#'
#' @param task An asynchronous function.
#' @param times Number of tries.
#' @param ... Arguments to pass to `task`.
#' @return Deferred value for the operation with retries.
#'
#' @family async control flow
#' @export
#' @examples
#' ## Try a download at most 5 times
#' afun <- async(function() {
#'   async_retry(
#'     function() http_get("https://eu.httpbin.org"),
#'     times = 5
#'   )$then(function(x) x$status_code)
#' })
#'
#' synchronise(afun())

async_retry <- function(task, times, ...) {
  task <- async(task)
  times <- times
  force(list(...))

  self <- deferred$new(
    type = "retry", call = sys.call(),
    parents = list(task(...)),
    parent_reject = function(value, resolve) {
      times <<- times - 1L
      if (times > 0) {
        task(...)$then(self)
      } else {
        stop(value)
      }
    }
  )
}

async_retry <- mark_as_async(async_retry)

#' Make an asynchronous funcion retryable
#'
#' @param task An asynchronous function.
#' @param times Number of tries.
#' @return Asynchronous retryable function.
#'
#' @family async control flow
#' @export
#' @examples
#' ## Create a downloader that retries five times
#' http_get_5 <- async_retryable(http_get, times = 5)
#' ret <- synchronise(
#'   http_get_5("https://eu.httpbin.org/get?q=1")$
#'     then(function(x) rawToChar(x$content))
#' )
#' cat(ret)

async_retryable <- function(task, times) {
  task <- async(task)
  force(times)
  function(...) {
    async_retry(task, times, ...)
  }
}


#' Retry an asynchronous function with exponential backoff
#'
#' Keeps trying until the function's deferred value resolves without
#' error, or `times` tries have been performed, or `time_limit` seconds
#' have passed since the start of the first try.
#'
#' @param task An asynchronous function.
#' @param times Maximum number of tries.
#' @param time_limit Maximum number of seconds to try.
#' @param backoff_callback If not `NULL` then a callback function to
#'   calculate waiting time, after the `i`the try. `i` is passed as an
#'   argument. If `NULL`, then the default is used, which is a uniform
#'   random number of seconds between 1 and 2^i.
#' @param ... Arguments to pass to `task`.
#' @return Deferred value for the operation with retries.
#'
#' @family async control flow
#' @export
#' @examples
#' \donttest{
#' afun <- function() {
#'   wait_100_ms <- function(i) 0.1
#'   async_backoff(
#'     function() if (runif(1) < 0.8) stop("nope") else "yes!",
#'     times = 5,
#'     backoff_callback = wait_100_ms
#'   )
#' }
#'
#' # There is a slight chance that it fails
#' tryCatch(synchronise(afun()), error = function(e) e)
#' }

async_backoff <- function(task, times = Inf, time_limit = Inf,
                          backoff_callback = NULL, ...) {

  task <- async(task)
  did <- 0
  times <- times
  time_limit <- time_limit
  backoff_callback <- backoff_callback %||% default_backoff
  force(list(...))

  self <- deferred$new(
    type = "backoff", call = sys.call(),
    action = function(resolve) {
      time_limit <<- Sys.time() + time_limit
      task(...)$then(self)
    },
    parent_reject = function(value, resolve) {
      did <<- did + 1L
      if ((is.null(times) || did < times) &&
          (is.null(time_limit) || Sys.time() < time_limit)) {
        wait <- backoff_callback(did)
        delay(wait)$
          then(function() task(...))$
          then(self)
      } else {
        stop(value)
      }
    }
  )
}

async_backoff <- mark_as_async(async_backoff)

default_backoff <- function(i) {
  as.integer(stats::runif(1, min = 1, max = 2^i) * 1000) / 1000
}

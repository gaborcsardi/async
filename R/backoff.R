
#' Retry an asynchronous function with exponential backoff
#'
#' Keeps trying until the function's deferred value resolves without
#' error, or `times` tries have been performed, or `time_limit` seconds
#' have passed since the start of the first try.
#'
#' Note that all unnamed arguments are passed to `task`.
#'
#' @param task An asynchronous function.
#' @param ... Arguments to pass to `task`.
#' @param .args More arguments to pass to `task`.
#' @param times Maximum number of tries.
#' @param time_limit Maximum number of seconds to try.
#' @param custom_backoff If not `NULL` then a callback function to
#'   calculate waiting time, after the `i`the try. `i` is passed as an
#'   argument. If `NULL`, then the default is used, which is a uniform
#'   random number of seconds between 1 and 2^i.
#' @param on_progress Callback function for a progress bar. Retries are
#'   announced here, if not `NULL`. `on_progress` is called with two
#'   arguments. The first is a named list with entries:
#'   * `event`: string that is either `"retry"` or `"givenup"`,
#'   * `tries`: number of tried so far,
#'   * `spent`: number of seconds spent trying so far,
#'   * `error`: the error object for the last failure,
#'   * `retry_in`: number of seconds before the next try.
#'   The second argument is `progress_data`.
#' @param progress_data `async_backoff()` will pass this object to
#'   `on_progress` as the second argument.
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
#'     custom_backoff = wait_100_ms
#'   )
#' }
#'
#' # There is a slight chance that it fails
#' tryCatch(synchronise(afun()), error = function(e) e)
#' }

async_backoff <- function(task, ..., .args = list(), times = Inf,
                          time_limit = Inf, custom_backoff = NULL,
                          on_progress = NULL, progress_data = NULL) {

  task <- async(task)
  args <- c(list(...), .args)
  times <- times
  time_limit <- time_limit
  custom_backoff <- custom_backoff %||% default_backoff
  on_progress <- on_progress
  progress_data <- progress_data

  did <- 0L
  started <- NULL
  limit <- NULL

  self <- deferred$new(
    type = "backoff", call = sys.call(),
    action = function(resolve) {
      started <<- Sys.time()
      limit <<- started + time_limit
      do.call(task, args)$then(self)
    },
    parent_reject = function(value, resolve) {
      did <<- did + 1L
      now <- Sys.time()
      if (did < times && now < limit) {
        wait <- custom_backoff(did)
        if (!is.null(on_progress)) {
          on_progress(list(
            event = "retry",
            tries = did,
            spent = now - started,
            error = value,
            retry_in = wait
          ), progress_data)
        }
        delay(wait)$
          then(function() do.call(task, args))$
          then(self)
      } else {
        if (!is.null(on_progress)) {
          on_progress(list(
            event = "givenup",
            tries = did,
            spent = now - started,
            error = value,
            retry_in = NA_real_
          ), progress_data)
        }
        stop(value)
      }
    }
  )
}

async_backoff <- mark_as_async(async_backoff)

default_backoff <- function(i) {
  as.integer(stats::runif(1, min = 1, max = 2^i) * 1000) / 1000
}

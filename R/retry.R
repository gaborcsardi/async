
#' Retry a task a number of times
#'
#' Attempts to get a successful response from task no more than `times`
#' times before returning an error. If the task is successful, the callback
#' will be passed the result of the successful task. If all attempts fail,
#' the callback will be passed the error and result (if any) of the final
#' attempt.
#'
#' @param task An async function to retry. Invoked with `(callback)`.
#' @param times The number of attempts to make before giving up.
#' @param callback A callback which is called when the task has succeeded,
#'   or after the final failed attempt. It receives the `err` and `result`
#'   arguments of the last attempt at completing the task. Invoked with
#'   `(err, results)`.
#' @return Task id.
#'
#' @family async control flow
#' @export
#' @examples
#' ## Try a download at most 5 times
#' result <- NULL
#' wait_for(retry(
#'   function(callback) http_get("https://httpbin.org", callback),
#'   times = 5,
#'   function(err, res) result <<- res$status_code
#' ))
#' result

retry <- function(task, times, callback) {
  assert_that(
    is_task(task),
    is_callback(callback),
    is_count(times)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  mycallback <- function(err, res) {
    if (is.null(err)) return(etask$callback(NULL, res))
    times <<- times - 1
    if (times) {
      async_call(task, args = list(), mycallback)
    } else {
      etask$callback(err, NULL)
    }
  }

  async_call(task, args = list(), mycallback)
  etask$id
}

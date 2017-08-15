
#' Make a task retryable
#'
#' A close relative of [retry()]. This method wraps a task and makes it
#' retryable, rather than immediately calling it with retries.
#'
#' @param task The asynchronous function to wrap. This function will be
#'   passed any arguments passed to the returned wrapper. Invoked with
#'   `(..., callback)`.
#' @return The wrapped asynchronous function.
#'
#' @family async control flow
#' @export
#' @examples
#' http_get_harder <- retryable(http_get, 5)
#' result <- NULL
#' await(http_get_harder(
#'   url = "https://httpbin.org",
#'   callback = function(err, res) result <<- res
#' ))
#' result$status_code

retryable <- function(task, times) {
  assert_that(is_task(task), is_count(times))
  function(..., callback) {
    force(times) ; force(callback)
    retry(
      function(callback) task(..., callback = callback),
      times,
      callback
    )
  }
}

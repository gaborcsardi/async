
#' @export

retryable <- function(task) {
  assert_that(is_task(task))
  function(..., callback, times) {
    args <- list(...) ; force(args) ; force(callback) ; force(times)
    retry(task, callback, times)
  }
}

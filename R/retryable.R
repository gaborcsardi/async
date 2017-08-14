
#' @export

retryable <- function(async_function) {
  force(async_function)
  function(..., callback, times) {
    args <- list(...) ; force(args) ; force(callback) ; force(times)
    retry(async_function, callback, times)
  }
}

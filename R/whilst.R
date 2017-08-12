
#' @export

whilst <- function(test_function, async_function, callback) {
  force(test_function) ; force(async_function) ; force(callback)
  if (test_function()) {
    mycallback <- function(err, ...) {
      if (!is.null(err)) {
        callback(err, ...)
      } else {
        if (test_function()) {
          async_function(mycallback)
        } else {
          callback(NULL, ...)
        }
      }
    }
    async_function(mycallback)
  }
}

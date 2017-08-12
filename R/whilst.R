
#' @export

whilst <- function(test_function, async_function, callback) {
  force(test_function) ; force(async_function) ; force(callback)
  n <- 0
  if (test_function()) {
    mycallback <- function(err, ...) {
      n <<- n + 1
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

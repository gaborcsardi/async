
#' @param async_function An async function that is called with
#'   a callback function. The callback function is called once
#'   the computation or I/O is done. The first argument of the
#'   callback function is the error (`NULL` if no error), and
#'   the second argument is the result (`NULL` is error).
#'
#' @export

retry <- function(async_function, callback, times) {
  force(async_function) ; force(callback); force(times)

  mycallback <- function(err, res) {
    if (is.null(err)) {
      callback(NULL, res)
    } else {
      times <<- times - 1
      if (times) {
        async_function(mycallback)
      } else {
        callback(err, NULL)
      }
    }
  }

  async_function(mycallback)
}

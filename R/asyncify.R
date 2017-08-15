
#' Create an async task from a synchronous function
#'
#' This is useful if you need the function in some asynchronous control
#' flow structure, e.g. [waterfall()].
#'
#' Arguments passed to the wrapper function are passed to the wrap function
#' (except for the `callback` argument of course).
#'
#' @param func The function to convert to asyncronous.
#' @return A task, which has the same arguments as the original function,
#'   plus an extra `callback` argument at the end. This function is called
#'   after the original computation is done by `func`.
#'
#' @family async utilities
#' @export
#' @examples
#' f <- function(x) { Sys.sleep(1/100); x + 42 }
#' af <- asyncify(f)
#' amap(c(0, 42), af, function(err, res) print(res))

asyncify <- function(func) {
  assert_that(is.function(func))
  function(..., callback) {
    tryCatch(
      callback(NULL, func(...)),
      error = function(e) callback(e, NULL)
    )
  }
}

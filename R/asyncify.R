
#' Create an async task from a synchronous function
#'
#' This is useful if you need the function in some asynchronous control
#' flow structure, e.g. [waterfall()].
#'
#' Arguments passed to the wrapper function are passed to the wrap function
#' (except for the `callback` argument of course).
#'
#' @param func The function to convert to asyncronous.
#' @param return Whether the callback has a return value argument, in
#'   addition to the error. Most callbacks have return values, so they
#'   have two arguments.
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

asyncify <- function(func, return = TRUE) {
  assert_that(is.function(func), is_flag(return))
  function(..., callback) {
    res <- tryCatch(
      func(...),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      error <- res
      res <- NULL
    } else {
      error <- NULL
    }
    if (return) callback(error, res) else callback(error)
  }
}

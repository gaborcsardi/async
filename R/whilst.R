
#' Repeatedly call task, while test returns true
#'
#' Calls callback when stopped, or an error occurs.
#'
#' @param test_function Synchronous truth test to perform before each
#'   execution of task. Invoked without arguments.
#' @param task An async function which is called each time test
#'   passes. Invoked with `(callback)`.
#' @param callback A callback which is called after the test function has
#'   failed and repeated execution of `task` has stopped. `callback` will
#'   be passed an error and any arguments passed to the final task's
#'   callback. Invoked with `(err, [results])`.
#' @return Task id.
#'
#' @family async control flow
#' @export
#' @examples
#' ## Keep calling while result is bigger than 0.1
#' calls <- 0
#' number <- Inf
#' wait_for(whilst(
#'   function() number > 0.1,
#'   function(callback) {
#'     number <<- runif(1)
#'     calls <<- calls + 1
#'     callback(NULL, number)
#'   },
#'   function(err, res) print(res)
#' ))
#' calls

whilst <- function(test_function, task, callback) {
  assert_that(
    is.function(test_function),
    is_task(task),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  if (test_function()) {
    mycallback <- function(err, ...) {
      if (!is.null(err)) return(etask$callback(err, ...))
      if (test_function()) {
        async_call(task, list(), mycallback)
      } else {
        etask$callback(NULL, ...)
      }
    }
    async_call(task, list(), mycallback)

  } else {
    etask$callback(NULL, NULL)
  }

  etask$id
}

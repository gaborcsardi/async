
#' Runs the tasks array of functions in series
#'
#' Eeach task is passing its result to the next in the array.
#' However, if any of the tasks pass an error to their own callback, the
#' next function is not executed, and the main
#' callback is immediately called with the error.
#'
#' @param tasks An array of async functions to run. Each function should
#'   complete with any number of result values. The result values will be
#'   passed as arguments, in order, to the next task.
#' @param callback A callback to run once all the functions have
#'   completed. This will be passed the results of the last task's
#'   callback. Invoked with `(err, [results])`.
#' @return Task id.
#'
#' @family async control flow
#' @export
#' @examples
#' await(waterfall(
#'   list(
#'     function(callback) callback(NULL, 'one', 'two'),
#'     function(arg1, arg2, callback) {
#'       ## arg1 now equals 'one' and arg2 now equals 'two'
#'       callback(NULL, 'three');
#'     },
#'     function(arg1, callback) {
#'       ## arg1 now equals 'three'
#'       callback(NULL, 'done');
#'     }
#'   ),
#'   function (err, result) print(result)
#' ))

waterfall <- function(tasks, callback) {
  assert_that(is_task_list(tasks), is_callback(callback))
  l <- length(tasks)
  if (l == 0) return()

  task <- get_default_event_loop()$run_generic(callback)

  w <- 1
  mycallback <- function(err, ...) {
    if (!is.null(err)) return(task$callback(err, NULL))
    w <<- w + 1
    if (w > l) return(task$callback(NULL, ...))
    async_call(tasks[[w]], list(...), mycallback)
  }

  async_call(tasks[[1]], list(), mycallback)

  task$id
}

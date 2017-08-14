
#' @param task An async function that is called with
#'   a callback function. The callback function is called once
#'   the computation or I/O is done. The first argument of the
#'   callback function is the error (`NULL` if no error), and
#'   the second argument is the result (`NULL` is error).
#'
#' @export

retry <- function(task, callback, times) {
  assert_that(
    is_task(task),
    is_callback(callback),
    is_count(times)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  mycallback <- function(err, res) {
    if (is.null(err)) return(etask$callback(NULL, res))
    times <<- times - 1
    if (times) {
      task(callback = mycallback)
    } else {
      etask$callback(err, NULL)
    }
  }

  task(callback = mycallback)
  etask$id
}


#' @param async_function An async function that is called with
#'   a callback function. The callback function is called once
#'   the computation or I/O is done. The first argument of the
#'   callback function is the error (`NULL` if no error), and
#'   the second argument is the result (`NULL` is error).
#'
#' @export

retry <- function(async_function, callback, times) {
  force(async_function) ; force(callback); force(times)

  task <- get_default_event_loop()$run_generic(callback)

  mycallback <- function(err, res) {
    if (is.null(err)) return(task$callback(NULL, res))
    times <<- times - 1
    if (times) {
      async_function(mycallback)
    } else {
      task$callback(err, NULL)
    }
  }

  async_function(mycallback)
  task$id
}

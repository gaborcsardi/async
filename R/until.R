
#' @export

until <- function(test_function, async_function, callback) {
  force(test_function) ; force(async_function) ; force(callback)

  task <- get_default_event_loop()$run_generic(callback)

  mycallback <- function(err, ...) {
    if (!is.null(err)) return(task$callback(err, ...))
    if (!test_function()) {
      async_function(mycallback)
    } else {
      task$callback(NULL, ...)
    }
  }
  async_function(mycallback)

  task$id
}

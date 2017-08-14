
#' @export

until <- function(test_function, task, callback) {
  assert_that(
    is.function(test_function),
    is_task(task),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  mycallback <- function(err, ...) {
    if (!is.null(err)) return(etask$callback(err, ...))
    if (!test_function()) {
      task(mycallback)
    } else {
      etask$callback(NULL, ...)
    }
  }
  task(mycallback)

  etask$id
}

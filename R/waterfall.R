
#' @export

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
    tasks[[w]](mycallback, ...)
  }

  tasks[[1]](mycallback)

  task$id
}

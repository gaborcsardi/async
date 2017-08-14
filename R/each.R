
#' @export

each <- function(list, task, callback) {
  assert_that(
    is_vector(list),
    is_task(task),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(list)
  lapply(list, function(item) {
    task(item, function(err) {
      if (!is.null(err)) return(etask$callback(err))
      l <<- l -1
      if (l == 0) etask$callback(NULL)
    })
  })

  etask$id
}

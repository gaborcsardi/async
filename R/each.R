
#' @export

each <- function(list, async_function, callback) {
  assert_that(
    is_vector(list),
    is_async_function(async_function),
    is_callback(callback)
  )

  task <- get_default_event_loop()$run_generic(callback)

  l <- length(list)
  lapply(list, function(item) {
    async_function(item, function(err) {
      if (!is.null(err)) return(task$callback(err))
      l <<- l -1
      if (l == 0) task$callback(NULL)
    })
  })

  task$id
}

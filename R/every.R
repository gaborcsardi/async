
#' @export

every <- function(list, async_function, callback) {
  assert_that(
    is_vector(list),
    is_async_function(async_function),
    is_callback(callback)
  )

  task <- get_default_event_loop()$run_generic(callback)

  l <- length(list)
  if (l == 0) return(task$callback(NULL, TRUE))

  lapply(seq_along(list), function(i) {
    async_function(list[[i]], function(err, res) {
      if (!is.null(err)) return(task$callback(err, NULL))
      if (!res) return(task$callback(NULL, FALSE))
      l <<- l - 1
      if (l == 0) task$callback(NULL, TRUE)
    })
  })

  task$id
}

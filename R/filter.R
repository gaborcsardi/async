
#' @export

filter <- function(list, async_function, callback) {
  assert_that(
    is_vector(list),
    is_async_function(async_function),
    is_callback(callback)
  )

  task <- get_default_event_loop()$run_generic(callback)

  l <- length(list)
  if (l == 0) return(task$callback(NULL, list))

  keep <- logical(l)
  lapply(seq_along(list), function(i) {
    async_function(list[[i]], callback = function(err, res) {
      if (!is.null(err)) return(task$callback(err))
      l <<- l - 1
      keep[i] <<- res
      if (l == 0) task$callback(NULL, list[keep])
    })
  })

  task$id
}

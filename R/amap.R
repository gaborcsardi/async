
#' @export

amap <- function(list, async_function, callback) {
  assert_that(
    is_vector(list),
    is_async_function(async_function),
    is_callback(callback)
  )

  l <- length(list)
  result <- structure(
    vector(mode = "list", length = l),
    names = names(list)
  )

  task <- get_default_event_loop()$run_generic(callback)

  lapply(seq_along(list), function(i) {
    async_function(list[[i]], function(err, res) {
      if (!is.null(err)) return(task$callback(err))
      l <<- l - 1
      result[[i]] <<- res
      if (l == 0) task$callback(NULL, result)
    })
  })

  task$id
}

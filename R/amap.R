
#' @export

amap <- function(list, task, callback) {
  assert_that(
    is_vector(list),
    is_task(task),
    is_callback(callback)
  )

  l <- length(list)
  result <- structure(
    vector(mode = "list", length = l),
    names = names(list)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  lapply(seq_along(list), function(i) {
    task(list[[i]], function(err, res) {
      if (!is.null(err)) return(etask$callback(err))
      l <<- l - 1
      result[[i]] <<- res
      if (l == 0) etask$callback(NULL, result)
    })
  })

  etask$id
}

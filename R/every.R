
#' @export

every <- function(list, task, callback) {
  assert_that(
    is_vector(list),
    is_task(task),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(list)
  if (l == 0) return(etask$callback(NULL, TRUE))

  lapply(seq_along(list), function(i) {
    task(list[[i]], function(err, res) {
      if (!is.null(err)) return(etask$callback(err, NULL))
      if (!res) return(etask$callback(NULL, FALSE))
      l <<- l - 1
      if (l == 0) etask$callback(NULL, TRUE)
    })
  })

  etask$id
}

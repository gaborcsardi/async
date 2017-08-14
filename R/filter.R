
#' @export

filter <- function(list, task, callback) {
  assert_that(
    is_vector(list),
    is_task(task),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(list)
  if (l == 0) return(etask$callback(NULL, list))

  keep <- logical(l)
  lapply(seq_along(list), function(i) {
    task(list[[i]], callback = function(err, res) {
      if (!is.null(err)) return(etask$callback(err))
      l <<- l - 1
      keep[i] <<- res
      if (l == 0) etask$callback(NULL, list[keep])
    })
  })

  etask$id
}


#' @export

detect <- function(list, async_function, callback) {
  force(list) ; force(async_function) ; force(callback)

  task <- get_default_event_loop()$run_generic(callback)

  l <- length(list)
  if (l == 0) return(task$callback(NULL, NULL))

  lapply(seq_along(list), function(i) {
    async_function(list[[i]], function(err, res) {
      if (!is.null(err)) return(task$callback(err, NULL))
      if (res) return (task$callback(NULL, list[[i]]))
      l <<- l - 1
      if (l == 0) task$callback(NULL, NULL)
    })
  })

  task$id
}

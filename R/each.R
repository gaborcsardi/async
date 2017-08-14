
#' @export

each <- function(list, async_function, callback) {
  force(list) ; force(async_function) ; force(callback)
  l <- length(list)

  task <- get_default_event_loop()$run_generic(callback)

  lapply(list, function(item) {
    async_function(item, function(err) {
      if (!is.null(err)) return(task$callback(err))
      l <<- l -1
      if (l == 0) task$callback(NULL)
    })
  })

  task$id
}

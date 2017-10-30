
#' Synchronously wrap on asynchronous code
#'
#' TODO
#'
#' @param expr Async function call expression.
#'
#' @export

sync_wrap <- function(expr) {
  new_el <- event_loop$new()
  async_env$loops <- c(async_env$loops, list(new_el))
  on.exit(async_env$loops[[length(async_env$loops)]] <- NULL)
  get_default_event_loop()$run()
  await(expr)
}

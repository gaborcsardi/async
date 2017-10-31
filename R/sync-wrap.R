
#' Synchronously wrap on asynchronous code
#'
#' TODO
#'
#' @param expr Async function call expression.
#'
#' @export

sync_wrap <- function(expr) {
  new_el <- push_event_loop()
  on.exit(pop_event_loop())
  new_el$run()
  await(expr)
}

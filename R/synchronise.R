
#' Synchronously wrap on asynchronous code
#'
#' TODO
#'
#' @param expr Async function call expression.
#'
#' @export

synchronise <- function(expr) {
  new_el <- push_event_loop()
  on.exit(pop_event_loop())
  res <- expr
  if (is_deferred(res)) {
    get_private(res)$null()
    get_private(res)$run_action()
  }
  new_el$run()
  get_value_x(res)
}

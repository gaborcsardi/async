
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
  async_hide(new_el$run())
  get_value_x(res)
}

async_hide <- function(expr) expr

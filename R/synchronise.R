
#' Synchronously wrap asynchronous code
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

  if (!is_deferred(res)) return(res)


  priv <- get_private(res)
  if (! identical(priv$event_loop, new_el)) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }

  priv$null()
  priv$run_action()

  while (priv$state == "pending") new_el$run("once")

  new_el$cancel_all()

  if (priv$state == "fulfilled") priv$value else stop(priv$value)
}

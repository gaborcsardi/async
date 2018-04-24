
#' Synchronously wrap asynchronous code
#'
#' Evaluate an expression in an async phase. It creates an event loop,
#' then evaluates the supplied expression. If its result is a deferred
#' value, it keeps running the event loop, until the deferred value is
#' resolved, and returns its resolved value.
#'
#' If an error is not handled in the async phase, `synchronise()` will
#' re-throw that error.
#'
#' `synchronise()` cancels all async processes on interrupt or extrenal
#' error.
#' 
#' @param expr Async function call expression. If it does not evaluate
#' to a deferred value, then it is just returned.
#'
#' @export
#' @examples
#' http_status <- function(url, ...) {
#'   http_get(url, ...)$
#'     then(function(x) x$status_code)
#' }
#'
#' synchronise(http_status("https://httpbin.org/status/418"))

synchronise <- function(expr) {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)

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

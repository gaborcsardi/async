
#' Wait for the resolution of deferred a value
#'
#' Either until it is successfully resolved, or it is rejected.
#' While waiting for the deferred value to resolve, the async event loop
#' is running, so other async operations do *not* stop.
#'
#' If the deferred value resolves to another deferred value, then
#' it wait on that as well, and so forth.
#'
#' @param def The deferred value to wait on. If not a deferred value,
#'   then it is resolved immediately, to itself.
#' @return The value of the deferred.
#'
#' @family await functions
#' @export
#' @examples
#' afun <- async(function() {
#'   dx <- delay(1/10)$then(~ 42)
#'   print(dx)
#'   print(await(dx))
#'   print(await("foobar"))
#' })
#' synchronise(afun())

await <- function(def) {
  await_all(def)[[1]]
}

#' Wait for a list of deferred values
#'
#' Similar to [await()], but waits for a list of deferred values.
#'
#' @param ... Deferred values to wait on.
#' @param .list More deferred values to wait on.
#'
#' @family await functions
#' @export
#' @examples
#' afun <- async(function() {
#'   urls <- c("https://eu.httpbin.org?q=1", "https://eu.httpbin.org?q=2")
#'   dx <- lapply(urls, http_head)
#'   resp <- await_all(.list = dx)
#'   lapply(resp, "[[", "status_code")
#' })
#' synchronise(afun())

await_all <- function(..., .list = list()) {
  el <- get_default_event_loop()

  defs <- c(list(...), .list)

  for (d in defs) {
    if (!is_deferred(d)) next
    if (!identical(d$get_event_loop(), el)) {
      err <- make_error(
        "Cannot await() across synchronization barrier",
        class = "async_synchronization_barrier_error")
      stop(err)
    }
  }

  num_todo <- length(defs)
  for (d in defs) {
    if (!is_deferred(d)) {
      num_todo <- num_todo - 1
    } else {
      d$then(
        function(value) num_todo <<- num_todo - 1,
        function(reason) num_todo <<- num_todo - 1
      )
    }
  }

  while (num_todo > 0) el$run("once")

  lapply(defs, get_value_x)
}

#' Wait for any of the deferred values to resolve
#'
#' Returns as soon as any of the specified deferred values is resolved or
#' rejected. If multiple deferred values resolved, then an arbitrary one
#' is returned.
#'
#' @param ... Deferred values.
#' @param .list List of deferred values.
#'
#' @family await functions
#' @export
#' @examples
#' # Returns as soon as one timer expires
#' afun <- async(function() {
#'   t1 <- delay(1)$then(~ 1)
#'   t2 <- delay(1/1000)$then(~ 2)
#'   await_any(t1, t2)
#' })
#' synchronise(afun())

await_any <- function(..., .list = list()) {
  el <- get_default_event_loop()

  defs <- c(list(...), .list)

  for (d in defs) {
    if (!is_deferred(d)) next
    if (!identical(d$get_event_loop(), el)) {
      err <- make_error(
        "Cannot await() across synchronization barrier",
        class = "async_synchronization_barrier_error")
      stop(err)
    }
  }

  num_done <- 0
  for (d in defs) {
    if (!is_deferred(d)) {
      num_done <- num_done + 1
      break;
    } else {
      d$then(
        function(value) num_done <<- num_done + 1,
        function(reason) num_done <<- num_done + 1
      )
    }
  }

  while (num_done == 0) el$run("once")

  states <- vcapply(defs, get_state_x)
  get_value_x(defs[states != "pending"][[1]])
}

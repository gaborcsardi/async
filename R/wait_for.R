
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
#' @family wait_for functions
#' @export
#' @examples
#' dx <- delay(1/10)$then(~ 42)
#' dx
#' wait_for(dx)
#' wait_for("foobar")

wait_for <- function(def) {
  wait_for_all(def)[[1]]
}

#' Wait for a list of deferred values
#'
#' Similar to [wait_for()], but waits for a list of deferred values.
#'
#' @param ... Deferred values to wait on.
#' @param .list More deferred values to wait on.
#'
#' @family wait_for functions
#' @export
#' @examples
#' urls <- c("https://httpbin.org?q=1", "https://httpbin.org?q=2")
#' dx <- lapply(urls, http_head)
#' resp <- wait_for_all(.list = dx)
#' lapply(resp, "[[", "status_code")

wait_for_all <- function(..., .list = list()) {
  defs <- c(list(...), .list)
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

  while (num_todo > 0) get_default_event_loop()$run("once")

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
#' @family wait_for functions
#' @export
#' @examples
#' # Returns as soon as one timer expires
#' t1 <- delay(1)$then(~ 1)
#' t2 <- delay(1/1000)$then(~ 2)
#' wait_for_any(t1, t2)

wait_for_any <- function(..., .list = list()) {
  defs <- c(list(...), .list)
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

  while (num_done == 0) get_default_event_loop()$run("once")

  states <- vcapply(defs, get_state_x)
  get_value_x(defs[states != "pending"][[1]])
}

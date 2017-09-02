
get_state_x <- function(x) {
  if (is.deferred(x)) x$get_state() else "not-deferred"
}

get_value_x <- function(x) {
  if (is.deferred(x)) x$get_value() else x
}

#' @export

await <- function(def) {
  await_list(def)[[1]]
}

#' @export

await_list <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  states <- vcapply(defs, get_state_x)
  while (any(states == "pending")) {
    get_default_event_loop()$run("once")
    states <- vcapply(defs, get_state_x)
  }
  lapply(defs, get_value_x)
}

#' @export

await_any <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  states <- vcapply(defs, get_state_x)
  while (all(states == "pending")) {
    get_default_event_loop()$run("once")
    states <- vcapply(defs, get_state_x)
  }
  get_value_x(defs[states != "pending"][[1]])
}

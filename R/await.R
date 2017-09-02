
#' @export

await <- function(def) {
  await_list(def)[[1]]
}

#' @export

await_list <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  num_todo <- length(defs)
  for (d in defs) {
    if (!is.deferred(d)) {
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

#' @export

await_any <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  num_done <- 0
  for (d in defs) {
    if (!is.deferred(d)) {
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

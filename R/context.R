
async_env <- new.env(parent = emptyenv())
async_env$loops <- list()

#' Default event loop of the R session.
#'
#' This event loop is created when the `async` package is loaded, and
#' all asyncronous constructs use this event loop by default.
#'
#' @return The default event loop of the R session.
#'
#' @seealso [event_loop]
#' @keywords internal

get_default_event_loop <- function() {
  num_loops <- length(async_env$loops)
  if (num_loops == 0) {
    stop("You can only call async functions from an async context")
  }

  async_env$loops[[num_loops]]
}

push_event_loop <- function() {
  new_el <- event_loop$new()
  async_env$loops <- c(async_env$loops, list(new_el))
  new_el
}

pop_event_loop <- function() {
  async_env$loops[[length(async_env$loops)]] <- NULL
}

start_dev_async_context <- function() {
  push_event_loop()
}

stop_dev_async_context <- function() {
  num_loops <- length(async_env$loops)
  if (num_loops != 1) {
    stop("Multiple async contexts, cannot remove dev context")
  }
  pop_event_loop()
}


async_env <- new.env(parent = emptyenv())
async_env$loops <- list()

get_default_event_loop <- function() {
  num_loops <- length(async_env$loops)
  if (num_loops == 0) {
    err <- make_error(
      "You can only call async functions from an async context",
      class = "async_synchronization_barrier_error"
    )
    stop(err)
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
  if (num_loops == 0) {
    warning("Dev async context not running")
  } else if (num_loops > 1) {
    stop("Multiple async contexts, cannot remove dev context")
  } else {
    pop_event_loop()
  }
}

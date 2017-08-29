
## TODO: think about error handling

#' Event loop
#'
#' @section Usage:
#' ```
#' el <- event_loop$new()
#'
#' el$wait_for(ids)
#' el$wait_for_all()
#'
#' el$run_http(handle, callback)
#' el$run_set_timeout(delay, callback)
#' el$run_generic(callback, ...)
#' ```
#'
#' @section Arguments:
#' \describe{
#'   \item{ids}{The task ids to wait for.}
#'   \item{handle}{A `curl` handle to use for the `HTTP` operation.}
#'   \item{callback}{Callback function to call when the asynchronous
#'      operation is done. See details below.}
#'   \item{delay}{Number of seconds to delay the execution of the callback.}
#'   \item{...}{Additional arguments to store in the task. These are
#'      currently not used for anything.}
#' }
#'
#' @section Details:
#' `$wait_for()` waits for all specified tasks to finish.
#'
#' `$wait_for_all()` waits for all tasks managed by the event loop to finish.
#'
#' `$run_http()` starts an asynchronous HTTP request, with the specified
#' `curl` handle. Once the request is done, and the response is available
#' (or an error happens), the callback is called with two arguments, the
#' error object or message (if any) and the `curl` response object.
#'
#' `$run_set_timeout()` starts a task with the specified delay.
#'
#' `$run_generic()` creates a generic task. It is supposed to take care of
#' calling its own callback itself. Tasks created by the asynchronous
#' control flow structures and the asynchronous iterators generic tasks.
#'
#' @section The default event loop:
#'
#' The `async` package creates a default event loop when it is loaded.
#' All asyncronous constructs use this event loop by default.
#'
#' @name event_loop
NULL

#' @importFrom R6 R6Class
#' @export

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function()
      el_init(self, private),
    wait_for = function(ids)
      el_wait_for(self, private, ids),
    wait_for_all = function()
      el_wait_for_all(self, private),

    run_http = function(handle, callback)
      el_run_http(self, private, handle, callback),
    run_set_timeout = function(delay, callback)
      el_run_set_timeout(self, private, delay, callback),
    run_generic = function(callback, ...)
      el_run_generic(self, private, callback, ...),

    defer_next_tick = function(callback, args = list())
      el_defer_next_tick(self, private, callback, args)
  ),

  private = list(
    poll = function(mode = c("once", "done-one"))
      el__poll(self, private, mode = match.arg(mode)),
    create_task = function(callback, ...)
      el__create_task(self, private, callback, ...),
    ensure_pool = function(...)
      el__ensure_pool(self, private, ...),
    get_poll_timeout = function(current)
      el__get_poll_timeout(self, private, current),
    do_next_tick = function()
      el__do_next_tick(self, private),
    fire_timers = function(current)
      el__fire_timers(self, private, current),

    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL,
    next_ticks = list()
  )
)

el_init <- function(self, private) {
  reg.finalizer(self, function(me) me$wait_for_all(), onexit = TRUE)
  invisible(self)
}

#' @importFrom curl multi_add

el_run_http <- function(self, private, handle, callback) {
  force(self) ; force(private) ; force(handle) ; force(callback)
  id <- private$create_task(callback, data = list(handle = handle))
  private$ensure_pool()
  multi_add(
    handle = handle,
    pool = private$pool,
    done = function(response) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      task$callback(NULL, response)
    },
    fail = function(error) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      task$callback(error, NULL)
    }
  )
  id
}

el_run_set_timeout <- function(self, private, delay, callback) {
  force(self) ; force(private) ; force(delay) ; force(callback)
  id <- private$create_task(callback, data = list(delay = delay))
  private$timers[id] <- Sys.time() + as.difftime(delay, units = "secs")
  id
}

el_run_generic <- function(self, private, callback, ...) {
  force(self) ; force(private); force(callback)
  data <- list(...)
  force(data)

  id <- private$create_task(callback, data = data)
  mycallback <-function(...) {
    private$tasks[[id]] <- NULL
    if (!is.null(callback)) callback(...)
    id
  }
  list(id = id, callback = mycallback)
}

el_wait_for <- function(self, private, ids) {
  while (any(ids %in% names(private$tasks))) private$poll(mode = "done-one")
}

el_wait_for_all <- function(self, private) {
  while (length(private$tasks)) private$poll(mode = "done-one")
}

el_defer_next_tick <- function(self, private, callback, args) {
  private$next_ticks <- append(
    private$next_ticks,
    list(list(callback, args))
  )
}

#' @importFrom curl multi_run

el__poll <- function(self, private, mode) {
  if (!length(private$tasks)) return()
  current <- Sys.time()
  timeout <- private$get_poll_timeout(current)
  if (mode == "once") {
    if (!is.null(private$pool)) {
      multi_run(timeout = timeout, poll = TRUE, pool = private$pool)
    }
  } else if (mode == "done-one") {
    if (!is.null(private$pool)) {
      multi_run(timeout = timeout, poll = TRUE, pool = private$pool)
    } else {
      Sys.sleep(timeout)
    }
  }
  private$fire_timers(Sys.time())
  private$do_next_tick()
}

el__do_next_tick <- function(self, private) {
  next_ticks <- private$next_ticks
  private$next_ticks <- list()
  for (nt in next_ticks) {
    do.call(nt[[1]], nt[[2]])
  }
}

#' @importFrom uuid UUIDgenerate

el__create_task <- function(self, private, callback, data, ...) {
  id <- UUIDgenerate()
  private$tasks[[id]] <- list(
    id = id,
    callback = callback,
    data = data,
    error = NULL,
    result = NULL
  )
  id
}

#' @importFrom curl new_pool

el__ensure_pool <- function(self, private, ...) {
  if (is.null(private$pool)) private$pool <- new_pool(...)
}

el__get_poll_timeout <- function(self, private, current) {
  max(0, min(Inf, private$timers - current))
}

el__fire_timers <- function(self, private, current) {
  expired <- names(private$timers)[private$timers <= current]
  for (id in expired) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    private$timers <- private$timers[setdiff(names(private$timers), id)]
    task$callback()
  }
}

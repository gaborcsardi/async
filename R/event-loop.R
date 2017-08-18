
## TODO: think about error handling

#' Event loop
#'
#' @section Usage:
#' ```
#' el <- event_loop$new()
#'
#' el$await(ids)
#' el$await_all()
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
#' `$await()` waits for all specified tasks to finish.
#'
#' `$await_all()` waits for all tasks managed by the event loop to finish.
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
    await = function(ids)
      el_await(self, private, ids),
    await_all = function()
      el_await_all(self, private),

    run_http = function(handle, callback)
      el_run_http(self, private, handle, callback),
    run_set_timeout = function(delay, callback)
      el_run_set_timeout(self, private, delay, callback),
    run_generic = function(callback, ...)
      el_run_generic(self, private, callback, ...)
  ),

  private = list(
    poll = function()
      el__poll(self, private),
    create_task = function(callback, ...)
      el__create_task(self, private, callback, ...),
    ensure_pool = function(...)
      el__ensure_pool(self, private, ...),
    get_poll_timeout = function(current)
      el__get_poll_timeout(self, private, current),

    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL
  )
)

#' @importFrom later later

el_init <- function(self, private) {
  reg.finalizer(self, function(me) me$await_all(), onexit = TRUE)
  later(function() private$poll())
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
  later(
    function() {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      private$timers <- private$timers[setdiff(names(private$times), id)]
      tryCatch(task$callback(), error = function(e) NULL)
    },
    delay
  )
  id
}

el_run_generic <- function(self, private, callback, ...) {
  force(self) ; force(private); force(callback)
  data <- list(...)
  force(data)

  id <- private$create_task(callback, data = data)
  mycallback <-function(...) {
    private$tasks[[id]] <- NULL
    callback(...)
    id
  }
  list(id = id, callback = mycallback)
}

el_await <- function(self, private, ids) {
  while (any(ids %in% names(private$tasks))) private$poll()
}

el_await_all <- function(self, private) {
  while (length(private$tasks)) private$poll()
}

#' @importFrom curl multi_run
#' @importFrom later run_now

el__poll <- function(self, private) {
  current <- Sys.time()
  timeout <- private$get_poll_timeout(current)
  if (!is.null(private$pool)) {
    multi_run(timeout = timeout, poll = TRUE, pool = private$pool)
  }
  run_now()
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
  min(
    Inf,
    max(0, min(Inf, private$timers - current))
  )
}

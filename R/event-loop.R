
## TODO: think about error handling

#' Event loop
#'
#' @section Usage:
#' ```
#' el <- event_loop$new()
#'
#' el$run_http(handle, callback)
#' el$run_delay(delay, callback)
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
#' `$run_http()` starts an asynchronous HTTP request, with the specified
#' `curl` handle. Once the request is done, and the response is available
#' (or an error happens), the callback is called with two arguments, the
#' error object or message (if any) and the `curl` response object.
#'
#' `$run_delay()` starts a task with the specified delay.
#'
#' @section The default event loop:
#'
#' The `async` package creates a default event loop when it is loaded.
#' All asyncronous constructs use this event loop by default.
#'
#' @name event_loop
#' @keywords internal
NULL

#' @importFrom R6 R6Class

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function()
      el_init(self, private),

    run_http = function(handle, callback)
      el_run_http(self, private, handle, callback),
    run_delay = function(delay, callback)
      el_run_delay(self, private, delay, callback),

    defer_next_tick = function(callback, args = list())
      el_defer_next_tick(self, private, callback, args),

    run = function(mode = c("default", "nowait", "once"))
      el_run(self, private, mode = match.arg(mode))

  ),

  private = list(
    create_task = function(callback, ...)
      el__create_task(self, private, callback, ...),
    ensure_pool = function(...)
      el__ensure_pool(self, private, ...),
    get_poll_timeout = function()
      el__get_poll_timeout(self, private),
    run_pending = function()
      el__run_pending(self, private),
    run_timers = function()
      el__run_timers(self, private),
    is_alive = function()
      el__is_alive(self, private),
    update_time = function()
      el__update_time(self, private),

    time = Sys.time(),
    stop_flag = FALSE,
    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL,
    next_ticks = list()
  )
)

el_init <- function(self, private) {
  ## TODO
  ## reg.finalizer(self, function(me) me$run("default"), onexit = TRUE)
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

el_run_delay <- function(self, private, delay, callback) {
  force(self) ; force(private) ; force(delay) ; force(callback)
  id <- private$create_task(callback, data = list(delay = delay))
  private$timers[id] <- Sys.time() + as.difftime(delay, units = "secs")
  id
}

el_defer_next_tick <- function(self, private, callback, args) {
  private$next_ticks <- append(
    private$next_ticks,
    list(list(callback, args))
  )
}

#' @importFrom curl multi_run

el_run <- function(self, private, mode) {

  ## This is closely modeled after the libuv event loop, on purpose,
  ## because some time we might switch to that.
  alive <- private$is_alive()
  if (! alive) private$update_time()

  while (alive && ! private$stop_flag) {
    private$update_time()
    private$run_timers()
    ran_pending <- private$run_pending()
    ## private$run_idle()
    ## private$run_prepare()

    timeout <- 0
    if (mode == "once" && !ran_pending || mode == "default") {
      timeout <- private$get_poll_timeout()
    }
    multi_run(timeout = timeout, poll = TRUE, pool = private$pool)

    ## private$run_check()
    ## private$run_closing_handles()

    if (mode == "once") {
      private$update_time()
      private$run_timers()
    }

    alive <- private$is_alive()
    if (mode == "once" || mode == "nowait") break
  }

  private$stop_flag <- FALSE

  alive
}

el__run_pending <- function(self, private) {
  next_ticks <- private$next_ticks
  private$next_ticks <- list()
  for (nt in next_ticks) {
    do.call(nt[[1]], nt[[2]])
  }

  length(next_ticks) > 0
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

el__get_poll_timeout <- function(self, private) {
  max(0, min(Inf, private$timers - private$time))
}

el__run_timers <- function(self, private) {
  expired <- names(private$timers)[private$timers <= private$time]
  expired <- expired[order(private$timers[expired])]
  for (id in expired) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    private$timers <- private$timers[setdiff(names(private$timers), id)]
    task$callback()
  }
}

el__is_alive <- function(self, private) {
  length(private$tasks) > 0 ||
    length(private$timers) > 0 ||
    length(private$next_ticks) > 0
}

el__update_time <- function(self, private) {
  private$time <- Sys.time()
}

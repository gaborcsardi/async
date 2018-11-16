
#' Repeated timer
#'
#' The supplied callback function will be called by the event loop
#' every `delay` seconds.
#'
#' @section Usage:
#' ```
#' t <- async_timer$new(delay, callback)
#' t$cancel()
#' ```
#'
#' @section Arguments:
#' * `delay`: Time interval in seconds, the amount of time to delay
#'   to delay the execution. It can be a fraction of a second.
#' * `callback`: Callback function without arguments. It will be called
#'   from the event loop every `delay` seconds.
#'
#' @section Details:
#'
#' An `async_timer` is an `[event_emitter]` object with a `timeout` event.
#' It is possible to add multiple listeners to this event, once the timer
#' is created. Note, however, that removing all listeners does not cancel
#' the timer, `timeout` events will be still emitted as usual.
#' For proper cancellation you'll need to call the `cancel()` method.
#'
#' It is only possible to create `async_timer` objects in an asynchronous
#' context, i.e. within a `synchronise()` or `run_event_loop()` call.
#' A `synchronise()` call finishes as soon as its returned deferred value
#' is resolved (or rejected), even if some timers are still active. The
#' active timers will be automatically cancelled in this case.
#'
#' @section Errors:
#' Errors are handled the same way as for generic event emitters. I.e. to
#' catch errors thrown in the `callback` function, you need to add a
#' listener to the `error` event, see the example below.
#'
#' @section Congestion:
#' `async_timer` is _not_ a real-time timer. In particular, if `callback`
#' does not return in time, before the next timer event, then all future
#' timer events will be delayed. Even if `callback` returns promptly, the
#' event loop might be busy with other events, and then the next timer
#' event is not emitted in time. In general there is no guarantee about
#' the timing of the timer events.
#'
#' @importFrom R6 R6Class
#' @export
#' @examples
#' ## Call 10 times a second, cancel with 1/10 probability
#' counter <- 0L
#' do <- function() {
#'   cb <- function() {
#'     cat("called\n")
#'     counter <<- counter + 1L
#'     if (runif(1) < 0.1) t$cancel()
#'   }
#'   t <- async_timer$new(1/10, cb)
#' }
#'
#' run_event_loop(do())
#' counter
#'
#' ## Error handling
#' counter <- 0L
#' do <- function() {
#'   cb <- function() {
#'     cat("called\n")
#'     counter <<- counter + 1L
#'     if (counter == 2L) stop("foobar")
#'     if (counter == 3L) t$cancel()
#'   }
#'   t <- async_timer$new(1/10, cb)
#'   handler <- function(err) {
#'     cat("Got error:", sQuote(conditionMessage(err)), ", handled\n")
#'   }
#'   t$listen_on("error", handler)
#' }
#'
#' run_event_loop(do())
#' counter
#'
#' ## Error handling at the synchonization point
#' counter <- 0L
#' do <- function() {
#'   cb <- function() {
#'     cat("called\n")
#'     counter <<- counter + 1L
#'     if (counter == 2L) stop("foobar")
#'     if (counter == 3L) t$cancel()
#'   }
#'   t <- async_timer$new(1/10, cb)
#' }
#'
#' tryCatch(run_event_loop(do()), error = function(x) x)
#' counter

async_timer <- R6Class(
  "async_timer",
  inherit = event_emitter,
  public = list(
    initialize = function(delay, callback)
      async_timer_init(self, private, super, delay, callback),
    cancel = function()
      async_timer_cancel(self, private)
  ),

  private = list(
    id = NULL
  )
)

async_timer_init <- function(self, private, super, delay, callback) {
  assert_that(
    is_time_interval(delay),
    is.function(callback) && length(formals(callback)) == 0)

  ## event emitter
  super$initialize()

  private$id <- get_default_event_loop()$add_delayed(
    delay,
    function() self$emit("timeout"),
    function(err, res) {
      if (!is.null(err)) self$emit("error", err)              # nocov
    },
    rep = TRUE)

  self$listen_on("timeout", callback)

  invisible(self)
}

async_timer_cancel  <- function(self, private) {
  self; private
  self$remove_all_listeners("timeout")
  get_default_event_loop()$cancel(private$id)
  invisible(self)
}

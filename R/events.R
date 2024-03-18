
#' Generic Event Emitter
#'
#' This is a generic class that can be used to create event emitters.
#' It is mostly modelled after the 'node.js' `EventEmitter` class
#'
#' @section Usage:
#' ```
#' ee <- event_emitter$new(async = TRUE)
#' ee$listen_on(event, callback)
#' ee$listen_off(event, callback)
#' ee$listen_once(event, callback)
#' ee$emit(event, ...)
#' ee$get_event_names()
#' ee$get_listener_count(event)
#' ee$remove_all_listeners(event)
#' ```
#'
#' @section Arguments:
#' * `async`: Whether to call listeners asynchronously, i.e. in the next
#'     tick of the event loop.
#' * `event`: String, name of the event.
#' * `callback`: Function, listener to call when the event is emitted.
#'     Its arguments must match the arguments passed to the `$emit()`
#'     method. It is possible to add the same callback function multiple
#'     times as a listener. It will be called as many times, as many times
#'     it was added.
#' * `...`: Arguments to pass to the listeners. They can be named or
#'     unnnamed.
#'
#' @section Details:
#'
#' `ee$listen_on()` adds `callback` as a new listener for `event`. It is
#' always added to the end of the listener list. Listeners will be called in
#' the order they were added. It returns a reference to the `event_emitter`
#' object, so calls can be chained.
#'
#' `ee$listen_off()` removes the first instance of `callback` from the
#' listener list of `event`. It uses [base::identical()] to find the
#' listener to remove. If `callback` is not among the listeners, nothing
#' happens. Note that if you call this method from an event handler, that
#' does not affect the already emitted events. It returns a reference to
#' the `event_emitter` object, so calls can be chained.
#'
#' `ee$listen_once` is similar to `ee$listen_on()`, but the callback will
#' be only called for a single event, and then it will be removed.
#' (Technically, the listener is removed before the callback is called.)
#' It returns a reference to the `event_emitter` object, so calls can be
#' chained.
#'
#' `ee$emit()` emits an event. All listeners in its listener list will be
#' called, in the order they were added. The arguments are passed to the
#' listeners, so they have to be compatible with them.
#'
#' `ee$get_event_names()` returns the names of the active events,
#' in a character vector. An event is active if it has at least one
#' listener.
#'
#' `ee$get_listener_count()` returns the number of listeners for an event.
#'
#' `ee$remove_all_listener()`  removes all listeners for an an event.
#'
#' @section Error handling:
#' Errors are handled by special `error` events. If a listener errors,
#' and the event emitter has an active `error` event (i.e. some listeners
#' exist for `error`, then _all_ listeners are called, in the order they
#' were specified. They receive the originally thrown error object as the
#' single argument. The error object has an `event` entry, which contains
#' the event name the failed listener was called on.
#'
#' If the event emitter does not have any listeners for the `error` event,
#' then it throws an error. This error propagates to the next
#' synchronization barrier, i.e. the last `synchronise()` or
#' `run_event_loop()` call, which fails.
#'
#' In an error happen within an `error` listener, then the same happens,
#' the last `synchronise()` or `run_event_loop()` call fails. You can
#' wrap the body of the error listeners in a `tryCatch()` call,
#' if you want to avoid this.
#'
#' @export
#' @importFrom R6 R6Class

event_emitter <- R6Class(
  "event_emitter",
  public = list(
    initialize = function(async = TRUE)
      ee_init(self, private, async),

    listen_on = function(event, callback)
      ee_listen_on(self, private, event, callback),

    listen_off = function(event, callback)
      ee_listen_off(self, private, event, callback),

    listen_once = function(event, callback)
      ee_listen_once(self, private, event, callback),

    emit = function(event, ...)
      ee_emit(self, private, event, ...),

    get_event_names = function()
      ee_get_event_names(self, private),

    get_listener_count = function(event)
      ee_get_listener_count(self, private, event),

    remove_all_listeners = function(event)
      ee_remove_all_listeners(self, private, event)
  ),

  private = list(
    lsts = NULL,
    async = NULL,

    cleanup_events = function()
      ee__cleanup_events(self, private),
    error_callback = function(err, res)
      ee__error_callback(self, private, err, res)
  )
)

ee_init <- function(self, private, async) {
  assert_that(is_flag(async))
  private$lsts <- structure(list(), names = character())
  private$async <- async
  invisible(self)
}

ee_listen_on <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = FALSE)))
  invisible(self)
}

ee_listen_off <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  for (idx in seq_along(private$lsts[[event]])) {
    if (identical(private$lsts[[event]][[idx]]$cb, callback)) {
      private$lsts[[event]] <- private$lsts[[event]][-idx]
      break
    }
  }
  invisible(self)
}

ee_listen_once <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = TRUE)))
  invisible(self)
}

ee_emit <- function(self, private, event, ...) {
  assert_that(is_string(event))
  list(...)
  tocall <- private$lsts[[event]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[[event]] <- tocall[!once]

  ## a for loop is not good here, because it does not create
  ## a closure for lst
  lapply(tocall, function(lst) {
    lst
    if (private$async) {
      get_default_event_loop()$add_next_tick(
        function() lst$cb(...),
        private$error_callback,
        data = list(error_info = list(event = event)))

    } else {
      call_with_callback(
        function() lst$cb(...),
        private$error_callback,
        info = list(event = event))
    }
  })

  invisible(self)
}

ee_get_event_names <- function(self, private) {
  private$cleanup_events()
  names(private$lsts)
}

ee_get_listener_count <- function(self, private, event) {
  assert_that(is_string(event))
  length(private$lsts[[event]])
}

ee_remove_all_listeners <- function(self, private, event) {
  assert_that(is_string(event))
  private$lsts[[event]] <- NULL
  invisible(self)
}

ee__cleanup_events <- function(self, private) {
  len <- viapply(private$lsts, length)
  private$lsts <- private$lsts[len > 0]
}

ee__error_callback <- function(self, private, err, res) {
  if (is.null(err)) return()
  tocall <- private$lsts[["error"]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[["error"]] <- tocall[!once]

  if (length(tocall)) {
    for (lst in tocall) lst$cb(err)
  } else {
    stop(err)
  }
}

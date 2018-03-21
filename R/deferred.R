
#' Deferred value
#'
#' @section Usage:
#' ```
#' dx <- deferred$new(action)
#'
#' dx$get_value()
#' dx$then(on_fulfilled = NULL, on_rejected = NULL)
#' dx$catch(on_rejected)
#' dx$finally(on_finally)
#' ```
#'
#' @section Arguments:
#' \describe{
#'   \item{action}{Function to kick off asynchronous I/O or computation.
#'     See more below.}
#'   \item{on_fulfilled}{Function to call when the deferred value was
#'     success fully resolved.}
#'   \item{on_rejected}{Function to call when the deferrred value was
#'     rejected because of an error.}
#'   \item{on_finally}{Function to call after the deferred was resolved or
#'     rejected.}
#' }
#'
#' @section Details:
#'
#' `deferred$new` creates a new deferred value. It argument is an `action`
#' function, which must have two arguments: `resolve` and `reject`.
#' The `action` function should be a piece of code that returns quickly, but
#' initiates a potentially long-running, asynchronous task. If/when the task
#' successfully completes, call `resolve(value)` where `value` is the
#' result of the I/O or computation (like the return value). If the task
#' fails, call `reject(reason)`, where `reason` is either an error object,
#' or a character string.
#'
#' `dx$get_value()` returns the resolved value, or the error message or
#' object of a deferred value. It is an error to call this method on a
#' deferred value that is pending.
#'
#' `dx$then()` creates a deferred value whose resolution (and rejection)
#' depends on the `dx` deferred value. When `dx` is successfully
#' resolved, the `on_fulfilled` function is called, with the resolved
#' value as an argument. If `dx` is rejected, then `on_rejected` is called,
#' with the error object or message as the argument.
#'
#' Note that the deferred value created by `dx$then()` will resolve
#' successfully, unless an error is throws from within `on_fulfilled` or
#' `on_rejected`. Whether `dx` was rejected or not, does not matter in this
#' case. This allows using `on_rejected` as an error handler.
#'
#' `dx$catch()` is a shortcut to provide an error handler, it is equivalent
#' to `dx$thin()` with `on_fulfilled` set to `NULL`.
#'
#' `dx$finally()` makes sure that `on_finally` runs once `dx` is resolved
#' or rejected. It is ideal to specifying cleanup functions, e.g. closing
#' a database.
#'
#' @name deferred
NULL

#' @importFrom R6 R6Class
#' @export

deferred <- R6Class(
  "deferred",
  public = list(
    initialize = function(action, on_progress = NULL, on_cancel = NULL,
                          lazy = TRUE)
      async_def_init(self, private, action, on_progress, on_cancel, lazy),
    get_value = function()
      def_get_value(self, private),
    then = function(on_fulfilled = NULL, on_rejected = NULL)
      def_then(self, private, on_fulfilled, on_rejected),
    catch = function(on_rejected)
      def_catch(self, private, on_rejected),
    finally = function(on_finally)
      def_finally(self, private, on_finally),
    cancel = function(reason = NULL)
      def_cancel(self, private, reason),
    null = function()
      def_null(self, private),

    get_event_loop = function() private$event_loop
  ),

  private = list(
    state = c("pending", "fulfilled", "rejected")[1],
    event_loop = NULL,
    value = NULL,
    on_fulfilled = list(),
    on_rejected = list(),
    progress_callback = NULL,
    cancel_callback = NULL,
    cancelled = FALSE,
    start_stack = NULL,
    dead_end = FALSE,

    resolve = function(value)
      def__resolve(self, private, value),
    reject = function(reason)
      def__reject(self, private, reason),
    progress = function(data)
      def__progress(self, private, data),

    make_error_object = function(err)
      def__make_error_object(self, private, err)
  )
)

async_def_init <- function(deferred, private, action, on_progress,
                           on_cancel, lazy) {

  private$event_loop <- get_default_event_loop()

  if (!is.function(action)) {
    action <- as_function(action)
    formals(action) <- alist(resolve = NULL, reject = NULL,
                             progress = NULL)
  }
  assert_that(is_action_function(action))
  assert_that(is.null(on_progress) || is.function(on_progress))
  private$progress_callback <- on_progress
  assert_that(is.null(on_cancel) || is.function(on_cancel))
  private$cancel_callback <- on_cancel

  action_args <- names(formals(action))
  args <- list(private$resolve, private$reject)
  if (!is.na(pr_arg <- match("progress", action_args))) {
    args$progress <- private$progress
  }

  if (lazy) {
    private$event_loop$add_next_tick(
      function() do.call(action, args),
      function(err, res) if (!is.null(err)) stop(err))

  } else {
    do.call(action, args)
  }

  invisible(deferred)
}

def_get_value <- function(self, private) {
  if (private$state == "pending") {
    stop("Deferred value not resolved yet")
  } else if (private$state == "rejected") {
    stop(private$value)
  } else {
    private$value
  }
}

make_then_function <- function(func, value) {
  func; value
  function() {
    if (is.function(func)) {
      if (num_args(func) >= 1) {
        func(value)
      } else {
        func()
      }
    } else {
      value
    }
  }
}

def_then <- function(self, private, on_fulfilled, on_rejected) {
  force(self)
  force(private)

  if (! identical(private$event_loop, get_default_event_loop())) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }

  on_fulfilled <- if (!is.null(on_fulfilled)) as_function(on_fulfilled)
  on_rejected  <- if (!is.null(on_rejected))  as_function(on_rejected)

  deferred$new(lazy = FALSE, function(resolve, reject) {
    force(resolve)
    force(reject)

    handle <- function(func) {
      force(func)
      function(value) {
        private$event_loop$add_next_tick(
          make_then_function(func, value),
          function(err, res) if (is.null(err)) resolve(res) else reject(err)
        )
      }
    }

    if (private$state == "pending") {
      private$on_fulfilled <- c(private$on_fulfilled,
                                list(handle(on_fulfilled)))
      private$on_rejected <- c(private$on_rejected,
                               list(handle(on_rejected %||% stop)))

    } else if (private$state == "fulfilled") {
      handle(on_fulfilled)(private$value)

    } else if (private$state == "rejected") {
      handle(on_rejected %||% stop)(private$value)
    }
  })
}

def_catch <- function(self, private, on_rejected) {
  force(on_rejected)
  self$then(on_rejected = on_rejected)
}

def_finally <- function(self, private, on_finally) {
  force(on_finally)
  self$then(
    on_fulfilled = function(value) {
      on_finally()
      value
    },
    on_rejected = function(reason) {
      on_finally()
      stop(reason)
    }
  )
}

def_cancel <- function(self, private, reason) {
  if (private$state != "pending") return()
  cancel_cond <- structure(
    list(message = reason %||% "Deferred computation cancelled", call = NULL),
    class = c("async_cancelled", "error", "condition")
  )
  private$reject(cancel_cond)
}

def_null <- function(self, private) {
  def__dead_end(self)
  invisible(self)
}

def__resolve <- function(self, private, value) {
  if (private$cancelled) return()
  if (private$state != "pending") stop("Deferred value already resolved")
  if (is_deferred(value)) {
    dx <- value$then(private$resolve, private$reject)
    def__dead_end(dx)
  } else {
    if (!private$dead_end && !length(private$on_fulfilled)) {
      stop("Computation going nowhere...")
    }
    private$state <- "fulfilled"
    private$value <- value
    for (f in private$on_fulfilled) f(value)
    private$on_fulfilled <- list()
  }
}

#' Create an error object for a rejected deferred computation
#'
#' * Make sure that the error is an error object.
#' * Make sure that the error has the correct classes.
#'
#' @param self self
#' @param private private self
#' @return error object
#'
#' @keywords internal

def__make_error_object <- function(self, private, err) {
  class(err) <- unique(c("async_rejected", class(err)))
  private$value <- err
}

def__reject <- function(self, private, reason) {
  if (private$cancelled) return()
  if (private$state != "pending") stop("Deferred value already rejected")
  if (is_deferred(reason)) {
    dx <- reason$then(private$resolve, private$reject)
    def__dead_end(dx)
  } else {
    private$state <- "rejected"
    private$make_error_object(reason)
    if (inherits(private$value, "async_cancelled")) {
      private$cancelled <- TRUE
      if (!is.null(private$cancel_callback)) {
        private$cancel_callback(conditionMessage(private$value))
      }
    }
    for (f in private$on_rejected) f(private$value)
    private$on_rejected <- list()
  }
}

def__progress <- function(self, private, data) {
  if (private$state != "pending") return()
  if (is.null(private$progress_callback)) return()
  private$progress_callback(data)
}

#' Is object a deferred value?
#'
#' @param x object
#' @return Whether it is a deferred value.
#'
#' @export
#' @examples
#' is_deferred(1:10)
#' afun <- function() {
#'   print(is_deferred(dx <- delay(1/100)))
#'   dx
#' }
#' synchronise(afun())

is_deferred <- function(x) {
  inherits(x, "deferred")
}

def__dead_end <- function(def) {
  def$.__enclos_env__$private$dead_end <- TRUE
}

def__cancel_pending <- function(defs, cancel) {
  if (cancel) {
    for (i in seq_along(defs)) {
      if (is_deferred(defs[[i]])) {
        defs[[i]]$null()
        defs[[i]]$cancel()
      }
    }
  }
}

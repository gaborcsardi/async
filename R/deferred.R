
#' Deferred value
#'
#' @section Usage:
#' ```
#' dx <- deferred$new(action)
#'
#' dx$get_state()
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
#' `dx$get_state()` returns the state of the deferred value. A deferred
#' value can be in three states: `"pending"`, `"resolved"` or `"rejected"`.
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
    initialize = function(action, on_progress = NULL, on_cancel = NULL)
      def_init(self, private, action, on_progress, on_cancel),
    get_state = function()
      private$state,
    get_value = function()
      def_get_value(self, private),
    then = function(on_fulfilled = NULL, on_rejected = NULL)
      def_then(self, private, on_fulfilled, on_rejected),
    catch = function(on_rejected)
      def_catch(self, private, on_rejected),
    finally = function(on_finally)
      def_finally(self, private, on_finally),
    cancel = function(reason = NULL)
      def_cancel(self, private, reason)
  ),

  private = list(
    state = c("pending", "fulfilled", "rejected")[1],
    id = NULL,
    value = NULL,
    on_fulfilled = list(),
    on_rejected = list(),
    progress_callback = NULL,
    cancel_callback = NULL,
    cancelled = FALSE,
    stack = NULL,

    resolve = function(value)
      def__resolve(self, private, value),
    reject = function(reason)
      def__reject(self, private, reason),
    progress = function(..., tick = NULL, total = NULL, ratio = NULL,
                        amount = NULL)
      def__progress(self, private, tick, total, ratio, amount, ...),

    make_error_object = function()
      def__make_error_object(self, private)
  )
)

def_init <- function(self, private, action, on_progress, on_cancel) {
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
  do.call(action, args)
  invisible(self)
}

def_get_value <- function(self, private) {
  if (private$state == "pending") {
    stop("Deferred value not resolved yet")
  } else if (private$state == "rejected") {
    stop(private$make_error_object())
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
  on_fulfilled <- if (!is.null(on_fulfilled)) as_function(on_fulfilled)
  on_rejected  <- if (!is.null(on_rejected))  as_function(on_rejected)
  def <- deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)

    handle <- function(func) {
      force(func)
      function(value) {
        get_default_event_loop()$add_next_tick(
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

  def
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
    list(message = reason %||% "Promise cancelled", call = NULL),
    class = c("async_cancelled", "error", "condition")
  )
  private$reject(cancel_cond)
}

def__resolve <- function(self, private, value) {
  if (private$state != "pending") stop("Deferred value already resolved")
  if (is_deferred(value)) {
    value$then(private$resolve, private$reject)
  } else {
    private$state <- "fulfilled"
    private$value <- value
    loop <- get_default_event_loop()
    for (f in private$on_fulfilled) f(value)
    private$on_fulfilled <- list()
  }
}

def__reject <- function(self, private, reason) {
  if (private$cancelled) return()
  if (private$state != "pending") stop("Deferred value already rejected")
  if (is_deferred(reason)) {
    reason$then(private$resolve, private$reject)
  } else {
    private$state <- "rejected"
    private$value <-
      if (is.character(reason)) simpleError(reason) else reason
    loop <- get_default_event_loop()
    if (inherits(reason, "async_cancelled") &&
        !is.null(private$cancel_callback)) {
      private$cancelled <- TRUE
      private$cancel_callback(conditionMessage(reason))
    }
    for (f in private$on_rejected) f(reason)
    private$on_rejected <- list()
  }
}

def__progress <- function(self, private, tick, total, ratio, amount, ...) {
  if (private$state != "pending") stop("Deferred value already resolved")
  if (is.null(private$progress_callback)) return()
  args <- list(tick = tick, total = total, ratio = ratio, amount = amount)
  has <- intersect(names(args), names(formals(private$progress_callback)))
  do.call(private$progress_callback, c(list(...), args[has]))
}

def__make_error_object <- function(self, private) {
  structure(
    list(
      message = private$value$message,
      call = private$value$stack
    ),
    class = unique(c(class(private$value), "async_deferred_rejected",
      "error", "condition"))
  )
}

#' Is object a deferred value?
#'
#' @param x object
#' @return Whether it is a deferred value.
#'
#' @export
#' @examples
#' is_deferred(1:10)
#' is_deferred(dx <- delay(1/100))
#' is_deferred(await(dx))

is_deferred <- function(x) {
  inherits(x, "deferred")
}

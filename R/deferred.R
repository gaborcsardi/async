
#' Deferred value
#'
#' @section Usage:
#' ```
#' dx <- deferred$new(action)
#'
#' dx$then(on_fulfilled)
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
#' `dx$then()` creates a deferred value whose resolution depends on the
#' `dx` deferred value. When `dx` is successfully resolved, the
#' `on_fulfilled` function is called, with the resolved value as an
#' argument.
#'
#' `dx$catch()` is a TODO
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
                          lazy = TRUE, parents = NULL, type = NULL)
      async_def_init(self, private, action, on_progress, on_cancel, lazy,
                     parents, type),
    then = function(on_fulfilled)
      def_then(self, private, on_fulfilled),
    catch = function(on_rejected)
      def_catch(self, private, on_rejected),
    finally = function(on_finally)
      def_finally(self, private, on_finally),
    cancel = function(reason = "Cancelled")
      def_cancel(self, private, reason),
    null = function()
      def_null(self, private)
  ),

  private = list(
    id = NULL,
    type = NULL,
    state = c("pending", "fulfilled", "rejected")[1],
    event_loop = NULL,
    value = NULL,
    children = list(),
    progress_callback = NULL,
    cancel_callback = NULL,
    cancelled = FALSE,
    dead_end = FALSE,
    parents = NULL,

    get_value = function()
      def__get_value(self, private),

    resolve = function(value)
      def__resolve(self, private, value),
    reject = function(reason)
      def__reject(self, private, reason),
    progress = function(data)
      def__progress(self, private, data),

    make_error_object = function(err)
      def__make_error_object(self, private, err),

    maybe_cancel_parents = function(reason)
      def__maybe_cancel_parents(self, private, reason),

    then_resolve = NULL,
    then_reject = NULL
  )
)

async_def_init <- function(self, private, action, on_progress,
                           on_cancel, lazy, parents, type) {

  ## TODO: handle errors that happen here, maybe.

  private$type <- type
  private$id <- get_id()
  private$event_loop <- get_default_event_loop()
  private$parents <- parents

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
  if (!is.na(sf_arg <- match("myself", action_args))) {
    args$myself <- self
  }
  if (!is.na(pr_arg <- match("progress", action_args))) {
    args$progress <- private$progress
  }

  ## We use isTRUE, because then we don't need to assert_flag(lazy),
  ## which could result an error, and it is not clear if that error
  ## should be reported synchronously or not

  if (isTRUE(lazy)) {
    private$event_loop$add_next_tick(
      function() do.call(action, args),
      function(err, res) if (!is.null(err)) private$reject(err))

  } else {
    call_with_callback(
      function() do.call(action, args),
      function(err, res) if (!is.null(err)) private$reject(err))
  }

  invisible(self)
}

def__get_value <- function(self, private) {
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

def_then <- function(self, private, on_fulfilled = NULL, on_rejected = NULL) {
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

  deferred$new(lazy = FALSE, parents = list(self),
               type = paste("then", private$id),
               function(resolve, reject, myself) {
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
      private$children <- c(private$children, list(myself))
      myprivate <- get_private(myself)
      myprivate$then_resolve <- on_fulfilled
      myprivate$then_reject <- on_rejected %||% stop

    } else if (private$state == "fulfilled") {
      handle(on_fulfilled)(private$value)

    } else if (private$state == "rejected") {
      handle(on_rejected %||% stop)(private$value)
    }
  })
}

def_catch <- function(self, private, on_rejected) {
  force(on_rejected)
  def_then(self, private, on_rejected = on_rejected)
}

def_finally <- function(self, private, on_finally) {
  force(on_finally)
  def_then(
    self,
    private,
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
  self$.__enclos_env__$private$dead_end <- TRUE
  invisible(self)
}

def__resolve <- function(self, private, value) {
  if (private$cancelled) return()
  if (private$state != "pending") stop("Deferred value already resolved")
  if (is_deferred(value)) {

    vpriv <- get_private(value)
    if (vpriv$state == "pending")  {
      vpriv$children <- c(vpriv$children, list(self))
      private$then_resolve <- identity
      private$then_reject <- stop
      private$parents <- c(private$parents, list(value))

    } else if (vpriv$state == "resolved") {
      private$resolve(vpriv$value)

    } else if (vpriv$state == "rejected") {
      private$reject(vpriv$value)
    }

  } else {
    if (!private$dead_end && !length(private$children)) {
      stop("Computation going nowhere...")
    }
    private$state <- "fulfilled"
    private$value <- value
    for (x in private$children) def__call_then("then_resolve", x, value)
    private$children <- list()
    private$maybe_cancel_parents(private$value)
    private$parents <- NULL
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
  err
}

def__reject <- function(self, private, reason) {
  if (private$cancelled) return()
  if (private$state != "pending") stop("Deferred value already rejected")
  if (is_deferred(reason)) {
    reason$then(private$resolve)$catch(private$reject)$null()
  } else {
    private$state <- "rejected"
    private$value <- private$make_error_object(reason)
    if (inherits(private$value, "async_cancelled")) {
      private$cancelled <- TRUE
    }
    if (!is.null(private$cancel_callback)) {
      private$cancel_callback(conditionMessage(private$value))
    }
    for (x in private$children) def__call_then("then_reject", x, private$value)
    private$children <- list()
    private$maybe_cancel_parents(private$value)
    private$parents <- NULL
  }
}

def__maybe_cancel_parents <- function(self, private, reason) {
  for (parent in private$parents) {
    if (is.null(parent)) next

    parent_priv <- get_private(parent)
    if (parent_priv$state != "pending") next

    chld <- parent_priv$children
    parent_priv$children <- chld[! vlapply(chld, identical, self)]
    if (!length(parent_priv$children)) parent$cancel(reason)
  }
}

def__call_then <- function(which, x, value)  {
  force(value)
  priv <- get_private(x)
  if (priv$state != "pending") return()

  cb <- priv[[which]]
  priv$event_loop$add_next_tick(
    make_then_function(cb, value),
    function(err, res) {
      if (is.null(err)) priv$resolve(res) else priv$reject(err)
    }
  )
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

#' Cancel async tasks
#'
#' Does nothing for deferred values that are already resolved.
#' Does nothing for objects that are not deferred values.
#'
#' @param ... Deferred values to cancel.
#' @param .list More deferred values to cancel.
#'
#' @export

async_cancel_pending <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  for (i in seq_along(defs)) {
    if (is_deferred(defs[[i]])) {
      defs[[i]]$null()
      defs[[i]]$cancel()
    }
  }
}

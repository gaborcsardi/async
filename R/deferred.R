
#' Deferred value
#'
#' @section Usage:
#' ```
#' dx <- deferred$new(action = NULL, on_progress = NULL, on_cancel = NULL,
#'          parents = NULL, parent_resolve = NULL, parent_reject = NULL,
#'          type = NULL)
#' dx$then(on_fulfilled)
#' dx$catch(...)
#' dx$finally(on_finally)
#' dx$cancel(reason = "Cancelled")
#' dx$share()
#' ```
#'
#' @section Arguments:
#' * `action`: Function to call when the deferred value starts running.
#'      it needs to have at least two arguments: `resolve` and `reject`,
#'      and the third `progress` argument is optional. See details below.
#' * `on_progress`: A function to call to report progress. See details
#'      below.
#' * `on_cancel`: A function to call when the deferred is cancelled. See
#'      details below.
#' * `parents`: A list of deferred values that will be the parents of the
#'      deferred value being created. If some of them are already owned,
#'      an error is thrown.
#' * `parent_resolve`: A function to call when a parent is resolved.
#'      See details below.
#' * `parent_reject`: A function to call when a parent throws an error.
#'      See details below.
#' * `type`: A label that can be used to indicate the type of the deferred
#'      value to create. This might be useful for debugging, but otherwise
#'      it is not used.
#' * `on_fulfilled`: Function to call when the parent deferred is resolved.
#'      Essentially this is the `parent_resolve` function of the `then()`
#'      deferred.
#' * `...` Error handlers, as in `tryCatch()`, see details below.
#' * `on_finally`: Function to call, after the deferred value is resolved
#'      or after it has thrown an error. It will be called without arguments.
#' * `reason` Error message or error object that will be used to cancel the
#'      deferred.
#'
#' @section Deferred values:
#'
#' Asynchronous computation is represented by deferred values.
#' A deferred value is an [R6](https://github.com/wch/R6) object.
#'
#' ```
#' deferred$new(action = NULL, on_progress = NULL, on_cancel = NULL,
#'    parents = NULL, parent_resolve = NULL, parent_reject = NULL,
#'    type = NULL)
#' ```
#'
#' Creates a new deferred value. `action` is a function that is called
#' once the deferred value is _started_ (i.e. _not_ when `dx` is created).
#' It must have one or two arguments: `resolve`, or `resolve` and `progress`
#' It should call `resolve` when it is done, with the final value of the
#' deferred as the argument. (See examples below.) If it has two arguments,
#' then the second one is a callback function for creating progress bars.
#' The deferred value may report its progress through this function.
#' See details in the _Progress bars_ section below.
#'
#' `action` is called when the evaluation of the deferred value is started.
#' Only deferred values that are needed to calculate the value of the
#' async phase, are evaluated. (See also _Lazy Evaluation_ below.)
#'
#' Note that `action` is optional, for some deferred values, no action is
#' takes when they are started. (These typically depend on their parent
#' nodes.)
#'
#' `on_cancel` is a function that is called without arguments when a
#' deferred value is cancelled. This includes explicit cancellation by
#' calling its `$cancel()` method, or auto-cancellation (see below).
#'
#' `parents` is a list of deferred values that need to be computed before
#' the current deferred value. When a parent deferred is resolved, the
#' `parent_resolve` function is called. When a parent referred throws an
#' error, the parent_reject` function is called.
#'
#' `parent_resolve` is a function with (up to) two arguments:
#' `value` and `resolve`. It will be called with the value of the
#' parent, the `resolve` callback of the deferred.
#' `parent_resolve` can resolve the deferred by calling the supplied `resolve`
#' callback, or it can keep waiting on other parents and/or external
#' computation. It may throw an error to fail the deferred.
#'
#' `parent_resolve` allows some shorthands as well:
#' * `NULL`: the deferred is resolved with the value of the parent.
#' * A function with no arguments: this function is called, and the deferred
#'   resolves to its return value.
#' * A function with one argument: this function is called with the value
#'   of the parent as the argument, and the deferred is resolved to its
#'   return value.
#' * A function with arguments `value` and `resolve`. This function is
#'   called with the value of the parent, and the resolve callback of the
#'   deferred.
#'
#' `parent_reject` is a function with (up to) two arguments:
#' `value`, `resolve`. It will be called with the error object
#' thrown by the parent.
#'
#' `parent_resolve` can resolve the deferred by calling the supplied
#' `resolve` callback, or it can keep waiting on other parents and/or
#' external computation. It may throw an error to fail the deferred. It may
#' also re-throw the error received from the parent, if it does not wish
#' to handle it.
#'
#' `parent_reject` also accepts some shorthands as well:
#' * `NULL`: the deferred throws the same error as the parent.
#' * A function with no arguments: this function is called, and the deferred
#'   resolves to its return value.
#' * A function with one argument: this function is called with the value
#'   of the parent as the argument, and the deferred is resolved to its
#'   return value.
#' * A function with arguments `value` and `resolve`. This function is
#'   called with the value of the parent, and the resolve callback of the
#'   deferred.

#' * A list of named error handlers, corresponding to the error handlers
#'   of `$catch()` (and `tryCatch()`). If these error handlers handle the
#'   parent's error, the deferred is resolved with the result of the
#'   handlers. Otherwise the deferred will be failed with the parent's
#'   error. The error handlers may also throw a new error.
#'
#' @section Error handling:
#'
#' The action function of the deferred, and also the `parent_resolve` and
#' `parent_reject` handlers may throw errors if the deferred cannot be
#' computed. Errors can be handled wit the `$catch()` member function:
#'
#' ```
#' dx$catch(...)
#' ```
#'
#' It takes the same named error handler arguments as `tryCatch()`.
#'
#' Technically, `$catch()` creates a new deferred value, and this new
#' deferred value is resolved to the result of the error handlers. Of the
#' handlers do not handle the error, then the new deferred will fail
#' with the same error.
#'
#' The `$finally()` method can be used to run create finalizer code that
#' runs when a deferred is resolved or when it fails. It can be used to
#' close database connections or other resources:
#'
#' ```
#' dx$finally(on_finally)
#' ```
#'
#' Technically, `$finally()` creates a new deferred, which will resolve
#' or fail the same way as the original one, but before doing that it will
#' call the `on_finally` function with no arguments.
#'
#' @section Builtin async functions:
#'
#' The async package comes with some basic async functions:
#' * [delay()] sets a timer and then resolves to `TRUE`.
#' * [async_constant()] resolves successfully to its argument.
#' * [http_get()] and [http_head()] make HTTP GET and HEAD requests.
#'
#' @section Combining async values:
#'
#' Async computation (just like ordinary sync computation) usually
#' consists of several steps that needs to be performed in the specified
#' order. The `$then()` method specifies that a step of computation needs
#' to be performed after the deferred value is known:
#'
#' ```
#' dx$then(on_fulfilled)
#' ```
#'
#' `on_fulfilled` is a function with zero or one formal arguments.
#' It will be called once the result of the deferred is known, with its
#' result. (The result is omitted if it has no arguments).
#'
#' `$then()` creates another deferred value, that will resolve to the
#' result of the `on_fulfilled` callback. Should this callback return
#' with a deferred value, then `$then()` the deferred value will be a
#' child of this newly creted deferred, and only resolve after that.
#'
#' See also [when_all()], [when_some()] and [when_any()], which can combine
#' multiple deferred values into one.
#'
#' You cannot call `$then()` (or [when_any()], [when_all()], etc. on the
#' same deferred value multiple times, unless it is a shared deferred
#' value. See _Ownership_ below.
#'
#' The [async_reflect()], [async_retry()], [async_sequence()],
#' [async_timeout()], [async_until()] and [async_whilst()] functions are
#' helpers for more complex async control flow.
#'
#' @section Ownership:
#'
#' async follows a strong ownership model. Each deferred value must be
#' owned by exactly one other deferred value  (unless they are shared, see
#' below).
#'
#' After a `dx2 <- dx$then()` call, the `dx` deferred is _owned_ by the
#' newly created deferred value. (The same applied to [when_any()], etc.)
#' This means that it is not possible to call `$then()` on the same
#' deferred value multiple times. The deferred value that is synchronized
#' by calling [synchronise()] on it, is owned by [synchronise()], see
#' _Synchronization_ below.
#'
#' The deferred values of an async phase form a directed graph, which we
#' call the async DAG (directed, acyclic graph). Usually (when no deferred
#' is shared, see below), this DAG is a rooted tree, the root of the tree
#' is the synchronised deferred, the final result of the async phase.
#'
#' @section Shared Deferred Values:
#'
#' In the rare cases when the strong ownership model is too restrictive,
#' a deferred value can be marked as _shared_:
#'
#' ```
#' dx$share()
#' ```
#'
#' This has the following implications:
#' * A shared deferred value can have multiple children (owners) in the
#'   async DAG.
#' * A shared deferred value is started after its first child is started.
#' * A shared deferred value is not auto-cancelled when all of its children
#'   are finished. (Because it might have more children in the future.)
#' * A shared deferred value is still auto-cancelled at the end of the
#'   event loop.
#'
#' Use shared deferred values sparingly, only when they are really needed,
#' as they forbid auto-cancellation, so deferred values will hold on to
#' resources longer, until the async phase is finished.
#'
#' @section Synchronization:
#'
#' async allows embedding asynchronous computation in synchronous code.
#' The execution of such a program has a sync phase and async phases. When the
#' program starts, it is in the sync phase. In the sync phase you cannot
#' create deferred values. (But you can still define (async) functions, that
#' will create deferred values when called.)
#'
#' To enter into an async phase, call [synchronise()] on an expression that
#' evaluates to a deferred value. The async phase will last until this
#' deferred value is computed or an error is thrown (and the error reaches
#' [synchronise()]).
#'
#' [synchronise()] creates an event loop, which manages the computation of
#' the deferred values in this particular async phase.
#'
#' Async phases can be embedded into each other. I.e. a program may call
#' [synchronise()] while in the async phase. The outer async phase's event
#' loop then stops until the inner async phase terminates. Deferred values
#' cannot be passed through a `synchronise()` barrier, to anoter (sync or
#' async phase). Should this happen, an error is reported on the first
#' operation on the leaked deferred value.
#'
#' In a typical application, a function is implemented asynchronously, and
#' then used synchronously by the interactive user, or another piece of
#' synchronous code, via [synchronise()] calls. The following example makes
#' three HTTP requests in parallel:
#'
#' ```
#' http_status3 <- function() {
#'   http_status <- function(url) {
#'     http_get(url)$then(function(response) response$status_code)
#'   }
#'   r1 <- http_status("https://eu.httpbin.org/status/403")
#'   r2 <- http_status("https://eu.httpbin.org/status/404")
#'   r3 <- http_status("https://eu.httpbin.org/status/200")
#'   when_all(r1, r2, r3)
#' }
#' synchronise(http_status3())
#' ```
#'
#' This async function can also be used asychronously, as a parent of
#' another deferred value, in an async phase.
#'
#' @section Lazy evaluation:
#'
#' async does not evaluate deferred values that are not part of the async
#' DAG of the async phase. These are clearly not needed to compute the
#' result of the async phase, so it would be a waste of resources working on
#' them. (It is also unclear how their errors should be handled.)
#'
#' In the following example, `d1` and `d2` are created, but they are not
#' part of the async DAG, so they are never evaluated.
#'
#' ```
#' do <- function() {
#'   d1 <- delay(1/100)$then(function() print("d1"))
#'   d2 <- d1$then(function() print("d2"))
#'   d3 <- delay(1/100)$then(function() print("d3"))
#'   d4 <- d3$then(function() print("d4"))
#'   d4
#' }
#' invisible(synchronise(do()))
#' ```
#'
#' @section Cancellation:
#'
#' The computation of a deferred can be cancelled when it is not needed
#' any more:
#'
#' ```
#' dx$cancel(reason = "Cancelled")
#' ```
#'
#' This will _fail_ the children of the deferred, unless they have been
#' completed already. It will also auto-cancel the parent DAG of the
#' deferred, unless they are shared deferreds, see the next Section.
#'
#' @section Auto-cancellation:
#'
#' In an async phase, it might happen that parts of the async DAG are not
#' needed for the final result any more. E.g. if a parent of a `when_all()`
#' node throws an error, then the other parents don't have to be computed.
#' In this case the event loop of the phase automatically cancels these
#' deferred values. Similarly, if a single parent of a [when_any()] node is
#' resolved, the other parents can be cancelled.
#'
#' In general, if a node of the async DAG is resolved, the whole directed
#' DAG, rooted at that node, can be cancelled (except for nodes that were
#' already resolved and nodes that have already failed).
#'
#' Auto-cancellation is very convenient, as you can be sure that resources
#' are free as soon as they are not needed. Some practical examples:
#'
#' * Making HTTP requests to many mirror web sites, to check their response
#'   time. As soon as the first reply is in, the rest of the HTTP requests
#'   are cancelled.
#' * In multi-process computation, as soon as one process fails, the rest are
#'   automatically cancelled. (Unless the failure is handled, of course.)
#'
#' async also has another type of cancellation, when [synchronise()] is
#' interrupted externally, either by the user or some system error. In this
#' case all processes and resources that were created in the event loop,
#' are cancelled and freed.
#'
#' Shared deferred values (see `$share()`) are not auto-cancelled when their
#' children are resolved or errored, but they are always cancelled at the
#' end of the async phase.
#'
#' @section Progress bars:
#'
#' A deferred value may report on its progress, if its action has a progress
#' callback. The progress callback is called with a list that describes
#' and event. We suggest that it always has an `event` entry, which is a
#' simple string. The rest of the list entries can be defined as needed,
#' but typically there will be a counter counting ticks, or a ratio
#' describing what part of the computation is already. See [http_get()]
#' for an async function that reports progress.
#'
#' @section Collections helper functions:
#'
#' async provides some utilities that make it easier to deal with
#' collections of deferred values:
#'
#' The current iterators:
#' * [async_map()] applies an async function to all elements of a vector or
#'   list (collection).
#' * [async_detect()] finds an element of a collection that passed an async
#'   truth test.
#' * [async_every()] checks if every element of a collection satisfies an
#'   async predicate. [async_some()] checks if any element does that.
#' * [async_filter()] keeps elements that pass an async truth test.
#'
#' @section Control flow helper functions:
#'
#' Control flow with deferred values can be challenging. Some helpers:
#' * [async_reflect()] creates an async function that always succeeds.
#'   This is useful if you want to apply it to a collection, and don't
#'   want to stop at the first error.
#' * [async_retry()] tries an async function a number of times.
#'   [async_retryable()] turns a regular function into a retryable one.
#' * [async_sequence()] chains two async functions. Calling their sequence
#'   is equivalent calling '$then()` on them, but [async_sequence()] is
#'   easier to use programmatically.
#' * [async_until()] and [async_whilst()] let you call an async function
#'   repeatedly, until or while a (syncronous) condition holds.
#' * [async_timeout()] runs an async function with a timeout.
#'
#' @section Examples:
#' Please see the README and the vignettes for examples.
#' @name deferred
NULL

#' @importFrom R6 R6Class
#' @export

deferred <- R6Class(
  "deferred",
  public = list(
    initialize = function(action = NULL, on_progress = NULL, on_cancel = NULL,
                          parents = NULL, parent_resolve = NULL,
                          parent_reject = NULL, type = NULL,
                          call = sys.call(-1))
      async_def_init(self, private, action, on_progress, on_cancel,
                     parents, parent_resolve, parent_reject, type, call),
    then = function(on_fulfilled)
      def_then(self, private, on_fulfilled),
    catch = function(...)
      def_catch(self, private, ...),
    finally = function(on_finally)
      def_finally(self, private, on_finally),
    cancel = function(reason = "Cancelled")
      def_cancel(self, private, reason),
    share = function() { private$shared <<- TRUE; invisible(self) }
  ),

  private = list(
    action = NULL,
    running = FALSE,
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
    parent_resolve = NULL,
    parent_reject = NULL,
    shared = FALSE,
    mycall = NULL,

    run_action = function()
      def__run_action(self, private),

    null = function()
      def__null(self, private),

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
    add_as_parent = function(child)
      def__add_as_parent(self, private, child),
    update_parent = function(old, new)
      def__update_parent(self, private, old, new),

    get_info = function()
      def__get_info(self, private)
  )
)

async_def_init <- function(self, private, action, on_progress,
                           on_cancel, parents, parent_resolve,
                           parent_reject, type, call) {

  private$type <- type
  private$id <- get_id()
  private$event_loop <- get_default_event_loop()
  private$parents <- parents
  private$action <- action
  private$mycall <- call

  "!DEBUG NEW `private$id` (`type`)"

  assert_that(is.null(on_progress) || is.function(on_progress))
  private$progress_callback <- on_progress
  assert_that(is.null(on_cancel) || is.function(on_cancel))
  private$cancel_callback <- on_cancel

  ## Handle the parents

  private$parent_resolve <- def__make_parent_resolve(parent_resolve)
  private$parent_reject <- def__make_parent_reject(parent_reject)

  for (prt in parents) {
    prt_pvt <- get_private(prt)
    prt_pvt$add_as_parent(self)
  }

  invisible(self)
}

def__run_action <- function(self, private) {
  if (private$running) return()
  action <- private$action
  private$running <- TRUE
  private$action <- NULL
  "!DEBUG ACTION `private$type` `private$id`"

  if (!is.null(action)) {
    if (!is.function(action)) {
      action <- as.function(action)
      formals(action) <- alist(resolve = NULL, progress = NULL)
    }
    assert_that(is_action_function(action))

    action_args <- names(formals(action))
    args <- list(private$resolve)
    if (!is.na(pr_arg <- match("progress", action_args))) {
      args$progress <- private$progress
    }

    private$event_loop$add_next_tick(
      function() {
        if (isTRUE(getOption("async_debug_steps", FALSE))) debug1(action)
        `__async_data__` <- list(private$id, "action", self, skip = 2L)
        do.call(action, args) },
      function(err, res) if (!is.null(err)) private$reject(err))
  }

  ## If some parents are done, we want them to notify us.
  ## We also start the ones that are not running yet
  for (prt in private$parents) {
    prt_priv <- get_private(prt)
    if (prt_priv$state != "pending") {
      def__call_then(
        if (prt_priv$state == "fulfilled") "parent_resolve" else "parent_reject",
        self, prt_priv$value)
    }
    prt_priv$run_action()
  }
}

def_then <- function(self, private, on_fulfilled = NULL,
                     on_rejected = NULL) {
  force(self)
  force(private)

  if (! identical(private$event_loop, get_default_event_loop())) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }

  if (!is_deferred(on_fulfilled)) {
    parent_resolve <- def__make_parent_resolve(on_fulfilled)
    parent_reject <- def__make_parent_reject(on_rejected)

    deferred$new(parents = list(self),
                 type = paste0("then-", private$id),
                 parent_resolve = parent_resolve,
                 parent_reject = parent_reject,
                 call = sys.call(-1))

  } else {
    private$add_as_parent(on_fulfilled)
    child_private <- get_private(on_fulfilled)
    child_private$parents <- c(child_private$parents, self)
    self
  }
}

def_catch <- function(self, private, ...) {
  def_then(self, private, on_rejected = list(...))
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
  invisible(self)
}

def__null <- function(self, private) {
  self$.__enclos_env__$private$dead_end <- TRUE
  invisible(self)
}

def__resolve <- function(self, private, value) {
  if (private$cancelled) return()
  if (private$state != "pending") return()

  if (is_deferred(value)) {
    private$parent_resolve <- def__make_parent_resolve(NULL)
    private$parent_reject <- def__make_parent_reject(NULL)

    # we need this in case self was shared and had multiple children
    val_pvt <- get_private(value)
    val_pvt$id <- private$id
    val_pvt$shared <- private$shared
    val_pvt$dead_end <- private$dead_end # This should not happen, though

    for (child in private$children) {
      ch_pvt <- get_private(child)
      ch_pvt$update_parent(self, value)
    }

    val_pvt$run_action()

  } else {
    if (!private$dead_end && !length(private$children) &&
        !private$shared) {
      ## This cannot happen currently
      "!DEBUG ??? DEAD END `private$id`"   # nocov
      warning("Computation going nowhere...")   # nocov
    }

    "!DEBUG +++ RESOLVE `private$id`"
    private$state <- "fulfilled"
    private$value <- value
    for (child in private$children) {
      def__call_then("parent_resolve", child, value)
    }
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

def__make_parent_resolve <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve) resolve(value)
  } else if (!is.function(fun)) {
    fun <- as.function(fun)
    function(value, resolve) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve) resolve(fun(value))
  } else if (identical(names(formals(fun)),
                       c("value", "resolve"))) {
    fun
  } else {
    stop("Invalid parent_resolve callback")
  }
}

def__make_parent_reject <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve) stop(value)
  } else if (is.list(fun)) {
    def__make_parent_reject_catch(fun)
  } else if (!is.function(fun)) {
    fun <- as.function(fun)
    function(value, resolve) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve) resolve(fun(value))
  } else if (identical(names(formals(fun)),
                       c("value", "resolve"))) {
    fun
  } else {
    stop("Invalid parent_reject callback")
  }
}

def__make_parent_reject_catch <- function(handlers) {
  handlers <- lapply(handlers, as.function)
  function(value, resolve) {
    ok <- FALSE
    ret <- tryCatch({
      quo <- as.call(c(list(quote(tryCatch), quote(stop(value))), handlers))
      ret <- eval(quo)
      ok <- TRUE
      ret
    }, error = function(x) x)

    if (ok) resolve(ret) else stop(ret)
  }
}

def__reject <- function(self, private, reason) {
  if (private$cancelled) return()
  if (private$state != "pending") return()

  ## 'reason' cannot be a deferred here

  "!DEBUG !!! REJECT `private$id`"
  private$state <- "rejected"
  private$value <- private$make_error_object(reason)
  if (inherits(private$value, "async_cancelled")) {
    private$cancelled <- TRUE
  }
  if (!is.null(private$cancel_callback)) {
    private$cancel_callback(conditionMessage(private$value))
  }
  for (child in private$children) {
    def__call_then("parent_reject", child, private$value)
  }
  private$maybe_cancel_parents(private$value)
  private$parents <- NULL
}

def__maybe_cancel_parents <- function(self, private, reason) {
  for (parent in private$parents) {
    if (is.null(parent)) next

    parent_priv <- get_private(parent)
    if (parent_priv$state != "pending") next
    if (parent_priv$shared) next
    parent$cancel(reason)
  }
}

def__call_then <- function(which, x, value)  {
  force(value);
  private <- get_private(x)
  if (!private$running) return()
  if (private$state != "pending") return()

  cb <- private[[which]]
  private$event_loop$add_next_tick(
    function() {
      if (isTRUE(getOption("async_debug_steps", FALSE))) {
        debug1(private[[which]])        # nocov
      }
      `__async_data__` <- list(private$id, "parent", x)
      private[[which]](value, private$resolve)
    },
    function(err, res) if (!is.null(err)) private$reject(err))
}

def__add_as_parent <- function(self, private, child) {
  "!DEBUG EDGE [`private$id` -> `get_private(child)$id`]"

  if (! identical(private$event_loop, get_private(child)$event_loop)) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }
  if (length(private$children) && !private$shared) {
    stop("Deferred value is already owned")
  }

  private$children <- c(private$children, child)

  if (get_private(child)$running) private$run_action()
  if (private$state == "pending") {
    ## Nothing to do

  } else if (private$state == "fulfilled") {
    def__call_then("parent_resolve", child, private$value)

  } else {
    def__call_then("parent_reject", child, private$value)
  }
}

def__update_parent <- function(self, private, old, new) {
  for (i in seq_along(private$parents)) {
    if (identical(private$parents[[i]], old)) {
      private$parents[[i]] <- new
    }
  }

  new_pvt <- get_private(new)
  new_pvt$add_as_parent(self)
}

def__progress <- function(self, private, data) {
  if (private$state != "pending") return()
  if (is.null(private$progress_callback)) return()
  private$progress_callback(data)
}

def__get_info <- function(self, private) {
  res <- data.frame(
    stringsAsFactors = FALSE,
    id = private$id,
    parents = I(list(viapply(private$parents, function(x) get_private(x)$id))),
    label = as.character(private$id),
    call = I(list(private$mycall)),
    children = I(list(viapply(private$children, function(x) get_private(x)$id))),
    type = private$type %||%  "unknown",
    running = private$running,
    state = private$state,
    cancelled = private$cancelled,
    shared = private$shared
  )
  src <- get_source_position(private$mycall)
  res$filename <- src$filename
  res$position <- src$position
  res$label <- paste0(
    res$id, " ",
    if (private$state == "fulfilled") paste0(cli::symbol$tick, " "),
    if (private$state == "rejected")  paste0(cli::symbol$cross, "  "),
    deparse(private$mycall)[1], " @ ",
    res$filename, ":", res$position)

  res
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

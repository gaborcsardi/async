
#' Deferred value
#'
#' @section Usage:
#' ```
#' TODO
#' ```
#'
#' @section Arguments:
#' TODO
#'
#' @section Details:
#' TODO
#'
#' @name deferred
NULL

#' @importFrom R6 R6Class
#' @export

deferred <- R6Class(
  "deferred",
  public = list(
    initialize = function(action = NULL, on_progress = NULL, on_cancel = NULL,
                          parents = NULL, parent_resolve = NULL,
                          parent_reject = NULL, type = NULL)
      async_def_init(self, private, action, on_progress, on_cancel,
                     parents, parent_resolve, parent_reject, type),
    then = function(on_fulfilled)
      def_then(self, private, on_fulfilled),
    catch = function(condition, ...)
      def_catch(self, private, condition, ...),
    finally = function(on_finally)
      def_finally(self, private, on_finally),
    cancel = function(reason = "Cancelled")
      def_cancel(self, private, reason),
    get_id = function() private$id
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
    parent_resolve = NULL,
    parent_reject = NULL,

    get_value = function()
      def__get_value(self, private),

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
      def__add_as_parent(self, private, child)
  )
)

async_def_init <- function(self, private, action, on_progress,
                           on_cancel, parents, parent_resolve,
                           parent_reject, type) {

  ## TODO: handle errors that happen here, maybe.

  private$type <- type
  private$id <- get_id()
  private$event_loop <- get_default_event_loop()
  private$parents <- parents

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

  ## Handle the action

  if (!is.null(action)) {
    if (!is.function(action)) {
      action <- as_function(action)
      formals(action) <- alist(resolve = NULL, reject = NULL,
                               progress = NULL)
    }
    assert_that(is_action_function(action))

    action_args <- names(formals(action))
    args <- list(private$resolve, private$reject)
    if (!is.na(pr_arg <- match("progress", action_args))) {
      args$progress <- private$progress
    }

    private$event_loop$add_next_tick(
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

  parent_resolve <- def__make_parent_resolve(on_fulfilled)
  parent_reject <- def__make_parent_reject(on_rejected)

  deferred$new(parents = list(self), type = paste0("then-", private$id),
               parent_resolve = parent_resolve,
               parent_reject = parent_reject)
}

def_catch <- function(self, private, condition, ...) {
  force(condition)
  def_then(self, private, on_rejected = condition)
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
    private$parents <- c(private$parents, list(value))
    get_private(value)$add_as_parent(self)

  } else {
    if (!private$dead_end && !length(private$children)) {
      warning("Computation going nowhere...")
    }

    "!DEBUG +++ RESOLVE `self$get_id()`"
    private$state <- "fulfilled"
    private$value <- value
    for (x in private$children) {
      def__call_then("parent_resolve", x, value, self$get_id())
    }
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

def__make_parent_resolve <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve, reject, id) resolve(value)
  } else if (!is.function(fun)) {
    fun <- as_function(fun)
    function(value, resolve, reject, id) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve, reject, id) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve, reject, id) resolve(fun(value))
  } else if (identical(names(formals(fun)),
                       c("value", "resolve", "reject"))) {
    function(value, resolve, reject, id) fun(value, resolve, reject)
  } else if (identical(names(formals(fun)),
                       c("value", "resolve", "reject", "id"))) {
    fun
  } else {
    stop("Invalid parent_resolve callback")
  }
}

def__make_parent_reject <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve, reject, id) stop(value)
  } else if (!is.function(fun)) {
    fun <- as_function(fun)
    function(value, resolve, reject, id) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve, reject, id) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve, reject, id) resolve(fun(value))
  } else if (identical(names(formals(fun)),
                       c("value", "resolve", "reject"))) {
    function(value, resolve, reject, id) fun(value, resolve, reject)
  } else if (identical(names(formals(fun)),
                       c("value", "resolve", "reject", "id"))) {
    fun
  } else {
    stop("Invalid parent_reject callback")
  }
}

def__reject <- function(self, private, reason) {
  if (private$cancelled) return()
  if (private$state != "pending") return()

  if (is_deferred(reason)) {
    private$parent_resolve <- def__make_parent_resolve(NULL)
    private$parent_reject <- def__make_parent_reject(NULL)
    private$parents <- c(private$parents, list(reason))
    get_private(reason)$add_as_parent(self)

  } else {
    "!DEBUG !!! REJECT `self$get_id()`"
    private$state <- "rejected"
    private$value <- private$make_error_object(reason)
    if (inherits(private$value, "async_cancelled")) {
      private$cancelled <- TRUE
    }
    if (!is.null(private$cancel_callback)) {
      private$cancel_callback(conditionMessage(private$value))
    }
    for (x in private$children) {
      def__call_then("parent_reject", x, private$value, self$get_id())
    }
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

def__call_then <- function(which, x, value, id)  {
  force(value); force(id)
  private <- get_private(x)
  if (private$state != "pending") return()

  cb <- private[[which]]
  private$event_loop$add_next_tick(
    function() private[[which]](value, private$resolve, private$reject, id),
    function(err, res) if (!is.null(err)) private$reject(err))
}

def__add_as_parent <- function(self, private, child) {
  "!DEBUG EDGE [`private$id` -> `child$get_id()`]"
  if (private$state == "pending") {
    private$children <- c(private$children, list(child))

  } else if (private$state == "fulfilled") {
    def__call_then("parent_resolve", child, private$value, self$get_id())

  } else {
    def__call_then("parent_reject", child, private$value, self$get_id())
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

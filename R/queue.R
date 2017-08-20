
#' Creates a queue object with the specified concurrency
#'
#' Items added to the queue are processed in parallel (up to the
#' concurrency limit). If all workers are in progress, the item is queued
#' until one becomes available. Once a worker completes work on an item,
#' that item's callback is called.
#'
#' @param worker_function Worker function. It will be called for each
#'   item inserted in the queue. It will be called with two arguments,
#'   the item, and a callback function. The callback function
#'   is to be called once the worker finished its job. The callback
#'   function has two arguments, the first one is used to report errors,
#'   it `NULL` if no errors happened. The second argument is the result
#'   of the worker's computation.
#' @param concurrency Number of parallel asynchronous workers allowed.
#' @return A [queue] object, that can be used to manage the workers.
#'
#' @seealso [queue] to manage the workers of a queue.
#' @export

make_queue <- function(worker_function, concurrency = 1) {
  assert_that(
    is_task(worker_function),
    is_count(concurrency),
    concurrency >= 1
  )
  queue$new(worker_function, concurrency)
}

#' Asynchronous queue
#'
#' Create the queue with [make_queue()].
#'
#' @section Usage:
#' ```
#' q$get_length()
#' q$is_started()
#' q$get_concurrency()
#' q$is_paused()
#' q$get_running()
#' q$is_idle()
#'
#' q$push(item, callback)
#' q$unshift(item, callback)
#' q$remove(test_function)
#' q$pause()
#' q$resume()
#'
#' q$call_if_saturated(callback)
#' q$call_if_unsaturated(callback)
#' q$call_if_empty(callback)
#' q$call_if_drained(callback)
#' q$call_if_error(callback)
#' ```
#'
#' @section Arguments:
#' \describe{
#'   \item{item}{An item to work on. It can be any object that the worker
#'     function of the queue can interpret.}
#'   \item{callback}{A callback function to call. See the details below
#'     on how it will be called for the various methods.}
#'   \item{test_function}{A test function that is called with the item
#'     as the single argument. It must return a logical value, that is
#'     `TRUE` is the item is to be removed from the queue.}
#' }
#'
#' @section Details:
#' `$get_length()` returns the number of items in the queue.
#'  Completed items are not included in the count.
#'
#' `$is_started()` returns `TRUE` if the queue has already started
#' running workers. This happens when the first item is added, if the
#' queue is not paused. Otherwise it happens at a `$resume()` call.
#'
#' `$get_concurrency()` returns the concurrency parameter of the queue.
#'
#' `$is_paused()` returns whether the queue is paused.
#'
#' `$get_running()` returns the number of items being processed. Only items
#' with running workers are included.
#'
#' `$is_idle()` returns whether the queue is idle.
#'
#' `$push(item, callback)` adds an item to the queue. If `callback` is not
#' `NULL`, then it must be a fucntion. This function will be called with
#' two arguments, the first one is the error object or message or `NULL` if
#' no error happened. The second one is the result of the worker on the
#' item.
#'
#' `$unshift(item, callback)` is like `$push()` but it inserts the item at
#' the beginning of the queue.
#'
#' `$remove(test_function)` removes all tasks from the queue that satisfy
#' a synchronous test function. The test function is called with the item
#' as the single argument, and must return `TRUE` if the item is to be
#' removed, and `FALSE` otherwise.
#'
#' `$pause()` stops the queue. No more workers are started until
#' `$resume()` is called on the queue.
#'
#' `$resume()` resumed a queue, that was stopped with `$pause()`.
#'
#' `$call_if_saturated(callback)` sets the callback function to call when
#' the queue is saturated. The callback is called without any arguments.
#' Setting it to `NULL` cancels these calls.
#'
#' `$call_if_unsaturated(callback)` sets the callback that is called when
#' the queue is unsaturated. The callback is called without any arguments.
#' Setting it to `NULL` cancels these calls.
#'
#' `$call_if_empty(callback)` sets the callback that is called when
#' the queue is empty, i.e. no items are waiting to be executed.
#' The callback is called without any arguments. Setting it to `NULL`
#' cancels these calls.
#'
#' `$call_if_drained(callback)` sets the callback that is called when there
#' are no running or waiting items in the queue. The callback is called
#' without any arguments. Setting it to `NULL` cancels these calls.
#'
#' `q$call_if_error(callback)` sets the callback to call when a worker
#' reports an error. The callback will be called with two arguments, the
#' error object or message and the item that errored.
#'
#' @name queue
NULL

#' @export

queue <- R6Class(
  "queue",

  public = list(
    initialize = function(worker_function, concurrency = 1)
      q_init(self, private, worker_function, concurrency),

    ## Simple queries
    get_length = function()      length(private$workers),
    is_started = function()      private$started,
    get_concurrency = function() private$concurrency,
    is_paused = function()       private$paused,
    get_running = function()     q_get_running(self, private),
    is_idle = function()         q_is_idle(self, private),
    get_id = function()          private$etask$id,

    ## Manipulation
    push = function(item, callback)
      q_push(self, private, item, callback),
    unshift = function(item, callback)
      q_unshift(self, private, item, callback),
    remove = function(test_function)
      q_remove(self, private, test_function),
    pause = function()
      q_pause(self, private),
    resume = function()
      q_resume(self, private),
    kill = function()
      q_kill(self, private),

    ## Callbacks
    call_if_saturated = function(callback)
      q_call_if_saturated(self, private, callback),
    call_if_unsaturated = function(callback)
      q_call_if_unsaturated(self, private, callback),
    call_if_empty = function(callback)
      q_call_if_empty(self, private, callback),
    call_if_drained = function(callback)
      q_call_if_drained(self, private, callback),
    call_if_error = function(callback)
      q_call_if_error(self, private, callback)
  ),

  private = list(

    schedule = function()
      q__schedule(self, private),
    call_callbacks = function()
      q__call_callbacks(self, private),

    worker_function = NULL,
    concurrency = NA,
    workers = list(),
    started = FALSE,
    paused = FALSE,
    etask = NULL,

    cb_saturated = NULL,
    cb_unsaturated = NULL,
    cb_empty = NULL,
    cb_drained = NULL,
    cb_error = NULL
  )
)

q_init <- function(self, private, worker_function, concurrency) {
  private$worker_function <- worker_function
  private$concurrency     <- concurrency
  private$etask           <- get_default_event_loop()$run_generic(NULL)
  self
}

q_get_running <- function(self, private) {
  sum(vlapply(private$workers, "[[", "running"))
}

q_is_idle <- function(self, private) {
  length(private$workers) == 0
}

q_push <- function(self, private, item, callback) {
  q_unshift(self, private, item, callback, length(private$workers))
}

q_unshift <- function(self, private, item, callback, after = 0) {
  assert_that(is_callback_or_null(callback))
  new_item <- list(
    item = item,
    callback = callback,
    running = FALSE
  )
  private$workers <- append(private$workers, list(new_item), after = after)
  private$schedule()
}

q_remove <- function(self, private, test_function) {
  assert_that(is.function(test_function))
  private$workers <- Filter(
    function(w) ! test_function(w$item),
    private$workers
  )
}

q_pause <- function(self, private) {
  private$paused <- TRUE
}

q_resume <- function(self, private) {
  private$paused <- FALSE
  private$schedule()
}

q_kill <- function(self, private) {
  private$etask$callback()
}

q_call_if_saturated <- function(self, private, callback) {
  assert_that(is.function(callback))
  old <- private$cb_saturated
  private$cb_saturated <- callback
  invisible(old)
}

q_call_if_unsaturated <- function(self, private, callback) {
  assert_that(is.function(callback))
  old <- private$cb_unsaturated
  private$cb_unsaturated <- callback
  invisible(old)
}

q_call_if_empty <- function(self, private, callback) {
  assert_that(is.function(callback))
  old <- private$cb_empty
  private$cb_empty <- callback
  invisible(old)
}

q_call_if_drained <- function(self, private, callback) {
  assert_that(is.function(callback))
  old <- private$cb_drained
  private$cb_drained <- callback
  invisible(old)
}

q_call_if_error <- function(self, private, callback) {
  assert_that(is.function(callback))
  old <- private$cb_error
  private$cb_error <- callback
  invisible(old)
}

q__schedule <- function(self, private) {
  if (private$paused) return()
  running <- vlapply(private$workers, "[[", "running")
  while (any(!running) && sum(running) < private$concurrency) {
    private$started <- TRUE
    local({
      to_start <- which(!running)[1]
      item <- private$workers[[to_start]]
      private$workers[[to_start]]$running <- TRUE
      async_call(
        private$worker_function, list(item$item), function(err, res) {
          private$workers[[to_start]] <- NULL
          if (!is.null(err) && !is.null(private$cb_error)) {
            private$cb_error(err, item$item)
          }
          if (!is.null(item$callback)) item$callback(err, res)
          private$schedule()
        }
      )
    })
    running <- vlapply(private$workers, "[[", "running")
  }

  private$call_callbacks()
}

q__call_callbacks <- function(self, private) {
  running <- vlapply(private$workers, "[[", "running")
  print(running)
  if (sum(!running) == 0 &&
      !is.null(cb <- private$cb_empty)) cb()
  if (length(private$workers) == 0 &&
      !is.null(cb <- private$cb_drained)) cb()
  if (sum(running) == private$concurrency &&
      !is.null(cb <- private$cb_saturated)) cb()
  if (sum(running) < private$concurrency &&
      !is.null(cb <- private$cb_unsaturated)) cb()
}

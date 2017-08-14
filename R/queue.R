
#' @export

make_queue <- function(worker_function, concurrency = 1) {
  assert_that(
    is_task(worker_function),
    is_count(concurrency),
    concurrency >= 1
  )
  queue$new(worker_function, concurrency)
}

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

    ## Manipulation
    push = function(task, callback)
      q_push(self, private, task, callback),
    unshift = function(task, callback)
      q_unshift(self, private, task, callback),
    remove = function(test_function)
      q_remove(self, private, test_function),
    pause = function()
      q_pause(self, private),
    resume = function()
      q_resume(self, private),

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
  invisible(self)
}

q_get_running <- function(self, private) {
  sum(vlapply(private$workers, "[[", "running"))
}

q_is_idle <- function(self, private) {
  length(private$workers) == 0
}

q_push <- function(self, private, task, callback) {
  q_unshift(self, private, task, callback, length(private$workers))
}

q_unshift <- function(self, private, task, callback, after = 0) {
  assert_that(is_callback_or_null(callback))
  new_item <- list(
    task = task,
    callback = callback,
    running = FALSE
  )
  private$workers <- append(private$workers, list(new_item), after = after)
  private$schedule()
}

q_remove <- function(self, private, test_function) {
  assert_that(is.function(test_function))
  private$workers <- Filter(
    function(w) ! test_function(w$task),
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
      task <- private$workers[[to_start]]
      private$workers[[to_start]]$running <- TRUE
      private$worker_function(
        task$task,
        function(err, res) {
          private$workers[[to_start]] <- NULL
          if (!is.null(err) && !is.null(private$cb_error)) {
            private$cb_error(err, task$task)
          }
          if (!is.null(task$callback)) task$callback(err, res)
        }
      )
    })
    running <- vlapply(private$workers, "[[", "running")
  }

  private$call_callbacks()
}

q__call_callbacks <- function(self, private) {
  running <- vlapply(private$workers, "[[", "running")
  if (sum(!running) == 0 && !is.null(cb <- private$cb_empty)) cb()
  if (all(running) && !is.null(cb <- private$cb_drained)) cb()
  if (sum(running) == private$concurrency &&
      !is.null(cb <- private$cb_saturated)) cb()
  if (sum(running) < private$concurrency &&
      !is.null(cb <- private$cb_unsaturated)) cb()
}

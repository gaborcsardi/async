
#' TODO:
#'
#' @export

async_queue <- function(run_limit = 1, input_limit = Inf) {

  assert_that(
    input_limit == Inf || (is_count(input_limit) && input_limit >= 0),
    run_limit == Inf || (is_count(run_limit) && run_limit >= 1)
  )

  done <- FALSE
  queue_resolve <- NULL
  waiting <- list()
  ready <- list()
  pushes <- list()
  pops <- list()
  running <- 0L

  # This is the promise for the queue itself. It will resolve when
  # all tasks are done, and `$done()` has been called.
  queue <- deferred$new(
    type = "async_queue",
    call = sys.call(),

    # When the queue is started, it is possible that it needs to resolve
    # immediately, as laziness means that `$done()` might be called before
    # the queue has started.
    action = function(resolve) {
      queue_resolve <<- resolve
      done <<- TRUE
    },

    # When a task finishes, we might need to
    # * notify an async_pop() promise, otherwise store the result;
    # * start another one;
    # * resolve.
    parent_resolve = function(value, resolve, id) {
      "!DEBUG Q task done `id`"
      print("parent resolve")
      running <<- running - 1L
      if (length(pops) > 0) {
        pop <- pops[[1]]
        pops <<- pops[-1]
        pop(value)
      } else {
        ready[[length(ready) + 1]] <<- value
      }
      maybe_queue()
      maybe_resolve()
    }
  )

  # We can queue a new task if we are under the limit and one is waiting.
  # We can resolve a push() if not too many are waiting and one is pending.
  maybe_queue <- function() {
    "!DEBUG Q task start attempt"
    if (running < run_limit && length(waiting) > 0) {
      "!DEBUG Q task start"
      waiting[[1]]$then(queue)
      waiting <<- waiting[-1]
      running <<- running + 1L
    }
    if (length(waiting) < input_limit && length(pushes) > 0) {
      "!DEBUG Q push allowed"
      push <- pushes[[1]]
      pushes <<- pushes[-1]
      push(TRUE)
    }
  }

  # We can resolve if `$done()` was called, no task is running, and
  # there is nothing in the queue. If there still pops waiting then
  # we'll resolve them to `quote(exhausted)`.
  maybe_resolve <- function() {
    "!DEBUG Q maybe resolve?"
    print("done?")
    if (done && length(ready) == 0 && running == 0 &&
        !is.null(queue_resolve)) {
      for (pop in pops) pop(quote(exhausted))
      print("queue done")
      queue_resolve(NULL)
    }
  }

  list(
    # nothing happens with the pushed promise, unless the promise returned
    # by async_push() is waited on.
    async_push = function(task) {
      "!DEBUG Q push"
      print("push")
      pushes; waiting; maybe_queue
      deferred$new(
        type = "async_queue_push",
        call = sys.call(),
        # When the promise returned by `async_push()` is waited on
        # we add the task to the waiting list, and maybe queue it.
        action = function(resolve) {
          print("push_action")
          "!DEBUG Q push action"
          pushes[[length(pushes) + 1]] <<- resolve
          waiting[[length(waiting) + 1L]] <<- task
          maybe_queue()
          maybe_resolve()
        }
      )
    },

    # Returns a promise for _some_ result from the queue
    async_pop = function() {
      print("pop")
      ready; pops; maybe_resolve
      deferred$new(
        type = "async_queue_pop",
        call = sys.call(),
        # If there is a result ready, we'll resolve with that.
        # Otherwise we add the pop to the list that waits for results,
        # and it will be called from the parent callback of the queue.
        action = function(resolve) {
          print("pop action")
          if (length(ready) > 0) {
            val <- ready[[1]]
            ready <<- ready[-1]
            resolve(val)
          } else {
            pops[[length(pops) + 1]] <<- resolve
          }
          maybe_queue()
          maybe_resolve()
        }
      )
    },

    when_done = function() {
      queue
    }
  )
}


#' TODO:
#'
#' @export

make_async_queue <- function(run_limit = 1, input_limit = Inf) {

  assert_that(
    input_limit == Inf || (is_count(input_limit) && input_limit >= 0),
    run_limit == Inf || (is_count(run_limit) && run_limit >= 1)
  )

  done <- FALSE
  queue_resolve <- NULL
  waiting <- list()
  ready <- list()
  pops <- list()
  running <- 0L

  queue <- deferred$new(
    type = "async_queue", call = sys.call(),
    parents = list(),
    action = function(resolve) queue_resolve <<- resolve,
    parent_resolve = function(value, resolve, id) {
      if (length(pops) > 0) {
        pop <- pops[[1]]
        pops <<- pops[-1]
        pop(value)
        maybe_resolve()
      } else {
        ready[[length(ready) + 1]] <<- value
      }
      running <<- running - 1L
      maybe_queue()
    }
  )

  maybe_queue <- function() {
    if (running < run_limit && length(waiting) > 0) {
      waiting[[1]]$then(queue)
      waiting <<- waiting[-1]
      running <<- running + 1L
    }
  }

  maybe_resolve <- function() {
    if (length(ready) == 0 && length(pops) == 0) {
      queue_resolve(NULL)
    }
  }

  list(
    push = function(task) {
      if (done) stop("queue is done already") # TODO exhausted
      waiting[[length(waiting) + 1L]] <<- task
      maybe_queue()
    },

    pop = function() {
      deferred$new(
        type = "async_queue_pop", call = sys.call(),
        parents = list(),
        action = function(resolve) {
          if (length(ready) > 0) {
            val <- ready[[1]]
            ready <<- ready[-1]
            resolve(val)
            maybe_resolve()
          } else {
            pops[[length(pops) + 1]] <<- resolve
          }
        }
      )
    },

    done = function() {
      done <<- TRUE
      invisible()
    },

    when_done = function() {
      queue
    }
  )
}

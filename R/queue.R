
#' TODO
#'
#' @export

async_queue <- function(task_generator, limit = 1) {
  assert_that(
    is.function(task_generator),
    limit == Inf || (is_count(limit) && limit >= 1)
  )

  done <- FALSE
  pops <- list()
  ready <- list()
  num_running <- 0L

  queue <- deferred$new(
    type = "async_queue",
    call = sys.call(),

    action = function(resolve) {
      maybe_queue()
      maybe_resolve(resolve)
    },

    parent_resolve = function(value, resolve, id) {
      num_running <<- num_running - 1L
      if (length(pops) > 0) {
        pop <- pops[[1]]
        pops <<- pops[-1]
        pop(value)
      } else {
        ready[[length(ready) + 1]] <<- value
      }
      maybe_queue()
      maybe_resolve(resolve)
    }
  )

  is_exhausted <- function(x) identical(x, quote(exhausted))

  maybe_queue <- function() {
    while (! done && num_running < limit) {
      task <- task_generator()
      if (is_exhausted(task)) {
        done <<- TRUE
      } else {
        task$then(queue)
        num_running <<- num_running + 1L
      }
    }
  }

  maybe_resolve <- function(resolve) {
    if (done && num_running == 0) {
      for (pop in pops) pop(quote(exhauster))
      resolve(NULL)
    }
  }

  list(
    async_pop = function() {
      deferred$new(
        type = "async_queue_pop",
        call = sys.call(),
        action = function(resolve) {
          if (done) {
            resolve(quote(exhausted))
          } else if (length(ready) > 0) {
            val <- ready[[1]]
            ready <<- ready[-1]
            resolve(val)
          } else {
            pops[[length(pops) + 1L]] <<- resolve
          }
        }
      )
    },

    when_done = function() {
      queue
    }
  )
}

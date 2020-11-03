
#' TODO:
#' * implement concurrency
#' * do we need to do anything about errors? Maybe not.
#'
#' @export

make_async_queue <- function(concurrency = 1) {
  concurrency

  done <- FALSE
  queue_resolve <- NULL
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
    }
  )

  maybe_resolve <- function() {
    if (length(ready) == 0 && length(pops) == 0) {
      queue_resolve(NULL)
    }
  }

  list(
    push = function(task) {
      if (done) stop("queue is done already")
      task$then(queue)
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

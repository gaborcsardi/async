
#' Run a list of functions in parallel
#'
#' Run a list of functions in parallel, without waiting until
#' the previous function has completed. If any of the functions pass an
#' error to its callback, the main callback is immediately called with the
#' value of the error. Once the tasks have completed, the results are
#' passed to the final callback as an array.
#'
#' Note: `parallel` is about kicking-off I/O tasks in parallel, not about
#' parallel execution of code. If your tasks do not use any timers or
#' perform any async I/O, they will actually be executed in series. Any
#' synchronous setup sections for each task will happen one after the
#' other.
#'
#' Hint: Use [reflect()] to continue the execution of other tasks when a
#' task fails.
#'
#' @param tasks A list (or collection) of async functions to run. Each
#'   async function can complete with any number of optional result values.
#' @param callback A callback to run once all the functions have
#'   completed successfully. This function gets a results list
#'   containing all the result arguments passed to the task
#'   callbacks. Invoked with `(err, results)`.
#' @return Task id.
#'
#' @family async control flow
#' @export
#' @examples
#' res <- NULL
#' await(parallel(
#'   list(
#'     function(callback) {
#'       http_get("https://eu.httpbin.org/get?q=foo",
#'                function(err, res) callback(err, rawToChar(res$content)))
#'     },
#'     function(callback) {
#'       http_get("https://eu.httpbin.org/get?q=bar",
#'                function(err, res) callback(err, rawToChar(res$content)))
#'     }
#'   ),
#'   function(err, result) { res <<- result }
#' ))
#' length(res)
#' cat(res[[1]])

parallel <- function(tasks, callback) {
  assert_that(
    is_task_list(tasks),
    is_callback(callback)
  )

  task <- get_default_event_loop()$run_generic(callback)

  l <- length(tasks)
  if (l == 0) return(task$callback(NULL, list()))

  result <- vector(mode = "list", length = l)
  lapply(seq_along(tasks), function(i) {
    tasks[[i]](callback = function(err, res) {
      if (!is.null(err)) return(task$callback(err, NULL))
      l <<- l - 1
      result[[i]] <<- res
      if (l == 0) task$callback(NULL, result)
    })
  })

  task$id
}

#' Run a list of functions in parallel, limiting the number of async tasks
#'
#' The same as parallel but runs a maximum of limit async operations at a
#' time.
#'
#' @param limit The maximum number of async operations at a time.
#' @inheritParams parallel
#'
#' @family async control flow
#' @export
#' @examples
#' error <- result <- NULL
#' await(parallel_limit(
#'   list(
#'     function(callback) set_timeout(1/100, function() callback(NULL, 1)),
#'     function(callback) set_timeout(2/100, function() callback(NULL, 2)),
#'     function(callback) set_timeout(3/100, function() callback(NULL, 3))
#'   ),
#'   limit = 2,
#'   function(err, res) { error <<- err ; result <<- res }
#' ))
#' error
#' result

parallel_limit <- function(tasks, limit, callback) {
  assert_that(
    is_task_list(tasks),
    is_numeric_scalar(limit),
    is_callback(callback)
  )

  l <- length(tasks)
  done <- 0
  result <- vector(mode = "list", length = l)

  task <- get_default_event_loop()$run_generic(callback)

  nextone <- 1
  mycallback <- function(err, res, i) {
    if (!is.null(err)) return(task$callback(err, NULL))
    done <<- done + 1
    result[[i]] <<- res
    if (done == l) return(task$callback(NULL, result))
    ## Of one has finished, then we can run another one
    if (nextone <= l) {
      i <- nextone
      nextone <<- nextone + 1
      tasks[[i]](callback = function(err, res) mycallback(err, res, i))
    }
  }

  for (ii in seq_len(min(l, limit))) {
    if (nextone > l) break
    local({
      i <- ii
      nextone <<- nextone + 1
      tasks[[i]](callback = function(err, res) mycallback(err, res, i))
    })
  }

  task$id
}

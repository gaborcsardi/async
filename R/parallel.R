
#' @export

parallel <- function(tasks, callback, limit = Inf) {
  assert_that(
    is_task_list(tasks),
    is_callback(callback),
    is_numeric_scalar(limit)
  )

  l <- length(tasks)
  if (l > 0 && limit != Inf) return(parallel_limit(tasks, callback, limit))

  task <- get_default_event_loop()$run_generic(callback)

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

parallel_limit <- function(tasks, callback, limit) {

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


#' @export

parallel <- function(tasks, callback, limit = Inf) {
  force(tasks) ; force(callback)

  l <- length(tasks)
  if (l == 0) return(callback(NULL, list()))

  if (limit != Inf) return(parallel_limit(tasks, callback, limit))

  result <- vector(mode = "list", length = l)
  lapply(seq_along(tasks), function(i) {
    tasks[[i]](function(err, res) {
      if (!is.null(err)) return(callback(err))
      l <<- l - 1
      result[[i]] <<- res
      if (l == 0) callback(NULL, result)
    })
  })
}

parallel_limit <- function(tasks, callback, limit) {

  l <- length(tasks)
  done <- 0
  result <- vector(mode = "list", length = l)

  ## We start the first 'limit' tasks manually,
  ## and the rest from the callback. We need to mark the manually
  ## started ones as running
  nextone <- 1
  mycallback <- function(err, res, i) {
    if (!is.null(err)) return(callback(err, NULL))
    done <<- done + 1
    result[[i]] <<- res
    if (done == l) return(callback(NULL, result))
    ## Of one has finished, then we can run another one
    if (nextone <= l) {
      i <- nextone
      nextone <<- nextone + 1
      tasks[[i]](function(err, res) mycallback(err, res, i))
    }
  }

  for (ii in seq_len(min(l, limit))) {
    if (nextone > l) break
    local({
      i <- ii
      nextone <<- nextone + 1
      tasks[[i]](function(err, res) mycallback(err, res, i))
    })
  }
}

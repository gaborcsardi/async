
#' @export

retry <- function(task, times, ...) {
  task <- async(task)
  force(times)

  deferred$new(function(resolve, reject) {

    xresolve <- function(value) resolve(value)
    xreject  <- function(reason) {
      times <<- times - 1
      if (times > 0) {
        task(...)$then(xresolve, xreject)
      } else {
        reject(reason)
      }
    }

    task(...)$then(xresolve, xreject)
  })
}

#' @export

retryable <- function(task, times) {
  task <- async(task)
  function(...) {
    retry(task, times, ...)
  }
}

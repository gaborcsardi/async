
#' @export

timeout <- function(task, timeout, ...) {
  task <- async(task)
  force(timeout)
  done <- FALSE

  deferred$new(function(resolve, reject) {
    task(...)$then(
      function(value) { if (!done) resolve(value); done <<- TRUE },
      function(reason) { if (!done) reject(reason); done <<- TRUE }
    )
    delay(timeout)$then(
      function(value) {
        if (!done) reject(make_error("Timed out", "async_timeout"))
        done <<- TRUE
      }
    )
  })
}

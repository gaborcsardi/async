
#' Repeatedly call task, while test returns true
#'
#' @param test Synchronous or asynchronous test function.
#' @param task Asynchronous function to call repeatedly.
#' @param ... Arguments to pass to `task`.
#' @return Deferred value, that is resolved when the iteration is done.
#'
#' @family async control flow
#' @export
#' @examples
#' ## Keep calling while result is bigger than 0.1
#' calls <- 0
#' number <- Inf
#' wait_for(async_whilst(
#'   function() number >= 0.1,
#'   function() {
#'     calls <<- calls + 1
#'     number <<- runif(1)
#'   }
#' ))
#' calls
#'

async_whilst <- function(test, task, ...) {
  force(test)
  task <- async(task)

  deferred$new(function(resolve, reject) {

    force(resolve)
    force(reject)

    xresolve <- function(value) {
      tryCatch(
        if (!wait_for(test())) {
          resolve(value)
        } else {
          task(...)$then(xresolve, xreject)
        },
        error = function(e) reject(e)
      )
    }
    xreject <- function(reason) reject(reason)

    if (wait_for(test())) task(...)$then(xresolve, xreject) else resolve(NULL)
  })
}


#' Repeatedly call task until it return `TRUE`
#'
#' @param test Synchronous test function.
#' @param task Asynchronous function to call repeatedly.
#' @param ... Arguments to pass to `task`.
#' @return Deferred value, that is resolved when the iteration is done.
#'
#' @family async control flow
#' @export
#' @examples
#' ## Keep calling until it "returns" a number less than < 0.1
#' calls <- 0
#' number <- Inf
#' synchronise(async_until(
#'   function() number < 0.1,
#'   function() {
#'     calls <<- calls + 1
#'     number <<- runif(1)
#'   }
#' ))
#' calls

async_until <- function(test, task, ...) {
  force(test)
  task <- async(task)

  deferred$new(function(resolve, reject) {

    force(resolve)
    force(reject)

    xresolve <- function(value) {
      tryCatch(
        if (test()) {
          resolve(value)
        } else {
          task(...)$then(xresolve)$catch(xreject)$null()
        },
        error = function(e) reject(e)
      )
    }
    xreject <- function(reason) reject(reason)

    task(...)$then(xresolve)$catch(xreject)$null()
  })
}

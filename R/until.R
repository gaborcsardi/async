
#' Repeatedly call task until it its test function returns `TRUE`
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

  self <- deferred$new(
    type = "async_until", call = sys.call(),
    parents = list(task(...)),
    parent_resolve = function(value, resolve) {
      if (test()) {
        resolve(value)
      } else {
        task(...)$then(self)
      }
    }
  )

  self
}

async_until <- mark_as_async(async_until)

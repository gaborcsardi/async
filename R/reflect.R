
#' Make an asynchronous function that always succeeds
#'
#' This is sometimes useful, if the function is applied to entries in
#' a vector or list.
#'
#' @param task Function to transform.
#' @return Async function returning a deferred value that is never
#'   rejected. Instead its value is a list with entries `error` and
#'   `result`. If the original deferred was resolved, then `error` is
#'   `NULL`. If the original deferred was rejected, then `result` is
#'   `NULL`.
#'
#' @family async control flow
#' @export
#' @examples
#'  badfun <- async(function() stop("oh no!"))
#'  safefun <- async_reflect(badfun)
#'  sync_wrap(when_all(safefun(), "good"))

async_reflect <- function(task) {
  task <- async(task)
  function(...) {
    task(...)$then(
      function(value)  list(error = NULL, result = value),
      function(reason) list(error = reason, result = NULL)
    )
  }
}

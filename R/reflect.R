
#' Safe wrapper of an async function
#'
#' Wraps the async function in another function that always completes with
#' a result object even when it errors.
#'
#' @param func Async function to wrap.
#' @return The wrapper function that never returns with an error.
#'   Instead the result object is a named list with entries `error` and
#'   `value`.
#'
#' @family async utilities
#' @export
#' @examples
#' badfun <- asyncify(function() stop("oh no!"))
#' safefun <- reflect(badfun)
#'
#' result <- NULL
#' wait_for(parallel(
#'   list(safefun, safefun, safefun),
#'   function(err, res) result <<- res
#' ))
#' result

reflect <- function(func) {
  force(func)
  function(..., callback) {
    cb <- force(callback)
    func(..., callback = function(err, res) {
      force(cb); force(err); force(res)
      cb(NULL, list(error = err, value = res))
    })
  }
}

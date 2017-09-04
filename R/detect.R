
#' Find the value of a match, asynchronously
#'
#' All predicates are running in parallel, and the returned match
#' is not guaranteed to be the first one.
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @export
#' @examples
#' dx <- async_detect(
#'   c("https://httpbin.org/status/404", "https://eu.httpbin.org",
#'     "https://httpbin.org"),
#'   async_sequence(http_head, function(x) x$status_code == 200)
#' )
#' await(dx)

async_detect <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  num_todo <- length(defs)
  done <- FALSE

  deferred$new(function(resolve, reject) {

    if (length(defs) == 0) return(resolve(NULL))

    lapply(seq_along(defs), function(i) {
      defs[[i]]$then(
        function(value) {
          if (!done && isTRUE(value)) {
            done <<- TRUE
            resolve(.x[[i]])
          } else {
            num_todo <<- num_todo - 1
            if (num_todo == 0) resolve(NULL)
          }
        },
        function(reason) reject(reason)
      )
    })
  })
}

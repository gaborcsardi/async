
#' Returns `TRUE` if at least one element in a collection satisfies an
#' async test
#'
#' If any iteratee call returns true, the main callback is
#' immediately called.
#'
#' @param coll	A collection to iterate over.
#' @param iteratee An async truth test to apply to each item in the
#'   collections in parallel. The iteratee should complete with a boolean
#'   result value. Invoked with `(item, callback)`.
#' @param callback A callback which is called as soon as any iteratee
#'   returns `TRUE`, or after all the iteratee functions have finished.
#'   Result will be either `TRUE` or `FALSE` depending on the values of the
#'   async tests. Invoked with `(err, result)`.
#' @return A task id, that can be waited on with [wait_for()].
#'
#' @family async iterators
#' @export
#' @examples
#' ## Check if one of these sites are up
#' wait_for(some(
#'   c("https://eu.httpbin.org", "https://httpbin.org"),
#'   sequence(http_head, asyncify(function(x) x$status_code)),
#'   function(err, res) print(res)
#' ))

some <- function(coll, iteratee, callback) {
  assert_that(
    is.vector(coll),
    is_task(iteratee),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(coll)
  if (l == 0) return(etask$callback(NULL, FALSE))

  lapply(seq_len(l), function(i) {
    async_call(iteratee, list(coll[[i]]), function(err, res) {
      if (!is.null(err)) return(etask$callback(err, NULL))
      if (res) return(etask$callback(NULL, TRUE))
      l <<- l - 1
      if (l == 0) etask$callback(NULL, FALSE)
    })
  })

  etask$id
}

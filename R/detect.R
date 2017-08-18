
#' Returns the first value in a collection that passes an async truth test
#'
#' The `iteratee` is applied in parallel, meaning the first `iteratee` to
#' return true will fire the `detect` callback with that result. That means
#' the result might not be the first item in the original `coll` (in terms
#' of order) that passes the test. If order within the original `coll` is
#' important, then look at [detect_series()].
#'
#' @param coll A collection to iterate over.
#' @param iteratee A truth test to apply to each item in `coll`.
#'   The `iteratee` must complete with a logical flag value as its result.
#'   Invoked with `(item, callback)`.
#' @param callback A callback which is called as soon as any iteratee
#'   returns true, or after all the `iteratee` functions have finished.
#'   Result will be the first item in the array that passes the truth test
#'   (iteratee) or `NULL` if none passed. Invoked with `(err, result)`.
#' @return Task id.
#'
#' @family async iterators
#' @export
#' @examples
#' ## Get an URL that works
#' id <- detect(
#'   c("https://eu.httpbin.org", "https://httpbin.org"),
#'   sequence(http_get, asyncify(function(x) x$status_code)),
#'   function(err, res) print(res)
#' )
#' await(id)

detect <- function(coll, iteratee, callback) {
  assert_that(
    is_collection(coll),
    is_iteratee(iteratee),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(coll)
  if (l == 0) return(etask$callback(NULL, NULL))

  lapply(seq_len(l), function(i) {
    async_call(iteratee, list(coll[[i]]), function(err, res) {
      if (!is.null(err)) return(etask$callback(err, NULL))
      if (res) return (etask$callback(NULL, coll[[i]]))
      l <<- l - 1
      if (l == 0) etask$callback(NULL, NULL)
    })
  })

  etask$id
}

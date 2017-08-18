
#' Returns `TRUE` if every element in collection satisfies an async test
#'
#' If any iteratee call returns `FALSE`, the main callback is immediately
#' called.
#'
#' @param coll A collection to iterate over.
#' @param iteratee An async truth test to apply to each item in the
#'   collection in parallel. The iteratee must complete with a logical
#'   scalar result value. Invoked with `(item, callback)`.
#' @param callback A callback which is called after all the iteratee
#'   functions have finished. Result will be either `TRUE` or `FALSE`
#'   depending on the values of the async tests. Invoked with
#'   `(err, result)`.
#' @return Task id.
#'
#' @family async iterators
#' @export
#' @examples
#' ## Check if all numbers are odd
#' result <- NULL
#' await(every(
#'   c(1,3,5,7,10,11),
#'   function(x, callback) callback(NULL, as.logical(x %% 2)),
#'   function(err, res) result <<- res
#' ))
#' result

every <- function(coll, iteratee, callback) {
  assert_that(
    is_collection(coll),
    is_task(iteratee),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(coll)
  if (l == 0) return(etask$callback(NULL, TRUE))

  lapply(seq_len(l), function(i) {
    async_call(iteratee, list(coll[[i]]), function(err, res) {
      if (!is.null(err)) return(etask$callback(err, NULL))
      if (!res) return(etask$callback(NULL, FALSE))
      l <<- l - 1
      if (l == 0) etask$callback(NULL, TRUE)
    })
  })

  etask$id
}

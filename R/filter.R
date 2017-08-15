
#' Returns a new array of all the values in a collection which pass an
#' async truth test
#'
#' This operation is performed in parallel, but the results array
#' will be in the same order as the original.
#'
#' @param coll A collection to iterate over. It needs to have an `[`
#'   operator.
#' @param iteratee A truth test to apply to each item in `coll`.
#'   The `iteratee` is passed a `callback(err, truthValue)`, which must be
#'   called with a logical argument once it has completed. Invoked with
#'   `(item, callback)`.
#' @param callback A callback which is called after all the iteratee
#'   functions have finished. Invoked with `(err, results)`.
#' @return Task id.
#'
#' @family async iterators
#' @export
#' @examples
#' ## Filter out non-working URLs
#' urls <- c("https://httpbin.org/get", "https://httpbin.org/status/404")
#' test_url <- function(url, callback) {
#'   http_head(url, function(err, res) {
#'     callback(NULL, is.null(err) && res$status_code == 200)
#'   })
#' }
#'
#' result <- NULL
#' await(filter(urls, test_url, function(err, res) result <<- res))
#' result

filter <- function(coll, iteratee, callback) {
  assert_that(
    is_collection(coll),
    is_task(iteratee),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(coll)
  if (l == 0) return(etask$callback(NULL, coll))

  keep <- logical(l)
  lapply(seq_len(l), function(i) {
    iteratee(coll[[i]], callback = function(err, res) {
      if (!is.null(err)) return(etask$callback(err))
      l <<- l - 1
      keep[i] <<- res
      if (l == 0) etask$callback(NULL, coll[keep])
    })
  })

  etask$id
}

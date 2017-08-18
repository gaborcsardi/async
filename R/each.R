
#' Applies an iteratee to each item in collection, in parallel
#'
#' The `iteratee` is called with an item from the list, and a callback for
#' when it has finished. If the `iteratee` passes an error to its callback,
#' the main `callback` (for the `each` function) is immediately called with
#' the error.
#'
#' Note, that since this function applies `iteratee` to each item in
#' parallel, there is no guarantee that the iteratee functions will
#' complete in order.
#'
#' @param coll A collection to iterate over.
#' @param iteratee An async function to apply to each item in `coll`.
#'   Invoked with `(item, callback)`. The array index is not passed to the
#'   iteratee. If  you need the index, use [each_of()].
#' @param callback A callback which is called when all iteratee functions
#'   have finished, or an error occurs. Invoked with `(err)`.
#' @return Task id.
#'
#' @family async iterators
#' @export
#' @examples
#' ## Check a list of URLs
#' urls <- c("https://httpbin.org/get", "https://httpbin.org/status/404")
#' check_url <- function(url, callback) {
#'   http_head(url, function(err, res) {
#'     if (!is.null(err)) {
#'       callback(err)
#'     } else if (res$status_code != 200) {
#'       callback(paste("HTTP error", res$status_code, res$url))
#'     } else {
#'       callback(NULL)
#'     }
#'   })
#' }
#'
#' error <- NULL
#' wait_for(each(urls, check_url, function(err) error <<- err))
#' error

each <- function(coll, iteratee, callback) {
  assert_that(
    is_collection(coll),
    is_iteratee(iteratee),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(coll)
  lapply(seq_len(l), function(i) {
    item <- coll[[i]]
    async_call(iteratee, list(item), function(err) {
      if (!is.null(err)) return(etask$callback(err))
      l <<- l -1
      if (l == 0) etask$callback(NULL)
    })
  })

  etask$id
}

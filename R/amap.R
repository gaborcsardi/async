
#' Produces a new collection of values by mapping each value in collection
#' through the iteratee function
#'
#' The `iteratee` is called with an item from `coll` and a callback for
#' when it has finished processing. Each of these callback takes 2
#' arguments: an error, and the transformed item from coll. If iteratee
#' passes an error to its callback, the main callback (for the map
#' function) is immediately called with the error.
#'
#' Note, that since this function applies the iteratee to each item in
#' parallel, there is no guarantee that the iteratee functions will
#' complete in order. However, the results array will be in the same order
#' as the original `coll`.
#'
#' @param coll A collection to iterate over.
#' @param iteratee An async function to apply to each item in `coll`. The
#'   iteratee should complete with the transformed item. Invoked with
#'   `(item, callback)`.
#' @param callback A callback which is called when all iteratee functions
#'   have finished, or an error occurs. The result is a list of the
#'   transformed items from the `coll`. Invoked with `(err, result)`.
#' @return A task id, that can be waited on with [wait_for()].
#'
#' @family async iterators
#' @export
#' @examples
#' error <- result <- NULL
#' id <- amap(
#'   seq(10, 100, by = 10) / 100,
#'   function(delay, callback) {
#'     set_timeout(delay, function() callback(NULL, "OK"))
#'   },
#'   function(err, res) { error <<- err; result <<- res }
#' )
#' error
#' result
#'
#' wait_for(id)
#' error
#' result

amap <- function(coll, iteratee, callback) {
  assert_that(
    is_collection(coll),
    is_task(iteratee),
    is_callback(callback)
  )

  l <- length(coll)
  result <- vector(mode = "list", length = l)

  etask <- get_default_event_loop()$run_generic(callback)

  lapply(seq_len(l), function(i) {
    async_call(iteratee, list(coll[[i]]), function(err, res) {
      if (!is.null(err)) return(etask$callback(err))
      l <<- l - 1
      result[[i]] <<- res
      if (l == 0) etask$callback(NULL, result)
    })
  })

  etask$id
}

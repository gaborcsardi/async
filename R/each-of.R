
#' Applies an iteratee to each item in collection, in parallel, with index
#'
#' Like each, except that it passes the index as the second argument to
#' the iteratee.
#'
#' @inheritParams each
#'
#' @export

each_of <- function(coll, iteratee, callback) {
  assert_that(
    is_collection(coll),
    is_iteratee(iteratee),
    is_callback(callback)
  )

  etask <- get_default_event_loop()$run_generic(callback)

  l <- length(coll)
  lapply(seq_len(l), function(i) {
    item <- coll[[i]]
    async_call(iteratee, list(item, i), function(err) {
      if (!is.null(err)) return(etask$callback(err))
      l <<- l -1
      if (l == 0) etask$callback(NULL)
    })
  })

  etask$id
}

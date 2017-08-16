
#' Concurrency-limited version of `detect`
#'
#' The same as [detect()], but the number of parallel asynchronous tasks
#' is limited.
#'
#' @param limit Maximum number of concurrent asynchronous tasks.
#' @return Task id.
#'
#' @inheritParams detect
#'
#' @family async iterators
#' @export
#' @examples
#' num <- 0
#' task <- function(x, callback) {
#'   num <<- num + 1
#'   print(paste("Running:", num))
#'   set_timeout(1/10, function() { num <<- num - 1; callback(NULL, FALSE) })
#' }
#'
#' result <- "not null"
#' await(detect_limit(1:5, task, 2, function(err, res) result <<- res))
#' result

detect_limit <- function(coll, iteratee, limit, callback) {
  assert_that(
    is_collection(coll),
    is_iteratee(iteratee),
    is_numeric_scalar(limit),
    is_callback(callback)
  )

  task <- get_default_event_loop()$run_generic(callback)

  l <- length(coll)
  if (l == 0) return(etask$callback(NULL, NULL))

  done <- 0
  nextone <- 1
  mycallback <- function(err, res, i) {
    if (!is.null(err)) return(task$callback(err, NULL))
    if (res) return(task$callback(NULL, coll[[i]]))
    done <<- done + 1
    if (done == l) return(task$callback(NULL, NULL))
    if (nextone <= l) {
      i <- nextone
      nextone <<- nextone + 1
      iteratee(
        coll[[i]],
        callback = function(err, res) mycallback(err, res, i)
      )
    }
  }

  for (ii in seq_len(min(l, limit))) {
    if (nextone > l) break
    local({
      i <- ii
      nextone <<- nextone + 1
      iteratee(
        coll[[i]],
        callback = function(err, res) mycallback(err, res, i)
      )
    })
  }

  task$id
}

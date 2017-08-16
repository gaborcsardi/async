
#' Serial version of `detect`
#'
#' The same as [detect()] but runs only a single async operation at a time.
#'
#' @inheritParams detect
#' @return Task id.
#'
#' @family async iterators
#' @export
#' @examples
#' nums <- NULL
#' result <- "not null"
#' await(detect_series(
#'   1:5,
#'   function(x, callback) { nums <<- c(nums, x) ; callback(NULL, FALSE) },
#'   function(err, res) result <<- res
#' ))
#' nums
#' result

detect_series <- function(coll, iteratee, callback) {
  detect_limit(coll, iteratee, limit = 1, callback)
}

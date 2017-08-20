
#' Perform a synchronous function call to an asynchronous function
#'
#' In case of an asynchronous error, `sync` throws the same error.
#'
#' @param func Asynchronous function to call.
#' @param ... Arguments to `func`.
#' @param .args More arguments to `func`. This argument is easier to use
#'   programmatically.
#' @return The result of the asynchronous function, i.e. the object that
#'   is passed to its callback. If no results are passed to the callback,
#'   then `NULL`.
#'
#' @export
#' @examples
#' result <- sync(
#'   parallel,
#'   list(
#'     asyncify(function() 1L),
#'     function(callback) callback(NULL, 2L),
#'     asyncify(function() 3L)
#'   )
#' )
#' result

sync <- function(func, ..., .args = list()) {
  assert_that(is_task(func), is.list(.args))
  error <- result <- NULL
  wait_for(async_call(func, c(list(...), .args), function(err, res) {
    error  <<- if (missing(err)) NULL else err
    result <<- if (missing(res)) NULL else res
  }))
  if (!is.null(error)) stop(error)
  result
}

#' Transform an asynchronous function into a synchronous one
#'
#' All parameters to the wrapper function will be passed to the
#' asynchronous function, `func`.
#'
#' @param func The function to transform.
#' @return The synchronous wrapper function to `func`.
#'
#' @family async utilities
#' @export
#' @examples
#' sync_parallel <- syncify(parallel)
#'
#' result <- sync_parallel(list(
#'   asyncify(function() 1L),
#'   function(callback) callback(NULL, 2L),
#'   asyncify(function() 3L)
#' ))
#' result

syncify <- function(func) {
  assert_that(is_task(func))
  function(...) {
    sync(func, ...)
  }
}

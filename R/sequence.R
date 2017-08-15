
#' Creates a function which is a composition of asynchronous functions
#'
#' Each function consumes the return value of the previous one.
#' Composing functions f(), g(), and h() would produce the result
#' of h(g(f())), only this version uses callbacks to obtain the return
#' values.
#'
#' @param ... Asynchronous functions to compose.
#' @param .list More asynchronous functions to compare. Appended to the
#'   previous ones. This interface allows easier programming.
#' @return Asynchronous function, the composition of all input functions.
#'
#' @family async control flow
#' @export
#' @examples
#' http_status <- sequence(
#'   http_get,
#'   asyncify(function(x) x$status_code)
#' )
#' await(http_status(
#'   "https://httpbin.org",
#'   callback = function(err, res) print(res)
#' ))

sequence <- function(..., .list = NULL) {
  funcs <- c(list(...), .list)
  assert_that(is_task_list(funcs))

  function(..., callback) {
    i <- 1
    force(callback)
    task <- get_default_event_loop()$run_generic(callback)
    mycallback <- function(err, ...) {
      if (!is.null(err)) return(task$callback(err, ...))
      i <<- i + 1
      if (i > length(funcs)) return(task$callback(NULL ,...))
      funcs[[i]](..., callback = mycallback)
    }
    funcs[[i]](..., callback = mycallback)
    task$id
  }
}

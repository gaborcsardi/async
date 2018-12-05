
#' Compose asynchronous functions
#'
#' This is equivalent to using the `$then()` method of a deferred, but
#' it is easier to use programmatically.
#'
#' @param ... Asynchronous functions to compose.
#' @param .list Mose asynchronous functions to compose.
#' @return Asynchronous function, the composition of all input functions.
#'   They are performed left to right, the ones in `.list` are the last
#'   ones.
#'
#' @family async control flow
#' @export
#' @examples
#' \donttest{
#' check_url <- async_sequence(
#'   http_head, function(x) identical(x$status_code, 200L))
#' synchronise(check_url("https://eu.httpbin.org/status/404"))
#' synchronise(check_url("https://eu.httpbin.org/status/200"))
#' }

async_sequence <- function(..., .list = NULL) {
  funcs <- c(list(...), .list)
  if (length(funcs) == 0) stop("Function list empty in `async_sequence`")

  function(...) {
    dx <- async(funcs[[1]])(...)
    for (i in seq_along(funcs)[-1]) dx <- dx$then(funcs[[i]])
    dx
  }
}

async_sequence <- mark_as_async(async_sequence)

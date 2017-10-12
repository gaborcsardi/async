
#' Compose asynchronous functions
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
#' check_url <- async_sequence(http_head, ~ identical(.$status_code, 200L))
#' wait_for(check_url("https://httpbin.org/status/404"))
#' wait_for(check_url("https://httpbin.org/status/200"))

async_sequence <- function(..., .list = NULL) {
  funcs <- c(list(...), .list)
  if (length(funcs) == 0) stop("Function list empty in `async_sequence`")

  function(...) {
    dx <- async(funcs[[1]])(...)
    for (i in seq_along(funcs)[-1]) dx <- dx$then(as_function(funcs[[i]]))
    dx
  }
}


## TODO: methods
## TODO: headers
## TODO: options
## TODO: can we save to file?

#' Asynchronous HTTP GET request
#'
#' Start an HTTP GET request in the background, and report its completion
#' via a deferred.
#'
#' @param url URL to connect to.
#' @return Task id that can be waited on with [wait_for()].
#'
#' @family asyncronous HTTP calls
#' @export
#' @importFrom curl new_handle
#' @examples
#' TODO

http_get <- function(url) {
  assert_that(is_string(url))
  handle <- new_handle(url = url)
  make_deferred_http(handle)
}

#' Asynchronous HTTP HEAD request
#'
#' @inheritParams http_get
#' @return Task id that can be waited on with [wait_for()].
#'
#' @family asyncronous HTTP calls
#' @export
#' @importFrom curl handle_setopt
#' @examples
#' TODO

http_head <- function(url) {
  assert_that(is_string(url))
  handle <- new_handle(url = url)
  handle_setopt(handle, customrequest = "HEAD", nobody = TRUE)
  make_deferred_http(handle)
}

make_deferred_http <- function(handle) {
  deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)
    get_default_event_loop()$run_http(handle, function(err, res) {
      if (is.null(err)) resolve(res) else reject(err)
    })
  })
}


## TODO: methods
## TODO: headers
## TODO: options
## TODO: can we save to file?

#' Asynchronous HTTP GET request
#'
#' Start an HTTP GET request in the background, and report its completion
#' by calling a callback function.
#'
#' @param url URL to connect to.
#' @param callback Callback function that will be called once the request
#'   is done, or an error happens. It will be called with two arguments.
#'   The first argument is not `NULL` on error and it contains an error
#'   object, or an error message from `curl`. The second argument contains
#'   the response from `curl`.
#' @return Task id that can be waited on with [wait_for()].
#'
#' @family asyncronous HTTP calls
#' @export
#' @importFrom curl new_handle
#' @examples
#' error <- result <- NULL
#' wait_for(http_get(
#'   "https://httpbin.org/get",
#'   function(err, res) { error <<- err ; result <<- res }
#' ))
#' error
#' cat(rawToChar(result$content))

http_get <- async(function(url, callback = NULL) {
  assert_that(is_string(url), is_callback_or_null(callback))
  handle <- new_handle(url = url)

  if (is.null(callback)) {
    make_deferred_http(handle)
  } else {
    get_default_event_loop()$run_http(handle, callback)
  }
})

#' Asynchronous HTTP HEAD request
#'
#' @inheritParams http_get
#' @return Task id that can be waited on with [wait_for()].
#'
#' @family asyncronous HTTP calls
#' @export
#' @importFrom curl handle_setopt
#' @examples
#' error <- result <- NULL
#' wait_for(http_head(
#'   "https://httpbin.org/get",
#'   function(err, res) { error <<- err ; result <<- res }
#' ))
#' error
#' curl::parse_headers(result$headers)

http_head <- async(function(url, callback = NULL) {
  assert_that(is_string(url), is_callback_or_null(callback))
  handle <- new_handle(url = url)
  handle_setopt(handle, customrequest = "HEAD", nobody = TRUE)

  if (is.null(callback)) {
    make_deferred_http(handle)
  } else {
    get_default_event_loop()$run_http(handle, callback)
  }
})

make_deferred_http <- function(handle) {
  id <- NULL
  def <- deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)
    id <<- get_default_event_loop()$run_http(handle, function(err, res) {
      if (is.null(err)) resolve(res) else reject(err)
    })
  })
  def$.__enclos_env__$private$set_id(id)
  def
}

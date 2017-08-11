
## TODO: methods
## TODO: headers
## TODO: options
## TODO: can we save to file?

#' @export
#' @importFrom curl new_handle

http_get <- function(url, callback) {
  handle <- new_handle(url = url)
  get_default_event_loop()$run_http(handle, callback)
}

#' @export
#' @importFrom curl handle_setopt

http_head <- function(url, callback) {
  handle <- new_handle(url = url)
  handle_setopt(handle, customrequest = "HEAD", nobody = TRUE)
  get_default_event_loop()$run_http(handle, callback)
}

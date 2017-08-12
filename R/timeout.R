
#' @export

set_timeout <- function(callback, delay) {
  get_default_event_loop()$run_set_timeout(delay, callback)
}

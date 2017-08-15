
#' @export

set_timeout <- function(delay, callback) {
  get_default_event_loop()$run_set_timeout(delay, callback)
}

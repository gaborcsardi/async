
#' Run a function after the specified time interval
#'
#' Since R is single-threaded, the callback might be executed (much) later
#' than the specified time period.
#'
#' @param delay Time interval in seconds, the amount of time to delay
#'   to delay the execution of the callback. It can be a fraction of a
#'   second.
#' @param callback The function to call after `delay` seconds. It will be
#'   called without arguments.
#' @return Task id, it can be waited on with [wait_for()].
#'
#' @export
#' @examples
#' TODO

delay <- function(delay, callback = NULL) {
  deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)
    get_default_event_loop()$run_delay(
      delay,
      function() resolve(TRUE)
    )
  })
}

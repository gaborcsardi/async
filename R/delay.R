
#' Run a function after the specified time interval
#'
#' Since R is single-threaded, the callback might be executed (much) later
#' than the specified time period.
#'
#' @param delay Time interval in seconds, the amount of time to delay
#'   to delay the execution of the callback. It can be a fraction of a
#'   second.
#' @return A deferred object.
#'
#' @export
#' @examples
#' ## Two HEAD requests with 1/2 sec delay between them
#' resp <- list()
#' dx <- http_head("https://httpbin.org?q=2")$
#'   then(function(value) resp[[1]] <<- value$status_code)$
#'   then(function(...) delay(1/2))$
#'   then(function(...) http_head("https://httpbin.org?q=2"))$
#'   then(function(value) resp[[2]] <<- value$status_code)
#' await(dx)
#' resp

delay <- function(delay) {
  assert_that(is_time_interval(delay))
  deferred$new(
    function(resolve, reject) {
      force(resolve)
      force(reject)
      get_default_event_loop()$add_delayed(
        delay,
        function() TRUE,
        function(err, res) resolve(res)
      )
    },
    longstack = cbind(c(0,0,0,0), c(3,0,0,0))
  )
}

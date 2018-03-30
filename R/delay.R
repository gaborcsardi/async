
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
#' afun <- async(function() {
#'   http_head("https://eu.httpbin.org?q=2")$
#'     then(function(value) resp[[1]] <<- value$status_code)$
#'     then(function(...) delay(1/2))$
#'     then(function(...) http_head("https://eu.httpbin.org?q=2"))$
#'     then(function(value) resp[[2]] <<- value$status_code)
#' })
#' synchronise(afun())
#' resp

delay <- function(delay) {
  force(delay)
  id <- NULL
  deferred$new(
    type = "delay",
    action = function(resolve, reject) {
      assert_that(is_time_interval(delay))
      force(resolve)
      force(reject)
      id <<- get_default_event_loop()$add_delayed(
        delay,
        function() TRUE,
        function(err, res) if (is.null(err)) resolve(res) else reject(err)
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

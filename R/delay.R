
#' Delay async computation for the specified time
#'
#' Since R is single-threaded, the deferred value might be resolved (much)
#' later than the specified time period.
#'
#' @param delay Time interval in seconds, the amount of time to delay
#'   to delay the execution. It can be a fraction of a second.
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
    action = function(resolve) {
      assert_that(is_time_interval(delay))
      force(resolve)
      id <<- get_default_event_loop()$add_delayed(
        delay,
        function() TRUE,
        function(err, res) resolve(TRUE)
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

delay <- mark_as_async(delay)

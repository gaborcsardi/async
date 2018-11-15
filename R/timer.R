
#' Repeated timer
#'
#' TODO
#'
#' @importFrom R6 R6Class
#' @include events.R
#' @export

async_timer <- R6Class(
  "async_timer",
  inherit = event_emitter,
  public = list(
    initialize = function(delay, callback)
      async_timer_init(self, private, super, delay, callback),
    cancel = function()
      async_timer_cancel(self, private)
  ),

  private = list(
    id = NULL
  )
)

async_timer_init <- function(self, private, super, delay, callback) {
  assert_that(
    is_time_interval(delay),
    is.function(callback) && length(formals(callback)) == 0)

  ## event emitter
  super$initialize()

  private$id <- get_default_event_loop()$add_delayed(
    delay,
    function() self$emit("timeout"),
    function(err, res) {
      if (!is.null(err)) self$emit("error", err)              # nocov
    },
    rep = TRUE)

  self$listen_on("timeout", callback)

  invisible(self)
}

async_timer_cancel  <- function(self, private) {
  self; private
  self$remove_all_listeners("timeout")
  get_default_event_loop()$cancel(private$id)
  invisible(self)
}


#' Generic Event Emitter
#'
#' TODO
#'
#' @export
#' @importFrom R6 R6Class

event_emitter <- R6Class(
  "event_emitter",
  public = list(
    initialize = function(async = TRUE)
      ee_init(self, private, async),

    listen_on = function(event, callback)
      ee_listen_on(self, private, event, callback),

    listen_off = function(event, callback)
      ee_listen_off(self, private, event, callback),

    listen_once = function(event, callback)
      ee_listen_once(self, private, event, callback),

    emit = function(event, ...)
      ee_emit(self, private, event, ...),

    get_event_names = function()
      ee_get_event_names(self, private),

    get_listener_count = function(event)
      ee_get_listener_count(self, private, event),

    remove_all_listeners = function(event)
      ee_remove_all_listeners(self, private, event)
  ),

  private = list(
    lsts = NULL,
    async = NULL,

    cleanup_events = function()
      ee__cleanup_events(self, private),
    error_callback = function(err, res)
      ee__error_callback(self, private, err, res)
  )
)

ee_init <- function(self, private, async) {
  assert_that(is_flag(async))
  private$lsts <- structure(list(), names = character())
  private$async <- async
  invisible(self)
}

ee_listen_on <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = FALSE)))
  invisible(self)
}

ee_listen_off <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  for (idx in seq_along(private$lsts[[event]])) {
    if (identical(private$lsts[[event]][[idx]]$cb, callback)) {
      private$lsts[[event]] <- private$lsts[[event]][-idx]
      break
    }
  }
  invisible(self)
}

ee_listen_once <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = TRUE)))
  invisible(self)
}

ee_emit <- function(self, private, event, ...) {
  assert_that(is_string(event))
  list(...)
  tocall <- private$lsts[[event]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[[event]] <- tocall[!once]

  ## a for loop is not good here, because it does not create
  ## a closure for lst
  lapply(tocall, function(lst) {
    lst
    if (private$async) {
      get_default_event_loop()$add_next_tick(
        function() lst$cb(...),
        private$error_callback)

    } else {
      call_with_callback(
        function() lst$cb(...),
        private$error_callback)
    }
  })

  invisible(self)
}

ee_get_event_names <- function(self, private) {
  private$cleanup_events()
  names(private$lsts)
}

ee_get_listener_count <- function(self, private, event) {
  assert_that(is_string(event))
  length(private$lsts[[event]])
}

ee_remove_all_listeners <- function(self, private, event) {
  assert_that(is_string(event))
  private$lsts[[event]] <- NULL
  invisible(self)
}

ee__cleanup_events <- function(self, private) {
  len <- viapply(private$lsts, length)
  private$lsts <- private$lsts[len > 0]
}

ee__error_callback <- function(self, private, err, res) {
  if (is.null(err)) return()
  tocall <- private$lsts[["error"]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[["error"]] <- tocall[!once]

  if (length(tocall)) {
    for (lst in tocall) lst$cb(err)
  } else {
    stop(err)
  }
}

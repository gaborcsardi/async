
## TODO: think about error handling

#' @importFrom R6 R6Class
#' @export

event_loop <- R6Class(
  public = list(
    initialize = function()
      el_init(self, private),
    await = function(ids)
      el_await(self, private, ids),
    await_all = function()
      el_await_all(self, private),
    await_any = function(ids)
      el_await_any(self, private, ids),
    run_http = function(handle, callback)
      el_run_http(self, private, handle, callback)
  ),

  private = list(

    finish_task = function(id, error, result)
      el__finish_task(self, private, id, error, result),
    poll = function()
      el__poll(self, private),
    create_task = function(callback, ...)
      el__create_task(self, private, callback, ...),
    ensure_pool = function(...)
      el__ensure_pool(self, private, ...),

    tasks = list(),
    pool = NULL,
    done = character()
  )
)

el_init <- function(self, private) {
  reg.finalizer(self, function(me) me$await_all(), onexit = TRUE)
  later::later(function() private$poll())
}

#' @importFrom curl multi_add

el_run_http <- function(self, private, handle, callback) {
  force(self) ; force(private) ; force(handle) ; force(callback)
  id <- private$create_task(callback, data = list(handle = handle))
  private$ensure_pool()
  multi_add(
    handle = handle,
    pool = private$pool,
    done = function(response) {
      private$finish_task(id, error = NULL, result = response)
    },
    fail = function(error) {
      private$finish_task(id, error = error, result = NULL)
    }
  )
  id
}

el_await <- function(self, private, ids) {
  while (any(ids %in% names(private$tasks))) private$poll()
}

el_await_all <- function(self, private) {
  while (length(private$tasks)) private$poll()
}

el_await_any <- function(self, private, ids) {
  while (all(ids %in% names(private$tasks))) private$poll()
}

el__finish_task <- function(self, private, id, error, result) {
  private$tasks[[id]]$error <- error
  private$tasks[[id]]$result <- result
  private$done <- c(private$done, id)
}

#' @importFrom curl multi_run

el__poll <- function(self, private) {
  if (is.null(private$pool)) return()

  if (length(private$done) == 0) {
    multi_run(timeout = Inf, poll = TRUE, pool = private$pool)
  }

  to_be_done <- private$done
  for (id in to_be_done) {
    if (id %in% names(private$tasks)) {
      task <- private$tasks[[id]]
      task$callback(task$error, task$result)
      private$tasks[[id]] <- NULL
      private$done <- setdiff(private$done, id)
    }
  }
}

#' @importFrom uuid UUIDgenerate

el__create_task <- function(self, private, callback, data, ...) {
  id <- UUIDgenerate()
  private$tasks[[id]] <- list(
    id = id,
    callback = callback,
    data = data,
    error = NULL,
    result = NULL
  )
  id
}

#' @importFrom curl new_pool

el__ensure_pool <- function(self, private, ...) {
  if (is.null(private$pool)) private$pool <- new_pool(...)
}

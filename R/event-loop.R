

## TODO: think about error handling

#' Event loop
#'
#' @section Usage:
#' ```
#' el <- event_loop$new()
#'
#' el$add_http(handle, callback)
#' el$add_delayed(delay, func, callback)
#' ```
#'
#' @section Arguments:
#' \describe{
#'   \item{handle}{A `curl` handle to use for the `HTTP` operation.}
#'   \item{callback}{Callback function to call when the asynchronous
#'      operation is done. See details below.}
#'   \item{delay}{Number of seconds to delay the execution of the callback.}
#'   \item{func}{TODO}
#' }
#'
#' @section Details:
#' `$add_http()` starts an asynchronous HTTP request, with the specified
#' `curl` handle. Once the request is done, and the response is available
#' (or an error happens), the callback is called with two arguments, the
#' error object or message (if any) and the `curl` response object.
#'
#' `$add_delayed()` starts a task with the specified delay.
#'
#' @section The default event loop:
#'
#' The `async` package creates a default event loop when it is loaded.
#' All asyncronous constructs use this event loop by default.
#'
#' @name event_loop
#' @keywords internal
NULL

#' @importFrom R6 R6Class

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function()
      el_init(self, private),

    add_http = function(handle, callback, file = NULL, progress = NULL,
                        deferred)
      el_add_http(self, private, handle, callback, file, progress, deferred),
    add_delayed = function(delay, func, callback, deferred)
      el_add_delayed(self, private, delay, func, callback, deferred),
    add_next_tick = function(func, callback, deferred)
      el_add_next_tick(self, private, func, callback, deferred),

    run = function(mode = c("default", "nowait", "once"))
      el_run(self, private, mode = match.arg(mode))
  ),

  private = list(
    create_task = function(callback, deferred, ...)
      el__create_task(self, private, callback, deferred, ...),
    ensure_pool = function(...)
      el__ensure_pool(self, private, ...),
    get_poll_timeout = function()
      el__get_poll_timeout(self, private),
    run_pending = function()
      el__run_pending(self, private),
    run_timers = function()
      el__run_timers(self, private),
    is_alive = function()
      el__is_alive(self, private),
    update_time = function()
      el__update_time(self, private),

    time = Sys.time(),
    stop_flag = FALSE,
    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL,
    next_ticks = character()
  )
)

el_init <- function(self, private) {
  ## TODO
  ## reg.finalizer(self, function(me) me$run("default"), onexit = TRUE)
  invisible(self)
}

#' @importFrom curl multi_add parse_headers_list handle_data

el_add_http <- function(self, private, handle, callback, progress, file,
                        deferred) {
  self; private; handle; callback; progress; outfile <- file
  num_bytes <- 0; total <- NULL
  id <- private$create_task(callback, data = list(handle = handle),
                            deferred = deferred)
  private$ensure_pool()
  if (!is.null(outfile) && file.exists(outfile)) unlink(outfile)
  multi_add(
    handle = handle,
    pool = private$pool,
    done = function(response) {
      if (!is.null(outfile)) {
        if (is.null(total)) {
          headers <- parse_headers_list(response$headers)
          tot <- as.numeric(headers$`content-length`)
          if (!is.null(tot) && length(tot) >= 1 && !is.na(tot[[1]])) {
            total <<- tot
          }
        } else {
          total <- NULL
        }
        progress(
          status_code = response$status_code,
          total = total,
          ratio = 1.0
        )
      }
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      task$callback(NULL, response)
    },
    data = if (!is.null(outfile)) {
      function(bytes) {
        ## R runs out of connections very quickly, especially because they
        ## are not removed until a gc(). However, calling gc() is
        ## expensive, so we only do it if we have to. This is a temporary
        ## solution until we can use our own connections, that are not
        ## so limited in their numbers.
        con <- tryCatch(
          file(outfile, open = "ab"),
          error = function(e) { gc(); file(outfile, open = "ab") }
        )
        writeBin(bytes, con)
        close(con)
        if (is.null(total)) {
          headers <- parse_headers_list(handle_data(handle)$headers)
          tot <- as.numeric(headers$`content-length`)
          if (!is.null(tot) && length(tot) >= 1 && !is.na(tot[[1]])) {
            total <<- tot
          }
        } else {
          total <- NULL
        }
        num_bytes <<- num_bytes + length(bytes)
        progress(
          total = total,
          amount = length(bytes),
          ratio = if (is.null(total)) NULL else num_bytes / total
        )
      }
    },
    fail = function(error) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      error <- make_error(message = error)
      error$call <- record_stack()
      class(error) <- unique(c("async_rejected", "async_http_error",
                               class(error)))
      task$callback(error, NULL)
    }
  )
  id
}

el_add_delayed <- function(self, private, delay, func, callback, deferred) {
  force(self); force(private); force(delay); force(func); force(callback)
  id <- private$create_task(
    callback,
    data = list(delay = delay, func = func),
    deferred = deferred
  )
  private$timers[id] <- Sys.time() + as.difftime(delay, units = "secs")
  id
}

el_add_next_tick <- function(self, private, func, callback, deferred) {
  force(self) ; force(private) ; force(callback)
  id <- private$create_task(callback, data = list(func = func),
                            deferred = deferred)
  private$next_ticks <- c(private$next_ticks, id)
}

#' @importFrom curl multi_run

el_run <- function(self, private, mode) {

  ## This is closely modeled after the libuv event loop, on purpose,
  ## because some time we might switch to that.
  alive <- private$is_alive()
  if (! alive) private$update_time()

  while (alive && ! private$stop_flag) {
    private$update_time()
    private$run_timers()
    ran_pending <- private$run_pending()
    ## private$run_idle()
    ## private$run_prepare()

    timeout <- 0
    if (mode == "once" && !ran_pending || mode == "default") {
      timeout <- private$get_poll_timeout()
    }
    multi_run(timeout = timeout, poll = TRUE, pool = private$pool)

    ## private$run_check()
    ## private$run_closing_handles()

    if (mode == "once") {
      private$update_time()
      private$run_timers()
    }

    alive <- private$is_alive()
    if (mode == "once" || mode == "nowait") break
  }

  private$stop_flag <- FALSE

  alive
}

el__run_pending <- function(self, private) {
  next_ticks <- private$next_ticks
  private$next_ticks <- character()
  for (id in next_ticks) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    call_with_callback(task$data$func, task$callback, task$deferred)
  }

  length(next_ticks) > 0
}

#' @importFrom uuid UUIDgenerate

el__create_task <- function(self, private, callback, data, deferred, ...) {
  id <- UUIDgenerate()
  private$tasks[[id]] <- list(
    id = id,
    callback = callback,
    deferred = deferred,
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

el__get_poll_timeout <- function(self, private) {
  max(0, min(Inf, private$timers - private$time))
}

el__run_timers <- function(self, private) {
  expired <- names(private$timers)[private$timers <= private$time]
  expired <- expired[order(private$timers[expired])]
  for (id in expired) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    private$timers <- private$timers[setdiff(names(private$timers), id)]
    call_with_callback(task$data$func, task$callback, task$deferred)
  }
}

el__is_alive <- function(self, private) {
  length(private$tasks) > 0 ||
    length(private$timers) > 0 ||
    length(private$next_ticks) > 0
}

el__update_time <- function(self, private) {
  private$time <- Sys.time()
}

#' Call `func` and then call `callback` with the result
#'
#' `callback` will be called with two arguments, the first one will the
#' error object if `func()` threw an error, or `NULL` otherwise. The second
#' argument is `NULL` on error, and the result of `func()` otherwise.
#'
#' @param func Function to call.
#' @param callback Callback to call with the result of `func()`,
#'   or the error thrown.
#'
#' @keywords internal

call_with_callback <- function(func, callback, deferred) {
  error <- NULL
  tryCatch(
    withCallingHandlers(
      result <- async_stack_run(deferred, func()),
      error = function(e) {
        e$call <- record_stack(); error <<- e;
        handler <- getOption("async.error")
        if (is.function(handler)) handler()
      }
    ),
    error = identity
  )
  if (is.null(error)) {
    callback(NULL, result)
  } else {
    callback(error, NULL)
  }
}

async_stack_run <- function(deferred, expr) { deferred; expr }

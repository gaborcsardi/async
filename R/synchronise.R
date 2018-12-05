
#' Synchronously wrap asynchronous code
#'
#' Evaluate an expression in an async phase. It creates an event loop,
#' then evaluates the supplied expression. If its result is a deferred
#' value, it keeps running the event loop, until the deferred value is
#' resolved, and returns its resolved value.
#'
#' If an error is not handled in the async phase, `synchronise()` will
#' re-throw that error.
#'
#' `synchronise()` cancels all async processes on interrupt or extrenal
#' error.
#'
#' @param expr Async function call expression. If it does not evaluate
#' to a deferred value, then it is just returned.
#'
#' @export
#' @examples
#' \donttest{
#' http_status <- function(url, ...) {
#'   http_get(url, ...)$
#'     then(function(x) x$status_code)
#' }
#'
#' synchronise(http_status("https://eu.httpbin.org/status/418"))
#' }

synchronise <- function(expr) {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)

  ## Mark this frame as a synchronization point, for debugging
  `__async_synchronise_frame__` <- TRUE

  res <- expr

  if (!is_deferred(res)) return(res)

  priv <- get_private(res)
  if (! identical(priv$event_loop, new_el)) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }

  priv$null()
  priv$run_action()

  if (isTRUE(getOption("async_debug"))) start_browser()
  while (priv$state == "pending") new_el$run("once")

  if (priv$state == "fulfilled") priv$value else stop(priv$value)
}

start_browser <- function() {
  async_debug_shortcuts()
  on.exit(async_debug_remove_shortcuts(), add = TRUE)
  cat("This is a standard `browser()` call, but you can also use the\n")
  cat("following extra commands:\n")
  cat("- .an / async_next(): next event loop iteration.\n")
  cat("- .as / async_step(): next event loop, debug next action or parent callback.\n")
  cat("- .asb / async_step_back(): stop debugging of callbacks.\n")
  cat("- .al / async_list(): deferred values in the current async phase.\n")
  cat("- .at / async_tree(): DAG of the deferred values.\n")
  cat("- .aw / async_where(): print call stack, mark async callback.\n")
  cat("- async_wait_for(): run until deferred is resolved.\n")
  cat("- async_debug(): debug action and/or parent callbacks of deferred.\n")
  cat("\n")
  browser(skipCalls = 1)
}

#' Run event loop to completion
#'
#' Creates a new event loop, evaluates `expr` in it, and then runs the
#' event loop to completion. It stops when the event loop does not have
#' any tasks.
#'
#' The expression typically creates event loop tasks. It should not create
#' deferred values, though, because those will never be evaluated.
#'
#' Unhandled errors propagate to the `run_event_loop()` call, which fails.
#'
#' In case of an (unhandled) error, all event loop tasks will be cancelled.
#'
#' @param expr Expression to run after creating a new event loop.
#' @return `NULL`, always. If the event loop is to return some value,
#' you can use lexical scoping, see the example below.
#'
#' @export
#' @examples
#' counter <- 0L
#' do <- function() {
#'   callback <- function() {
#'     counter <<- counter + 1L
#'     if (runif(1) < 1/10) t$cancel()
#'   }
#'   t <- async_timer$new(1/1000, callback)
#' }
#' run_event_loop(do())
#' counter

run_event_loop <- function(expr) {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)

  ## Mark this frame as a synchronization point, for debugging
  `__async_synchronise_frame__` <- TRUE

  expr
  new_el$run()

  invisible()
}

distill_error <- function(err) {
  if (is.null(err$aframe)) return(err)
  err$aframe <- list(
    frame = err$aframe$frame,
    deferred = err$aframe$data[[1]],
    type = err$aframe$data[[2]],
    call = get_private(err$aframe$data[[3]])$mycall
  )
  err
}

# nocov start
#' @export

print.async_rejected <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

# nocov end

#' @export

format.async_rejected <- function(x, ...) {
  x <- distill_error(x)
  src <- get_source_position(x$aframe$call)
  paste0(
    "<async error: ", x$message, "\n",
    " in *", x$aframe$type, "* callback of `",
    expr_name(x$aframe$call %||% ""),
    "` at ", src$filename, ":", src$position, ">"
  )
}

#' @export

summary.async_rejected <- function(object, ...) {
  x <- distill_error(object)
  fmt_out <- format(object, ...)
  stack <- async_where(calls = x$calls, parents = x$parents,
                       frm = list(x$aframe))
  stack_out <- format(stack)
  structure(
    paste0(fmt_out, "\n\n", stack_out),
    class = "async_rejected_summary")
}

# nocov start

#' @export

print.async_rejected_summary <- function(x, ...) {
  cat(x)
  invisible(x)
}

# nocov end

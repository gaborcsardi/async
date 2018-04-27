
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
#' http_status <- function(url, ...) {
#'   http_get(url, ...)$
#'     then(function(x) x$status_code)
#' }
#'
#' synchronise(http_status("https://httpbin.org/status/418"))

synchronise <- function(expr) {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)

  ## Mark this frame, and a synchronization point, for debugging
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

  if (isTRUE(getOption("async_debug"))) browser()
  while (priv$state == "pending") new_el$run("once")

  if (priv$state == "fulfilled") priv$value else stop(priv$value)
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

#' @export

print.async_rejected <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

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
  fmt_out <- format(x, ...)
  stack <- async_where(calls = x$calls, parents = x$parents,
                       frm = list(x$aframe))
  stack_out <- format(stack)
  structure(
    paste0(fmt_out, "\n\n", stack_out),
    class = "async_rejected_summary")
}

#' @export

print.async_rejected_summary <- function(x, ...) {
  cat(x)
  invisible(x)
}

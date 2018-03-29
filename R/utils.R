
vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

viapply <- function(X, FUN, ..., FUN.VALUE = integer(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

get_value_x <- function(x) {
  if (is_deferred(x)) get_private(x)$get_value() else x
}

make_error <- function(message, class = "simpleError", call = NULL) {
  class <- c(class, "error", "condition")
  structure(
    list(message = as.character(message), call = call),
    class = class
  )
}

num_args <- function(fun) {
  length(formals(fun))
}

get_private <- function(x) {
  x$.__enclos_env__$private
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

call_with_callback <- function(func, callback) {
  recerror <- NULL
  result <- NULL
  tryCatch(
    withCallingHandlers(
      result <- func(),
      error = function(e) {
        recerror <<- e;
        handler <- getOption("async.error")
        if (is.function(handler)) handler()
      }
    ),
    error = identity
  )
  callback(recerror, result)
}

get_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

lapply_args <- function(X, FUN, ..., .args = list()) {
  do.call("lapply", c(list(X = X, FUN = FUN), list(...), .args))
}

`push<-` <- function(l, value) {
  if (is.list(l)) {
    c(l, list(value))
  } else {
    c(l, value)
  }
}

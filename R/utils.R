
vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

vcapply <- function(X, FUN, ..., FUN.VALUE = character(1)) {
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

sort_by_name <- function(x) {
  x[order(names(x))]
}

get_cond_message <- function(x) {
  if (is.character(x)) paste(x, collapse = "\n") else conditionMessage(x)
}

find_in_stack <- function(stack, elem) {
  for (i in rev(seq_along(stack))) {
    stacki <- stack[[i]]
    attributes(stacki) <- NULL
    if (identical(stacki, elem)) return(i)
  }
  NULL
}

find_call_in_stack <- function(stack, elem) {
  for (i in rev(seq_along(stack))) {
    stacki_call <- stack[[i]][[1]]
    if (identical(stacki_call, elem)) return(i)
  }
  NULL
}

find_calls_in_stack <- function(stack, elem) {
  vlapply(stack, function(x) identical(x[[1]], elem))
}

null_rownames <- function(x) {
  rownames(x) <- NULL
  x
}

drop_nulls <- function(x) {
  x[! vlapply(x, is.null)]
}

unique_names <- function(x) {
  x[unique(names(x))]
}

has_utf8 <- function() {
  ## TODO: this is a hack, we need to export `has_utf8()` from `cli`
  cli::get_spinner()$name == "dots"
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

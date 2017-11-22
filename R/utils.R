
vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

vcapply <- function(X, FUN, ..., FUN.VALUE = character(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

viapply <- function(X, FUN, ..., FUN.VALUE = integer(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

get_state_x <- function(x) {
  if (is_deferred(x)) x$get_state() else "not-deferred"
}

get_state_check_x <- function(x, event_loop) {
  if (is_deferred(x)) {
    if (!identical(x$get_event_loop(), event_loop)) {
      err <- make_error(
        "Cannot await() across synchronization barrier",
        class = "async_synchronization_barrier_error")
      stop(err)
    }
    x$get_state()
  } else {
    "not-deferred"
  }
}

get_value_x <- function(x) {
  if (is_deferred(x)) x$get_value() else x
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

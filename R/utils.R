
vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

vcapply <- function(X, FUN, ..., FUN.VALUE = character(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

get_state_x <- function(x) {
  if (is_deferred(x)) x$get_state() else "not-deferred"
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

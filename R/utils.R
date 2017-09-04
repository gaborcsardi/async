
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

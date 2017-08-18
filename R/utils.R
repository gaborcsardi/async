
vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

find_callback_arg <- function(fun) {
  if (length(cb <- tail(names(formals(fun)), 1))) {
    cb
  } else {
    stop("Function `fun` has no arguments, it is not an async function")
  }
}

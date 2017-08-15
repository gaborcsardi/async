
#' @importFrom assertthat assert_that on_failure<-
NULL

## TODO: we don't check now, because all we need is a `[[` and a `length`
## method.

is_collection <- function(x) {
  TRUE
}

on_failure(is_collection) <- function(call, env) {
  paste0(deparse(call$x), " is not a vector or list")
}

is_task <- function(x) {
  is.function(x)
}

on_failure(is_task) <- function(call, env) {
  paste0(deparse(call$x), " is not an async function (see ?async)")
}

is_iteratee <- function(x) {
  is.function(x)
}

on_failure(is_iteratee) <- function(call, env) {
  paste0(deparse(call$x), " is not an async iteratee (see ?async)")
}

is_callback <- function(x) {
  is.function(x) && length(formals(x)) >= 1
}

on_failure(is_callback) <- function(call, env) {
  paste0(deparse(call$x), " is not a callback function (see ?async)")
}

is_callback_or_null <- function(x) {
  is.function(x) && length(formals(x)) >= 1
}

on_failure(is_callback_or_null) <- function(call, env) {
  paste0(
    deparse(call$x),
    " is not a callback function or NULL (see ?async)")
}

is_task_list <- function(x) {
  is.list(x) && all(vlapply(x, is_task))
}

on_failure(is_task_list) <- function(call, env) {
  paste0(deparse(call$x), " is not a list of tasks (see ?async)")
}

is_numeric_scalar <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_numeric_scalar) <- function(call, env) {
  paste0(deparse(call$x), " is not a numeric scalar")
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

## Note that this allows Inf!

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && round(x) == x && x >= 0
}

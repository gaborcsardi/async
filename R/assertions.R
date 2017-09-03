
#' @importFrom assertthat assert_that on_failure<-
NULL

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_action_function <- function(x) {
  is.function(x) && length(formals(x)) == 2
}

on_failure(is_action_function) <- function(call, env) {
  paste0(deparse(call$x), " is not a function with two arguments")
}

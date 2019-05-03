
#' @importFrom assertthat assert_that on_failure<-
NULL

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length 1 logical)")
}

is_action_function <- function(x) {
  is.function(x) && length(formals(x)) %in% 1:2
}

on_failure(is_action_function) <- function(call, env) {
  paste0(deparse(call$x), " is not a function with two arguments")
}

is_time_interval <- function(x) {
  inherits(x, "difftime") ||
    (is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0)
}

on_failure(is_time_interval) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid time interval")
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x
}

on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (non-negative integer)")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " must be a flag (length 1 logical)")
}


#' Make an async function call
#'
#' We need to do some extra work, compared to a regular function call,
#' because we need to work out the name of the callback argument, which is
#' the last argument of the function.
#'
#' This is because the function might take variable number of arguments via
#' `...` and then we have to name the callback argument. Or, some argument
#' (probably with default values) are not included in the call, but we
#' still want to make sure that the callback is passed to the last
#' argument.
#'
#' @param fun Function to call, a function object.
#' @param args Other arguments to pass to the function.
#' @param callback Function to pass as the callback (last) argument of
#'   `fun`.
#' @return The return value of the called `fun`.
#'
#' @keywords internal

async_call <- function(fun, args, callback) {
  cb_arg <- find_callback_arg(fun)
  args[[cb_arg]] <- callback
  do.call(fun, args)
}

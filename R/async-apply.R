
#' Creates an asynchronous function with some arguments already applied.
#'
#' Useful as a shorthand when combined with other control flow
#' functions. Any arguments passed to the returned function are added to
#' the arguments originally passed to `async_apply`.
#'
#' @param func The function you want to eventually apply all arguments
#'   to.
#' @param ... The arguments to apply to `func`.
#' @return The partially applied function.
#'
#' @family async utilities
#' @export
#' @examples
#' f <- asyncify(function(a, b, c) a + b + c)
#' error <- result <- NULL
#' wait_for(parallel(
#'   list(
#'     async_apply(f, 1, 2, 3),
#'     async_apply(f, 4, 5, 6),
#'     async_apply(f, 7, 8, 9)
#'   ),
#'   function(err, res) { error <<- err; result <<- res }
#' ))
#' result

async_apply <- function(func, ...) {
  assert_that(is_task(func))
  args <- list(...)
  function(..., callback) {
    async_call(func, c(args, list(...)), callback)
  }
}

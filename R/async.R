
#' Create an async function
#'
#' Create an async function, that returns a deferred value, from a
#' regular function. If `fun` is already an async function, then it does
#' nothing, just returns it.
#'
#' The result function will have the same arguments, with the same default
#' values, and the same environment as the original input function.
#'
#' @param fun Original function.
#' @return Async version of the original function.
#'
#' @export
#' @examples
#' f <- function(x) 42
#' af <- async(f)
#' is_async(f)
#' is_async(af)
#' f()
#' dx <- af()
#' dx
#' await(dx)

async <- function(fun) {
  fun <- as_function(fun)
  if (is_async(fun)) return(fun)

  async_fun <- fun
  body(async_fun) <- expr({
    tryCatch(
      {
        r <- evalq({ !!! body(fun) })
        if (is_deferred(r)) {
          r
        } else {
          deferred$new(function(resolve, reject) resolve(r))
        }
      },
      error = function(e) deferred$new(function(resolve, reject) reject(e))
    )
  })

  attr(async_fun, "async") <- list(TRUE)

  async_fun
}

#' Checks if a function is async
#'
#' If `fun` is not a function, an error is thrown.
#'
#' Currently, it checks for the `async` attribute, which is set by
#' [async()].
#'
#' @param fun Function.
#' @return Logical scalar, whether `fun` is async.
#'
#' @export
#' @examples
#' f <- function(x) 42
#' af <- async(f)
#' is_async(f)
#' is_async(af)
#' f()
#' dx <- af()
#' dx
#' await(dx)

is_async <- function(fun) {
  assert_that(is.function(fun))
  is.list(a <- attr(fun, "async")) && identical(a[[1]], TRUE)
}

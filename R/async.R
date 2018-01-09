
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
#' synchronise(dx <- af())
#' dx

async <- function(fun) {
  fun <- as_function(fun)
  if (is_async(fun)) return(fun)

  async_fun <- fun
  body(async_fun) <- expr({
    mget(ls(environment(), all.names = TRUE), environment())
    (!! deferred)$new(
      function(resolve, reject) {
        force(resolve) ; force(reject)
        (!! get_default_event_loop)()$add_next_tick(
          function() {
            evalq(
              { !!! body(fun) },
              envir = parent.env(parent.env(environment()))
            )
          },
          function(err, res) {
            if (is.null(err)) resolve(res) else reject(err)
          },
          deferred = environment(resolve)$self
        )
      }
    )
  })

  attr(async_fun, "async")$async <- TRUE

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
#' synchronise(dx <- af())
#' dx

is_async <- function(fun) {
  assert_that(is.function(fun))
  is.list(a <- attr(fun, "async")) && identical(a$async, TRUE)
}

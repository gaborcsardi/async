
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
    fun2 <- function() {
      evalq({
        el <- get_default_event_loop()
        el$add_context(pure = FALSE)
        withCallingHandlers(
          { ret <- { !!! body(fun) }; el$drop_context(); ret },
          condition = function(c) el$drop_context()
        )
      }, envir = parent.env(environment()))
    }
    d1 <- deferred$new(function(resolve, reject) resolve(NULL), type = "async")
    d2 <- deferred$new(
      parent_resolve = function(value, resolve, reject) resolve(fun2()),
      parents = list(d1))
    d2
  })

  attr(async_fun, "async")$async <- TRUE

  async_fun
}

#' @export

pure_async <- function(fun) {
  fun <- as_function(fun)
  if (is_async(fun)) return(fun)

  async_fun <- fun
  body(async_fun) <- expr({
    mget(ls(environment(), all.names = TRUE), environment())
    fun2 <- function() {
      evalq({
        el <- get_default_event_loop()
        el$add_context(pure = TRUE)
        withCallingHandlers({
          ret <- { !!! body(fun) }
          el$drop_context(unlock = ret)
          ret
        }, condition = function(c) el$drop_context())
      }, envir = parent.env(environment()))
    }

    ## This is ridiculously difficult...
    d1 <- deferred$new(
      action = function(resolve, reject) resolve(NULL),
      type = "tick")
    d2 <- deferred$new(
      type = "pure_async",
      parent_resolve = function(value, resolve, reject) {
        res <- fun2()
        if (is_deferred(res)) {
          priv <- get_private(d2)
          priv$parent_resolve <- def__make_parent_resolve(NULL)
          priv$parents <- list(res)
          get_private(res)$add_as_parent(d2)
          res$lock()
        } else {
          resolve(res)
        }
      },
      parents = list(d1))

    d1$lock()
    d2
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

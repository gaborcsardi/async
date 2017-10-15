
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
#' wait_for(dx)

async <- function(fn) {

  force(fn)
  factory <- expr_interp(function() {
    args <- list(!!!fn_fmls_syms(fn))
    new_async_generator(body(fn), environment(fn), args)
  })
  formals(factory) <- fn_fmls(fn)

  factory
}

new_async_generator <- function(body, env, args = list()) {
  parts <- flowery:::machine_parts(body, pause_sym = quote(await))
  if (is_null(parts)) {
    stop("Functions without `await` are not supported yet")
  }
  env <- flowery:::gen_env(env, args)

  deferred$new(function(resolve, reject) {
    resolve; reject

    gen <- expr_interp(function(`_result` = NULL) {
      nm <- gen_arg_name(env_get(env, "_pause_sym"))
      if (!is.null(nm)) env_set(env, nm, `_result`, create = TRUE)

      evalq(env, expr = {
        while (TRUE) {
          !!flowery:::machine_switch(parts)
        }
      })
    })

    continue <- function(value) {
      ret <- gen(value)
      if (is_deferred(ret)) {
        ret$then(continue)

      } else {
        resolve(ret)
      }
    }

    continue()
  })
}

gen_arg_name <- function(expr) {
  if (!length(expr) || expr[[1]] != quote(`<-`)) return(NULL)
  if (! is.symbol(expr[[2]])) {
    stop("await assignment must be have a symbol on the left hand side")
  }
  as.character(expr[[2]])
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
#' wait_for(dx)

is_async <- function(fun) {
  assert_that(is.function(fun))
  is.list(a <- attr(fun, "async")) && identical(a[[1]], TRUE)
}

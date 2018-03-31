
#' Make a minimal deferred that resolves to the specified value
#'
#' This is sometimes useful to start a deferred chain.
#'
#' Note that the evaluation of `value` is forced when the deferred value
#' is created.
#'
#' @param value The value to resolve to.
#' @return A deferred value.
#'
#' @export
#' @examples
#' afun <- async(function() {
#'   async_constant(1/100)$
#'     then(function(x) delay(x))$
#'     then(function(x) print(x))
#' })
#' synchronise(afun())

async_constant <- function(value = NULL) {
  force(value)
  deferred$new(function(resolve, reject) resolve(value), type = "constant")
}

async_constant <- mark_as_async(async_constant)

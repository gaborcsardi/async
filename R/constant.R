
#' Make a minimal deferred that resolves to the specified value
#'
#' This is sometimes useful to start a deferred chain.
#'
#' @param value The value to resolve to.
#' @return A deferred value.
#'
#' @export
#' @examples
#' dx <- async_constant(1/100)$
#'   then(~ delay(.))$
#'   then(~ print(.))
#' await(dx)

async_constant <- function(value = NULL) {
  force(value)
  deferred$new(function(resolve, reject) resolve(value))
}

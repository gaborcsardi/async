
#' Apply an asynchronous function to each element of a vector
#'
#' @param A list or atomic vector.
#' @param .f Asynchronous function to apply.
#' @param ... Additional arguments to `.f`.
#' @return Deferred value that is resolved after all deferred values
#'   from the application of `.f` are resolved.
#' 
#' @family async iterators
#' @export
#' @examples
#' dx <- async_map(
#'   seq(10, 100, by = 10) / 100,
#'   function(wait) delay(wait)$then(~ "OK")
#' )
#' dx
#' await(dx)

async_map <- function(.x, .f, ...) {
  defs <- lapply(.x, async(.f), ...)
  when_all(.list = defs)
}

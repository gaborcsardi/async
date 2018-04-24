
#' Do every or some elements of a list satisfy an asynchronous predicate?
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @export
#' @examples
#' # Check if all numbers are odd
#' # Note the use of force() here. Otherwise x will be evaluated later,
#' # and by then its value might change.
#' is_odd <- async(function(x) {
#'   force(x)
#'   delay(1/1000)$then(function() as.logical(x %% 2))
#' })
#' synchronise(async_every(c(1,3,5,7,10,11), is_odd))
#' synchronise(async_every(c(1,3,5,7,11), is_odd))

async_every <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  deferred$new(
    type = "async_every",
    parents = defs,
    action = function(resolve) if (nx == 0) resolve(TRUE),
    parent_resolve = function(value, resolve) {
      if (!done && !isTRUE(value)) {
        done <<- TRUE
        resolve(FALSE)
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(TRUE)
      }
    }
  )
}

async_every <- mark_as_async(async_every)

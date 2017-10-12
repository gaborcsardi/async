
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
#' is_odd <- function(x) {
#'   force(x)
#'   delay(1/1000)$then(~ as.logical(x %% 2))
#' }
#' wait_for(async_every(c(1,3,5,7,10,11), is_odd))
#' wait_for(async_every(c(1,3,5,7,11), is_odd))

async_every <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  num_todo <- length(defs)
  done <- FALSE

  deferred$new(function(resolve, reject) {

    if (length(defs) == 0) return(resolve(TRUE))

    lapply(seq_along(defs), function(i) {
      defs[[i]]$then(
        function(value) {
          if (!done && !isTRUE(value)) {
            done <<- TRUE
            resolve(FALSE)
          } else {
            num_todo <<- num_todo - 1
            if (num_todo == 0) resolve(TRUE)
          }
        },
        function(reason) reject(reason)
      )
    })
  })
}

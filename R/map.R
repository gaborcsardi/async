
#' Apply an asynchronous function to each element of a vector
#'
#' @param .x A list or atomic vector.
#' @param .f Asynchronous function to apply.
#' @param ... Additional arguments to `.f`.
#' @param .args More additional arguments to `.f`.
#' @param .limit Number of elements to process simulateneously.
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

async_map <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  if (.limit < length(.x))  {
    async_map_limit(.x, .f, ..., .args = .args, .limit = .limit)
  } else {
    defs <- do.call(lapply, c(list(.x, async(.f), ...), .args))
    when_all(.list = defs)
  }
}

async_map_limit <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  force(.limit)
  .f <- async(.f)

  len <- length(.x)
  result <- structure(
    vector(mode = "list", length = len),
    names = names(.x)
  )
  args <- c(list(...), .args)
  done <- 0

  deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)
    nextone <- 1

    xfulfill <- function(value, which) {
      done <<- done + 1
      result[[which]] <<- value
      if (done == len) resolve(result)
      if (nextone <= len) {
        i <- nextone
        do.call(.f, c(list(.x[[i]]), args))$then(
          function(value) xfulfill(value, i),
          xreject
        )
      }
      nextone <<- nextone + 1
    }
    xreject <- function(reason) reject(reason)

    for (ii in seq_len(.limit)) {
      local({
        i <- ii
        do.call(.f, c(list(.x[[i]]), args))$then(
          function(value) xfulfill(value, i),
          xreject
        )
      })
      nextone <- nextone + 1
    }
  })
}

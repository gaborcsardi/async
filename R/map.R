
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
#' synchronise(async_map(
#'   seq(10, 100, by = 10) / 100,
#'   function(wait) delay(wait)$then(function() "OK")
#' ))

async_map <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  if (.limit < length(.x))  {
    async_map_limit(.x, .f, ..., .args = .args, .limit = .limit)
  } else {
    defs <- do.call(lapply, c(list(.x, async(.f), ...), .args))
    when_all(.list = defs)
  }
}

async_map <- mark_as_async(async_map)

async_map_limit <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  len <- length(.x)
  nx <- len
  .f <- async(.f)
  args <- c(list(...), .args)

  nextone <- .limit + 1L
  firsts <- lapply_args(.x[seq_len(.limit)], .f, .args = args)

  result <- structure(
    vector(mode = "list", length = len),
    names = names(.x)
  )

  self <- deferred$new(
    type = "async_map (limit)", call = sys.call(),
    action = function(resolve) {
      self; nx; firsts
      lapply(seq_along(firsts), function(idx) {
        firsts[[idx]]$then(function(val) list(idx, val))$then(self)
      })
      if (nx == 0) resolve(result)
    },
    parent_resolve = function(value, resolve) {
      self; nx; nextone; result; .f
      nx <<- nx - 1L
      result[ value[[1]] ] <<- value[2]
      if (nx == 0) {
        resolve(result)
      } else if (nextone <= len) {
        idx <- nextone
        dx <- do.call(".f", c(list(.x[[nextone]]), args))
        dx$then(function(val) list(idx, val))$then(self)
        nextone <<- nextone + 1L
      }
    }
  )

  self
}


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
#'   function(wait) delay(wait)$then(~ "OK")
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
  ids <- viapply(firsts, function(x) x$get_id())

  result <- structure(
    vector(mode = "list", length = len),
    names = names(.x)
  )

  self <- deferred$new(
    type = "async_map (limit)",
    parents = firsts,
    action = function(resolve) if (nx == 0) resolve(result),
    parent_resolve = function(value, resolve, id) {
      nx <<- nx - 1L
      result[[match(id, ids)]] <<- value
      if (nx == 0) {
        resolve(result)
      } else if (nextone <= len) {
        dx <- do.call(".f", c(list(.x[[nextone]]), args))
        ids <<- c(ids, dx$get_id())
        dx$then(self)
        nextone <<- nextone + 1L
      }
    }
  )

  self
}

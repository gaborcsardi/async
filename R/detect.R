
#' Find the value of a match, asynchronously
#'
#' All predicates are running in parallel, and the returned match
#' is not guaranteed to be the first one.
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @param .limit Number of elements to process simulateneously.
#'   If it is 1, then the predicate is applied sequentially.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @export
#' @examples
#' \donttest{
#' synchronise(async_detect(
#'   c("https://eu.httpbin.org/status/404", "https://eu.httpbin.org",
#'     "https://eu.httpbin.org/status/403"),
#'   async_sequence(http_head, function(x) x$status_code == 200)
#' ))
#' }

async_detect <- function(.x, .p, ..., .limit = Inf) {
  if (.limit < length(.x)) {
    async_detect_limit(.x, .p, ..., .limit = .limit)
  } else {
    async_detect_nolimit(.x, .p, ...)
  }
}

async_detect <- mark_as_async(async_detect)

async_detect_nolimit <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  self <- deferred$new(
    type = "async_detect", call = sys.call(),
    action = function(resolve) {
      lapply(seq_along(defs), function(idx) {
        defs[[idx]]$then(function(val) if (isTRUE(val)) idx)$then(self)
      })
      if (nx == 0) resolve(NULL)
    },
    parent_resolve = function(value, resolve) {
      if (!done && !is.null(value)) {
        done <<- TRUE
        resolve(.x[[value]])
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(NULL)
      }
    }
  )
}

async_detect_limit <- function(.x, .p, ..., .limit = .limit) {
  len <- length(.x)
  nx <- len
  .p <- async(.p)
  args <- list(...)

  done <- FALSE
  nextone <- .limit + 1L
  firsts <- lapply(.x[seq_len(.limit)], .p, ...)

  self <- deferred$new(
    type = "async_detect (limit)", call = sys.call(),
    action = function(resolve) {
      lapply(seq_along(firsts), function(idx) {
        firsts[[idx]]$then(function(val) if (isTRUE(val)) idx)$then(self)
      })
      if (nx == 0) resolve(NULL)
    },
    parent_resolve = function(value, resolve) {
      if (!done && !is.null(value)) {
        done <<- TRUE
        resolve(.x[[value]])
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) {
          resolve(NULL)
        } else if (nextone <= len) {
          idx <- nextone
          dx <- .p(.x[[nextone]], ...)
          dx$then(function(val) if (isTRUE(val)) idx)$then(self)
          nextone <<- nextone + 1L
        }
      }
    }
  )

  self
}


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
#' @param cancel Whether to cancel the deferred computations that are
#'   not needed to finish `async_detect()`, including the case when
#'   `.p` throws an error.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @export
#' @examples
#' synchronise(async_detect(
#'   c("https://eu.httpbin.org/status/404", "https://eu.httpbin.org",
#'     "https://eu.httpbin.org/status/403"),
#'   async_sequence(http_head, function(x) x$status_code == 200)
#' ))

async_detect <- function(.x, .p, ..., .limit = Inf, cancel = TRUE) {
  if (.limit < length(.x)) {
    async_detect_limit(.x, .p, ..., .limit = .limit, cancel = cancel)
  } else {
    async_detect_nolimit(.x, .p, ..., cancel = cancel)
  }
}

async_detect_nolimit <- function(.x, .p, ..., cancel) {
  force(cancel)

  defs <- lapply(.x, async(.p), ...)
  num_todo <- length(defs)
  done <- FALSE

  deferred$new(function(resolve, reject) {

    if (length(defs) == 0) return(resolve(NULL))

    lapply(seq_along(defs), function(i) {
      defs[[i]]$
        then(
          function(value) {
            if (!done && isTRUE(value)) {
              done <<- TRUE
              def__cancel_pending(defs, cancel)
              resolve(.x[[i]])
            } else {
              num_todo <<- num_todo - 1
              if (num_todo == 0) resolve(NULL)
            }
          })$
        catch(
          function(reason) {
            def__cancel_pending(defs, cancel)
            reject(reason)
          }
        )$null()
    })
  })
}

async_detect_limit <- function(.x, .p, ..., .limit = .limit, cancel) {
  force(.limit); force(cancel)
  .p <- async(.p)

  len <- length(.x)
  num_todo <- len
  args <- list(...)
  done <- FALSE

  deferred$new(function(resolve, reject) {
    force(resolve) ; force(reject)
    nextone <- 1

    xfulfill <- function(value, which) {
      if (done) return()
      if (isTRUE(value)) {
        done <<- TRUE
        def__cancel_pending(.x, cancel)
        return(resolve(.x[[which]]))
      } else {
        num_todo <<- num_todo - 1
        if (num_todo == 0) return(resolve(NULL))
        if (nextone <= len) {
          i <- nextone
          .p(.x[[i]])$
            then(function(value) xfulfill(value, i))$
            catch(xreject)$
            null()
        }
      }
      nextone <<- nextone + 1
    }
    xreject <- function(reason) {
      if (done) return()
      done <<- TRUE
      def__cancel_pending(.x, cancel)
      reject(reason)
    }

    for (ii in seq_len(.limit)) {
      local({
        i <- ii
        .p(.x[[i]])$
          then(function(value) xfulfill(value, i))$
          catch(xreject)$
          null()
      })
      nextone <- nextone + 1
    }
  })
}

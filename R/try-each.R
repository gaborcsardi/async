
#' It runs each task in series but stops whenever any of the functions were
#' successful. If one of the tasks were successful, the callback will be
#' passed the result of the successful task. If all tasks fail, the
#' callback will be passed the error and result (if any) of the final
#' attempt.
#' @param ... Deferred values to run in series.
#' @param .list More deferred values to run, `.list` is easier to use
#'   programmatically.
#' @return Resolves to the result of the first successful deferred.
#'   Otherwise throws an error. The error objects of all failed deferreds
#'   will be in the `errors` member of the error object.
#'
#' @family async control flow
#' @export
#' @examples
#' do <- function() {
#'   async_try_each(
#'     async(function() stop("doh"))(),
#'     async(function() "cool")(),
#'     async(function() stop("doh2"))(),
#'     async(function() "cool2")()
#'   )
#' }
#' synchronise(do())

async_try_each <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  wh <- nx <- NULL
  errors <- list()

  self <- deferred$new(
    type = "async_try_each",
    action = function(resolve) {
      nx <<- length(defs)
      if (nx == 0) resolve(NULL)
      wh <<- 1L
      defs[[wh]]$then(self)
    },
    parent_resolve = function(value, resolve, id) {
      resolve(value)
    },
    parent_reject = function(value, resolve, id) {
      errors <<- c(errors, list(value))
      if (wh == nx) {
        err <- structure(
          list(errors = errors, message = "async_try_each failed"),
          class = c("async_rejected", "error", "condition"))
        stop(err)
      } else {
        wh <<- wh + 1
        defs[[wh]]$then(self)
      }
    }
  )

  self
}

async_try_each <- mark_as_async(async_try_each)

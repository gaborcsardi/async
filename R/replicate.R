
#' Replicate an async function a number of times
#'
#' Similar to [base::replicate()], with some differences:
#' * it takes an async function, instead of an expression, and
#' * it always returns a list.
#'
#' @param n Number of replications.
#' @param task Async function to call.
#' @param ... Additional arguments to `task`.
#' @param .limit Number of concurrent async processes to create.
#' @return Resolves to a list of the results of the `n` `task` calls.
#'
#' @export
#' @examples
#' \donttest{
#' ## perform an HTTP request three times, and list the reponse times
#' do <- function() {
#'   async_replicate(3,
#'     function() http_get("https://eu.httpbin.org")$then(function(x) x$times))
#' }
#' synchronise(do())
#' }

async_replicate <- function(n, task, ...,  .limit = Inf) {
  assert_that(
    is_count(n),
    .limit == Inf || is_count(.limit), .limit >= 1L)

  force(list(...))
  task <- async(task)

  if (n == 0) {
    async_constant(list())
  } else if (n <= .limit) {
    async_replicate_nolimit(n, task, ...)
  } else {
    async_replicate_limit(n, task, ..., .limit = .limit)
  }
}

async_replicate_nolimit <- function(n, task, ...) {
  defs <- lapply(seq_len(n), function(i) task(...))
  when_all(.list = defs)
}

async_replicate_limit  <- function(n, task, ..., .limit = .limit) {
  n; .limit

  defs <- nextone <- result <- NULL

  self <- deferred$new(
    type = "async_replicate", call = sys.call(),
    action = function(resolve) {
      defs <<- lapply(seq_len(n), function(i) task(...))
      result <<- vector(n, mode = "list")
      lapply(seq_len(.limit), function(idx) {
        defs[[idx]]$then(function(val) list(idx, val))$then(self)
      })
      nextone <<- .limit + 1L
    },
    parent_resolve = function(value, resolve) {
      result[[ value[[1]] ]] <<- value[[2]]
      if (nextone > n) {
        resolve(result)
      } else {
        idx <- nextone
        defs[[nextone]]$then(function(val) list(idx, val))$then(self)
        nextone <<- nextone + 1L
      }
    }
  )

  self
}


#' Keep or drop elements using an asyncronous predicate function
#'
#' `async_filter` keep the elements for which `.p` is true. (Tested
#' via `isTRUE()`. `async_reject` is the opposite, it drops them.
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @return A deferred value for the result.
#'
#' @family async iterators
#' @export
#' @examples
#' ## Filter out non-working URLs
#' afun <- async(function(urls) {
#'   test_url <- async_sequence(http_head, ~ identical(.$status_code, 200L))
#'   async_filter(urls, test_url)
#' })
#' urls <- c("https://eu.httpbin.org/get",
#'           "https://eu.httpbin.org/status/404")
#' synchronise(afun(urls))

async_filter <- function(.x, .p, ...) {
  when_all(.list = lapply(.x, async(.p), ...))$
    then(function(res) .x[vlapply(res, isTRUE)])
}

async_filter <- mark_as_async(async_filter)

#' @rdname async_filter
#' @export

async_reject <- function(.x, .p, ...) {
  when_all(.list = lapply(.x, async(.p), ...))$
    then(function(res) .x[! vlapply(res, isTRUE)])
}

async_reject <- mark_as_async(async_reject)

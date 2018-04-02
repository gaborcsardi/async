
#' Keep element using an asyncronous predicate function
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

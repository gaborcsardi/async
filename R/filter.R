
#' Keep element using an asyncronous predicate function
#'
#' @param .x A list or atomic vector.
#' @param .p An asynchronous predicate function.
#' @param ... Additional arguments to the predicate function.
#' @param cancel Whether to cancel the deferred computations if `.p`
#'   throws an error.
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

async_filter <- function(.x, .p, ..., cancel = TRUE) {
  force(cancel)
  defs <- lapply(.x, async(.p), ...)
  num_todo <- length(defs)
  keep <- logical(num_todo)

  deferred$new(function(resolve, reject) {

    if (length(defs) == 0) return(resolve(.x))

    lapply(seq_along(defs), function(i) {
      defs[[i]]$
        then(
          function(value) {
            num_todo <<- num_todo - 1
            keep[i] <<- as.logical(value)
            if (num_todo == 0) {
              resolve(.x[keep])
            }
          })$
        catch(
          function(reason) {
            def__cancel_pending(defs, cancel)
            reject(reason)
          }
        )$
        null()
    })
  })
}

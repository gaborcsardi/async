
#' Deferred value for a set of deferred values
#'
#' Create a deferred value that is resolved when all listed deferred values
#' are resolved. Note that the error of an input deferred value
#' triggers the error `when_all` as well.
#'
#' async has auto-cancellation, so if one deferred value errors, the rest
#' of them will be automatically cancelled.
#'
#' @param ... Deferred values.
#' @param .list More deferred values.
#' @return A deferred value, that is conditioned on all deferred values
#'   in `...` and `.list`.
#'
#' @seealso [when_any()], [when_some()]
#' @export
#' @examples
#' ## Check that the contents of two URLs are the same
#' afun <- async(function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_all(u1, u2)$
#'     then(~ identical(.[[1]]$content, .[[2]]$content))
#' })
#' synchronise(afun())

when_all <- function(..., .list = list()) {

  defs <- c(list(...), .list)
  isdef <- vlapply(defs, is_deferred)
  nx <- sum(isdef)

  deferred$new(
    type = "when_all",
    parents = defs[isdef],
    action = function(resolve) if (nx == 0) resolve(defs),
    parent_resolve = function(value, resolve, reject) {
      nx <<- nx - 1L
      if (nx == 0L) resolve(lapply(defs, get_value_x))
    }
  )
}

when_all <- mark_as_async(when_all)

get_value_x <- function(x) {
  if (is_deferred(x)) get_private(x)$value else x
}

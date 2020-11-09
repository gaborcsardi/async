
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
#' \donttest{
#' ## Check that the contents of two URLs are the same
#' afun <- async(function() {
#'   u1 <- http_get("https://eu.httpbin.org")
#'   u2 <- http_get("https://eu.httpbin.org/get")
#'   when_all(u1, u2)$
#'     then(function(x) identical(x[[1]]$content, x[[2]]$content))
#' })
#' synchronise(afun())
#' }

when_all <- function(..., .list = list()) {

  defs <- c(list(...), .list)
  nx <- 0L

  self <- deferred$new(
    type = "when_all",
    call = sys.call(),
    action = function(resolve) {
      self; nx; defs
      lapply(seq_along(defs), function(idx) {
        if (is_deferred(defs[[idx]])) {
          nx <<- nx + 1L
          defs[[idx]]$then(function(val) list(idx, val))$then(self)
        }
      })
      if (nx == 0) resolve(defs)
    },
    parent_resolve = function(value, resolve) {
      defs[[ value[[1]] ]] <<- value[[2]]
      nx <<- nx - 1L
      if (nx == 0L) resolve(defs)
    }
  )
}

when_all <- mark_as_async(when_all)

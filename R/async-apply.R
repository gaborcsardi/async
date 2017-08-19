
#' @export

async_apply <- function(func, ...) {
  assert_that(is_task(func))
  args <- list(...)
  function(..., callback) {
    async_call(func, c(args, list(...)), callback)
  }
}

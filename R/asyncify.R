
#' @export

asyncify <- function(func) {
  assert_that(is.function(func))
  function(..., callback) {
    tryCatch(
      callback(NULL, func(...)),
      error = function(e) callback(e, NULL)
    )
  }
}

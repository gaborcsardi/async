
#' @export

asyncify <- function(func) {
  force(func)
  function(..., callback) {
    tryCatch(
      callback(NULL, func(...)),
      error = function(e) callback(e, NULL)
    )
  }
}

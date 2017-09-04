
#' @export

reflect <- function(task) {
  task <- async(task)
  function(...) {
    task(...)$then(
      function(value)  list(error = NULL, result = value),
      function(reason) list(error = reason, result = NULL)
    )
  }
}

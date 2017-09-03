
#' @export

reflect <- function(task) {
  task <- async(task)
  task()$then(
    function(value)  list(error = NULL, result = value),
    function(reason) list(error = reason, result = NULL)
  )
}

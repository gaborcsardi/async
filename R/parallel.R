
#' @export

parallel <- function(tasks, callback) {
  force(tasks) ; force(callback)
  l <- length(tasks)
  result <- vector(mode = "list", length = l)
  lapply(seq_along(tasks), function(i) {
    tasks[[i]](function(err, res) {
      if (!is.null(err)) return(callback(err))
      l <<- l - 1
      result[[i]] <<- res
      if (l == 0) callback(NULL, result)
    })
  })
}

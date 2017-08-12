
#' @export

waterfall <- function(tasks, callback) {
  force(tasks) ; force(callback)
  l <- length(tasks)
  if (l == 0) return()

  w <- 1
  mycallback <- function(err, ...) {
    if (!is.null(err)) return(callback(err, NULL))
    w <<- w + 1
    if (w > l) return(callback(NULL, ...))
    tasks[[w]](mycallback, ...)
  }

  tasks[[1]](mycallback)
}

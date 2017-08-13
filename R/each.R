
#' @export

each <- function(list, async_function, callback) {
  force(list) ; force(async_function) ; force(callback)
  l <- length(list)

  lapply(list, function(item) {
    async_function(item, function(err) {
      if (!is.null(err)) return(callback(err))
      l <<- l -1
      if (l == 0) callback(NULL)
    })
  })
}

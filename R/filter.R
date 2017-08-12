
#' @export

filter <- function(list, async_function, callback) {
  force(list) ; force(async_function) ; force(callback)
  l <- length(list)
  if (l == 0) return(callback(NULL, list))

  keep <- logical(l)
  lapply(seq_along(list), function(i) {
    async_function(list[[i]], function(err, res) {
      if (!is.null(err)) return(callback(err))
      l <<- l - 1
      keep[i] <<- res
      if (l == 0) callback(NULL, list[keep])
    })
  })
}

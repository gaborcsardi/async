
#' @export

filter <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  num_todo <- length(defs)
  keep <- logical(num_todo)

  deferred$new(function(resolve, reject) {

    if (length(defs) == 0) return(resolve(.x))

    lapply(seq_along(defs), function(i) {
      defs[[i]]$then(
        function(value) {
          num_todo <<- num_todo - 1
          keep[i] <<- as.logical(value)
          if (num_todo == 0) resolve(.x[keep])
        },
        function(reason) reject(reason)
      )
    })
  })
}


#' @export

detect <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p))
  num_todo <- length(defs)
  done <- FALSE

  deferred$new(function(resolve, reject) {

    if (length(defs) == 0) return(resolve(NULL))

    lapply(seq_along(defs), function(i) {
      defs[[i]]$then(
        function(value) {
          if (!done && isTRUE(value)) {
            done <<- TRUE
            resolve(.x[[i]])
          } else {
            num_todo <<- num_todo - 1
            if (num_todo == 0) resolve(NULL)
          }
        },
        function(reason) reject(reason)
      )
    })
  })
}

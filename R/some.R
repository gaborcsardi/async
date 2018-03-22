
#' @export
#' @rdname async_every

async_some <- function(.x, .p, ..., cancel = TRUE) {
  force(cancel)
  defs <- lapply(.x, async(.p), ...)
  num_todo <- length(defs)
  done <- FALSE

  deferred$new(function(resolve, reject) {

    if (length(defs) == 0) return(resolve(FALSE))

    lapply(seq_along(defs), function(i) {
      defs[[i]]$
        then(
          function(value) {
            if (!done && isTRUE(value)) {
              done <<- TRUE
              if (cancel) async_cancel_pending(.list = defs)
              resolve(TRUE)
            } else {
              num_todo <<- num_todo - 1
              if (num_todo == 0) resolve(FALSE)
            }
          }
        )$
        catch(
          function(reason) {
            if (cancel) async_cancel_pending(.list = defs)
            reject(reason)
          }
        )$
        null()
    })
  })
}

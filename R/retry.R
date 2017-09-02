
#' @export

retry <- function(task, times) {
  deferred$new(function(resolve, reject) {

    xresolve <- function(value) resolve(value)
    xreject  <- function(reason) {
      times <<- times - 1
      if (times > 0) {
        async(task)()$then(xresolve, xreject)
      } else {
        reject(reason)
      }
    }

    async(task)()$then(xresolve, xreject)
  })
}

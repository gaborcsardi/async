
#' @export

until <- function(test, task) {
  force(test)
  task <- async(task)

  deferred$new(function(resolve, reject) {

    force(resolve)
    force(reject)

    xresolve <- function(value) {
      tryCatch(
        if (test(value)) {
          resolve(value)
        } else {
          task()$then(xresolve, xreject)
        },
        error = function(e) reject(e)
      )
    }
    xreject <- function(reason) reject(reason)

    task()$then(xresolve, xreject)
  })
}

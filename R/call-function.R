
#' Asynchronous function call, in a worker pool
#'
#' The function will be called on another process, very much like
#' [callr::r()].
#'
#' @param func Function to call. See also the notes at [callr::r()].
#' @param args Arguments to pass to the function. They will be copied
#'   to the worker process.
#' @return Deferred object.
#'
#' @export

call_function <- function(func, args = list()) {
  func; args

  id <- NULL

  deferred$new(
    type = "pool-task", call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      id <<- get_default_event_loop()$add_pool_task(
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(func = func, args = args))
    },
    on_cancel = function(reason) {
      if (!is.null(id)) {
        get_default_event_loop()$cancel(id)
      }
    }
  )
}

call_function <- mark_as_async(call_function)

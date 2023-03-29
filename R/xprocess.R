
#' External process via a process generator
#'
#' Wrap any [processx::process] object into a deferred value. The
#' process is created by a generator function.
#'
#' @param process_generator Function that returns a [processx::process]
#'   object. See details below about the current requirements for the
#'   returned process.
#' @param error_on_status Whether to fail if the process terminates
#'   with a non-zero exit status.
#' @param ... Extra arguments, passed to `process_generator`.
#' @return Deferred object.
#'
#' Current requirements for `process_generator`:
#' * It must take a `...` argument, and pass it to
#'   `processx::process$new()`.
#' * It must use the `poll_connection = TRUE` argument.
#' These requirements might be relaxed in the future.
#'
#' If you want to obtain the standard output and/or error of the
#' process, then `process_generator` must redirect them to files.
#' If you want to discard them, `process_generator` can set them to
#' `NULL`.
#'
#' `process_generator` should not use pipes (`"|"`) for the standard
#' output or error, because the process will stop running if the
#' pipe buffer gets full. We currently never read out the pipe buffer.
#'
#' @export
#' @examples
#' \dontrun{
#' lsgen <- function(dir = ".", ...) {
#'   processx::process$new(
#'     "ls",
#'     dir,
#'     poll_connection = TRUE,
#'     stdout = tempfile(),
#'     stderr = tempfile(),
#'     ...
#'   )
#' }
#' afun <- function() {
#'   external_process(lsgen)
#' }
#' synchronise(afun())
#' }

external_process <- function(process_generator, error_on_status = TRUE,
                             ...) {

  process_generator; error_on_status; args <- list(...)
  args$encoding <- args$encoding %||% ""
  args$cleanup_tree <- args$cleanup_tree %||% TRUE

  id <- NULL

  deferred$new(
    type = "external_process", call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      px <- do.call(process_generator, args)
      stdout <- px$get_output_file()
      stderr <- px$get_error_file()
      pipe <- px$get_poll_connection()
      id <<- get_default_event_loop()$add_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(process = px, stdout = stdout, stderr = stderr,
             error_on_status = error_on_status, encoding = args$encoding)
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

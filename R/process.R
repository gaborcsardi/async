
#' Asynchronous external process execution
#'
#' Start an external process in the background, and report its completion
#' via a deferred.
#'
#' @inheritParams processx::run
#' @param error_on_status Whether to reject the referred value if the
#'    program exits with a non-zero status.
#' @return Deferred object.
#'
#' @family asynchronous external processes
#' @export
#' @importFrom processx process
#' @examples
#' \dontrun{
#' afun <- function() {
#'   run_process("ls", "-l")$
#'     then(function(x) strsplit(x$stdout, "\r?\n")[[1]])
#' }
#' synchronise(afun())
#' }

run_process <- function(command = NULL, args = character(),
  error_on_status = TRUE, wd = NULL, env = NULL,
  windows_verbatim_args = FALSE, windows_hide_window = FALSE,
  encoding = "", ...) {

  command; args; error_on_status; wd; env; windows_verbatim_args;
  windows_hide_window; encoding; list(...)

  id <- NULL

  deferred$new(
    type = "process", call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      stdout <- tempfile()
      stderr <- tempfile()
      px <- process$new(command, args = args,
        stdout = stdout, stderr = stderr, poll_connection = TRUE,
        env = env, cleanup = TRUE, cleanup_tree = TRUE, wd = wd,
        encoding = encoding, ...)
      pipe <- px$get_poll_connection()
      id <<- get_default_event_loop()$add_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(process = px, stdout = stdout, stderr = stderr,
             error_on_status = error_on_status, encoding = encoding))
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

run_process <- mark_as_async(run_process)

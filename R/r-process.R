
#' Asynchronous call to an R function, in a background R process
#'
#' Start a background R process and evaluate a function call in it.
#' It uses [callr::r_process] internally.
#'
#' @inheritParams callr::r_bg
#' @export
#' @importFrom callr r_process_options r_process rcmd_safe_env
#'
#' @examples
#' \dontrun{
#' afun <- function() {
#'   run_r_process(function() Sys.getpid())
#' }
#' synchronise(afun())
#' }

run_r_process <- function(func, args = list(), libpath = .libPaths(),
  repos = c(getOption("repos"), c(CRAN = "https://cloud.r-project.org")),
  cmdargs = c("--no-site-file", "--slave", "--no-save", "--no-restore"),
  system_profile = FALSE, user_profile = FALSE, env = rcmd_safe_env()) {

  func; args; libpath; repos; cmdargs; system_profile; user_profile; env

  id <- NULL

  deferred$new(
    type = "r-process", call = sys.calls(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      stdout <- tempfile()
      stderr <- tempfile()
      opts <- r_process_options(
        func = func, args = args, libpath = libpath, repos = repos,
        cmdargs = cmdargs, system_profile = system_profile,
        user_profile = user_profile, env = env, stdout = stdout,
        stderr = stderr, cleanup_tree = TRUE)
      rx <- r_process$new(opts)
      pipe <- rx$get_poll_connection()
      id <<- get_default_event_loop()$add_r_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(process = rx, stdout = stdout, stderr = stderr,
             error_on_status = TRUE, encoding = ""))
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

run_r_process <- mark_as_async(run_r_process)

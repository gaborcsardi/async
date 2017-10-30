
#' Wait for all deferred values in an environment
#'
#' The function returns when all deferred values are resolved (or
#' rejected).
#'
#' It is permitted to change the environment (from within the async
#' functions, as the is no synchronous code running and R is single
#' threaded) while the wait is taking place. This allows waiting on a
#' dynamic set of async operations.
#' 
#' @param env Environment to wait on. An error is thrown if not an
#'   environment.
#' @return A named list of resolved values. An unhandled rejection make
#'   `await_env` throw an error. Note that since the environment is a hash,
#'   the order of the values is arbitrary.
#' 
#' @family await functions
#' @export
#' @examples
#' ## dynamically change the async operations we are waiting on
#' afun <- async(function() {
#'   env <- new.env()
#'   env$foo <- delay(1/10000)$
#'     then(function() {
#'       env$foo2 <- async_constant("OK2")
#'       "OK"
#'     })
#'   await_env(env)
#' })
#' sync_wrap(afun())

await_env <- function(env) {
  assert_that(is.environment(env))

  ## TODO: check event loop of deferred values in the environment?
  num_pending <- function(env) {
    sum(eapply(env, get_state_x, all.names = TRUE) == "pending")
  }
  
  while (num_pending(env) > 0) get_default_event_loop()$run("once")

  eapply(env, get_value_x, all.names = TRUE)
}

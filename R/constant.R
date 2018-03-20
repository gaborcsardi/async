
#' Make a minimal deferred that resolves to the specified value
#'
#' This is sometimes useful to start a deferred chain.
#'
#' @param value The value to resolve to.
#' @return A deferred value.
#'
#' @export
#' @examples
#' afun <- async(function() {
#'   async_constant(1/100)$
#'     then(function(x) delay(x))$
#'     then(function(x) print(x))
#' })
#' synchronise(afun())

async_constant <- function(value = NULL) {
  force(value)
  deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)
    get_default_event_loop()$add_next_tick(
      function() { },
      function(err, res) if (is.null(err)) resolve(value) else reject(err),
      deferred = environment(resolve)$self
    )
  })
}

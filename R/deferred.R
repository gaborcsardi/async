
#' @importFrom R6 R6Class
#' @export

deferred <- R6Class(
  "deferred",
  public = list(
    initialize = function(action)
      def_init(self, private, action),
    get_status = function()
      private$status,
    then = function(on_fulfilled = NULL, on_rejected = NULL)
      def_then(self, private, on_fulfilled, on_rejected),
    get_value = function()
      def_get_value(self, private)
  ),

  private = list(
    status = c("pending", "fulfilled", "rejected")[1],
    id = NULL,
    value = NULL,
    on_fulfilled = list(),
    on_rejected = list(),

    resolve = function(value)
      def__resolve(self, private, value),
    reject = function(reason)
      def__reject(self, private, reason),

    ## TODO: this is temporary
    set_id = function(id)
      private$id <- id
  )
)

def_init <- function(self, private, action) {
  ## TODO
  ## assert_that(is_action_function(action))
  action(private$resolve, private$reject)
  invisible(self)
}

def_then <- function(self, private, on_fulfilled, on_rejected) {
  ## TODO
  ## assert_that(
  ##   is_function_or_null(on_fulfilled),
  ##   is_function_or_null(on_rejected)
  ## )
  force(on_fulfilled)
  force(on_rejected)
  promise$new(function(resolve, reject) {

    handle_fulfill <- function(value) {
      if (is.function(on_fulfilled)) value <- on_fulfilled(value)
      resolve(value)
    }

    handle_reject <- function(reason) {
      if (is.function(on_rejected)) {
        resolve(on_rejected(reason))
      } else {
        reject(reason)
      }
    }

    private$on_fulfilled <- c(private$on_fulfilled, list(handle_fulfill))
    private$on_rejected <- c(private$on_rejected, list(handle_reject))
  })
}

def_get_value <- function(self, private) {
  if (private$status == "pending") {
    stop("Deferred value not resolved yet")
  } else if (private$status == "rejected") {
    stop(private$value)
  } else {
    private$value
  }
}

def__resolve <- function(self, private, value) {
  if (private$status != "pending") stop("Deferred value already resolved")
  if (is.deferred(value)) {
    value$then(private$resolve, private$reject)
  } else {
    private$status <- "fulfilled"
    private$value <- value
    loop <- get_default_event_loop()
    for (f in private$on_fulfilled) loop$defer_next_tick(f, list(value))
    private$on_fulfilled <- list()
  }
}

def__reject <- function(self, private, reason) {
  if (private$status != "pending") stop("Deferred value already resolved")
  if (is.deferred(reason)) {
    reason$then(private$reject, private$reject)
  } else {
    private$status <- "rejected"
    private$value <- reason
    loop <- get_default_event_loop()
    for (f in private$on_rejected) loop$defer_next_tick(f, list(reason))
  }
}

#' @export

is.deferred <- function(x) {
  inherits(x, "deferred")
}

#' @export

await <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  ids <- vcapply(defs, function(x) x$.__enclos_env__$private$id)
  get_default_event_loop()$wait_for(ids)
}

#' @export

async <- function(fun) {
  attr(fun, "async") <- list(TRUE)
  fun
}

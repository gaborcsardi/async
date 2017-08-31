
#' @importFrom R6 R6Class
#' @export

deferred <- R6Class(
  "deferred",
  public = list(
    initialize = function(action)
      def_init(self, private, action),
    get_state = function()
      private$state,
    then = function(on_fulfilled = NULL, on_rejected = NULL)
      def_then(self, private, on_fulfilled, on_rejected),
    get_value = function()
      def_get_value(self, private)
  ),

  private = list(
    state = c("pending", "fulfilled", "rejected")[1],
    id = NULL,
    task = NULL,
    value = NULL,
    on_fulfilled = list(),
    on_rejected = list(),

    resolve = function(value)
      def__resolve(self, private, value),
    reject = function(reason)
      def__reject(self, private, reason),

    ## TODO: this is temporary
    set_id = function(id)
      private$id <- id,
    set_task = function(task) {
      private$task <- task
      private$id <- task$id
    }
  )
)

def_init <- function(self, private, action) {
  assert_that(is_action_function(action))
  action(private$resolve, private$reject)
  invisible(self)
}

#' @importFrom rlang as_function

def_then <- function(self, private, on_fulfilled, on_rejected) {
  on_fulfilled <- if (!is.null(on_fulfilled)) as_function(on_fulfilled)
  on_rejected  <- if (!is.null(on_rejected))  as_function(on_rejected)
  def <- deferred$new(function(resolve, reject) {
    force(resolve)
    force(reject)

    handle_fulfill <- function(value) {
      tryCatch(
        {
          if (is.function(on_fulfilled)) value <- on_fulfilled(value)
          resolve(value)
        },
        error = function(e) reject(e)
      )
    }

    handle_reject <- function(reason) {
      tryCatch(
        {
          if (is.function(on_rejected)) {
            reason <- on_rejected(reason)
            resolve(reason)
          } else {
            reject(reason)
          }
        },
        error = function(e) reject(e)
      )
    }

    if (private$state == "pending") {
      private$on_fulfilled <- c(private$on_fulfilled, list(handle_fulfill))
      private$on_rejected <- c(private$on_rejected, list(handle_reject))

    } else if (private$state == "fulfilled") {
      get_default_event_loop()$defer_next_tick(
        handle_fulfill, list(private$value))

    } else if (private$state == "rejected") {
      get_default_event_loop()$defer_next_tick(
        handle_reject, list(private$value))
    }
  })

  def$.__enclos_env__$private$set_task(
    get_default_event_loop()$run_generic(NULL)
  )

  def
}

def_get_value <- function(self, private) {
  if (private$state == "pending") {
    stop("Deferred value not resolved yet")
  } else if (private$state == "rejected") {
    stop(private$value)
  } else {
    private$value
  }
}

def__resolve <- function(self, private, value) {
  if (private$state != "pending") stop("Deferred value already resolved")
  if (is.deferred(value)) {
    value$then(private$resolve, private$reject)
  } else {
    private$state <- "fulfilled"
    private$value <- value
    loop <- get_default_event_loop()
    for (f in private$on_fulfilled) loop$defer_next_tick(f, list(value))
    private$on_fulfilled <- list()
    if (!is.null(private$task)) private$task$callback()
  }
}

def__reject <- function(self, private, reason) {
  if (private$state != "pending") stop("Deferred value already resolved")
  if (is.deferred(reason)) {
    reason$then(private$resolve, private$reject)
  } else {
    private$state <- "rejected"
    private$value <- reason
    loop <- get_default_event_loop()
    for (f in private$on_rejected) loop$defer_next_tick(f, list(reason))
    private$on_rejected <- list()
    if (!is.null(private$task)) private$task$callback()
  }
}

#' @export

is.deferred <- function(x) {
  inherits(x, "deferred")
}

#' @export

await <- function(def) {
  await_list(def)[[1]]
}

#' @export

await_list <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  states <- vcapply(defs, get_state_x)
  while (any(states == "pending")) {
    ids <- na.omit(vcapply(defs, get_id_x))
    get_default_event_loop()$wait_for(ids)
    defs <- lapply(defs, get_value_x)
    states <- vcapply(defs, get_state_x)
  }
  lapply(defs, get_value_x)
}

get_state_x <- function(x) {
  if (is.deferred(x)) x$get_state() else "not-deferred"
}

get_id_x <- function(x) {
  if (is.deferred(x)) x$.__enclos_env__$private$id else NA_character_
}

get_value_x <- function(x) {
  if (is.deferred(x)) x$get_value() else x
}

make_resolved_deferred <- function(x) {
  force(x)
  deferred$new(function(resolve, reject) {
    resolve(x)
  })
}

make_rejected_deferred <- function(x) {
  force(x)
  deferred$new(function(resolve, reject) {
    reject(x)
  })
}

#' @export

async <- function(fun) {
  assert_that(is.function(fun))
  async_fun <- function(...) {
    tryCatch(
      {
        r <- fun(...)
        if (is.deferred(r)) r else make_resolved_deferred(r)
      },
      error = function(e) {
        make_rejected_deferred(e)
      }
    )
  }
  attr(async_fun, "async") <- list(TRUE)
  async_fun
}

#' @export

is_async <- function(fun) {
  assert_that(is.function(fun))
  is.list(a <- attr(fun, "async")) && identical(a[[1]], TRUE)
}

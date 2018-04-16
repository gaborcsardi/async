
#' @importFrom R6 R6Class

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function()
      el_init(self, private),

    add_http = function(handle, callback, file = NULL, progress = NULL)
      el_add_http(self, private, handle, callback, file, progress),
    add_delayed = function(delay, func, callback)
      el_add_delayed(self, private, delay, func, callback),
    add_next_tick = function(func, callback)
      el_add_next_tick(self, private, func, callback),

    cancel = function(id)
      el_cancel(self, private, id),
    cancel_all = function()
      el_cancel_all(self, private),

    run = function(mode = c("default", "nowait", "once"))
      el_run(self, private, mode = match.arg(mode))
  ),

  private = list(
    create_task = function(callback, ..., id =  NULL, type = "foobar")
      el__create_task(self, private, callback, ..., id = id, type = type),
    ensure_pool = function(...)
      el__ensure_pool(self, private, ...),
    get_poll_timeout = function()
      el__get_poll_timeout(self, private),
    run_pending = function()
      el__run_pending(self, private),
    run_timers = function()
      el__run_timers(self, private),
    is_alive = function()
      el__is_alive(self, private),
    update_time = function()
      el__update_time(self, private),

    time = Sys.time(),
    stop_flag = FALSE,
    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL,
    next_ticks = character()
  )
)

el_init <- function(self, private) {
  invisible(self)
}

#' @importFrom curl multi_add parse_headers_list handle_data

el_add_http <- function(self, private, handle, callback, progress, file) {
  self; private; handle; callback; progress; outfile <- file

  id  <- private$create_task(callback, list(handle = handle), type = "http")
  private$ensure_pool()
  if (!is.null(outfile) && file.exists(outfile)) unlink(outfile)

  content <- NULL

  multi_add(
    handle = handle,
    pool = private$pool,
    done = function(response) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      response$content <- do.call(c, as.list(content))
      response$file <- outfile
      task$callback(NULL, response)
    },
    data = function(bytes, ...) {
      if (!is.null(outfile)) {
        ## R runs out of connections very quickly, especially because they
        ## are not removed until a gc(). However, calling gc() is
        ## expensive, so we only do it if we have to. This is a temporary
        ## solution until we can use our own connections, that are not
        ## so limited in their numbers.
        con <- tryCatch(
          file(outfile, open = "ab"),
          error = function(e) { gc(); file(outfile, open = "ab") } # nocov
        )
        writeBin(bytes, con)
        close(con)
      } else {
        content <<- c(content, list(bytes))
      }
    },
    fail = function(error) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      error <- make_error(message = error)
      class(error) <- unique(c("async_rejected", "async_http_error",
                               class(error)))
      task$callback(error, NULL)
    }
  )
  id
}

el_add_delayed <- function(self, private, delay, func, callback) {
  force(self); force(private); force(delay); force(func); force(callback)
  id <- private$create_task(
    callback,
    data = list(delay = delay, func = func),
    type = "delayed"
  )
  private$timers[id] <- Sys.time() + as.difftime(delay, units = "secs")
  id
}

el_add_next_tick <- function(self, private, func, callback) {
  force(self) ; force(private) ; force(callback)
  id <- private$create_task(callback, data = list(func = func),
                            type = "nexttick")
  private$next_ticks <- c(private$next_ticks, id)
}

#' @importFrom curl multi_cancel

el_cancel <- function(self, private, id) {
  private$next_ticks <- setdiff(private$next_ticks, id)
  private$timers  <- private$timers[setdiff(names(private$times), id)]
  if (id %in% names(private$tasks) && private$tasks[[id]]$type == "http") {
    multi_cancel(private$tasks[[id]]$data$handle)
  }
  private$tasks[[id]] <- NULL
  invisible(self)
}

#' @importFrom curl multi_cancel multi_list

el_cancel_all <- function(self, private) {
  http <- multi_list(pool = private$pool)
  lapply(http, multi_cancel)
  private$next_ticks <- character()
  private$timers <- Sys.time()[numeric()]
  private$tasks <-  list()
  invisible(self)
}

#' @importFrom curl multi_run multi_list

el_run <- function(self, private, mode) {

  ## This is closely modeled after the libuv event loop, on purpose,
  ## because some time we might switch to that.
  alive <- private$is_alive()
  if (! alive) private$update_time()

  while (alive && ! private$stop_flag) {
    private$update_time()
    private$run_timers()
    ran_pending <- private$run_pending()
    ## private$run_idle()
    ## private$run_prepare()

    num_poll <- length(multi_list(pool = private$pool))
    timeout <- 0
    if (mode == "once" && !ran_pending || mode == "default") {
      timeout <- private$get_poll_timeout()
    }
    if (num_poll) {
      multi_run(timeout = timeout, poll = TRUE, pool = private$pool)
    } else if (length(private$timers)) {
      Sys.sleep(timeout)
    }

    ## private$run_check()
    ## private$run_closing_handles()

    if (mode == "once") {
      private$update_time()
      private$run_timers()
    }

    alive <- private$is_alive()
    if (mode == "once" || mode == "nowait") break
  }

  private$stop_flag <- FALSE

  alive
}

el__run_pending <- function(self, private) {
  next_ticks <- private$next_ticks
  private$next_ticks <- character()
  for (id in next_ticks) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    call_with_callback(task$data$func, task$callback)
  }

  length(next_ticks) > 0
}

#' @importFrom uuid UUIDgenerate

el__create_task <- function(self, private, callback, data, ..., id, type) {
  id <- id %||% UUIDgenerate()
  private$tasks[[id]] <- list(
    type = type,
    id = id,
    callback = callback,
    data = data,
    error = NULL,
    result = NULL
  )
  id
}

#' @importFrom curl new_pool

el__ensure_pool <- function(self, private, ...) {
  if (is.null(private$pool)) private$pool <- new_pool(...)
}

el__get_poll_timeout <- function(self, private) {
  if (length(private$next_ticks)) {
    ## TODO: can this happen at all? Probably not, but it does not hurt...
    0 # nocov
  } else {
    max(0, min(Inf, private$timers - private$time))
  }
}

el__run_timers <- function(self, private) {
  expired <- names(private$timers)[private$timers <= private$time]
  expired <- expired[order(private$timers[expired])]
  for (id in expired) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    private$timers <- private$timers[setdiff(names(private$timers), id)]
    call_with_callback(task$data$func, task$callback)
  }
}

el__is_alive <- function(self, private) {
  length(private$tasks) > 0 ||
    length(private$timers) > 0 ||
    length(private$next_ticks) > 0
}

el__update_time <- function(self, private) {
  private$time <- Sys.time()
}

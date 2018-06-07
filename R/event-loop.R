
#' @importFrom R6 R6Class

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function()
      el_init(self, private),

    add_http = function(handle, callback, file = NULL, progress = NULL,
                        data = NULL)
      el_add_http(self, private, handle, callback, file, progress, data),
    add_process = function(conns, callback, data)
      el_add_process(self, private, conns, callback, data),
    add_delayed = function(delay, func, callback, rep = FALSE)
      el_add_delayed(self, private, delay, func, callback, rep),
    add_next_tick = function(func, callback, data = NULL)
      el_add_next_tick(self, private, func, callback, data),

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

el_add_http <- function(self, private, handle, callback, progress, file,
                        data) {
  self; private; handle; callback; progress; outfile <- file; data

  id  <- private$create_task(callback, list(handle = handle, data = data),
                             type = "http")
  private$ensure_pool()
  if (!is.null(outfile)) cat("", file = outfile)

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

el_add_process <-  function(self, private, conns, callback, data) {
  self; private; conns; callback; data
  data$conns <- conns
  private$create_task(callback, data, type = "process")
}

el_add_delayed <- function(self, private, delay, func, callback, rep) {
  force(self); force(private); force(delay); force(func); force(callback)
  force(rep)
  id <- private$create_task(
    callback,
    data = list(delay = delay, func = func, rep = rep),
    type = "delayed"
  )
  private$timers[id] <- Sys.time() + as.difftime(delay, units = "secs")
  id
}

el_add_next_tick <- function(self, private, func, callback, data) {
  force(self) ; force(private) ; force(callback); force(data)
  data$func <- func
  id <- private$create_task(callback, data = data, type = "nexttick")
  private$next_ticks <- c(private$next_ticks, id)
}

#' @importFrom curl multi_cancel

el_cancel <- function(self, private, id) {
  private$next_ticks <- setdiff(private$next_ticks, id)
  private$timers  <- private$timers[setdiff(names(private$timers), id)]
  if (id %in% names(private$tasks) && private$tasks[[id]]$type == "http") {
    multi_cancel(private$tasks[[id]]$data$handle)
  } else if (id %in% names(private$tasks) &&
             private$tasks[[id]]$type == "process") {
    private$tasks[[id]]$data$process$kill()
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

#' @importFrom curl multi_run multi_list multi_fdset
#' @importFrom processx conn_get_fileno

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

    num_http <- length(multi_list(pool = private$pool))
    types <- vcapply(private$tasks, "[[", "type")
    num_proc <- sum(types == "process")
    num_poll <- num_http + num_proc

    timeout <- 0

    if (mode == "once" && !ran_pending || mode == "default") {
      timeout <- private$get_poll_timeout()
    }

    if (num_poll) {

      fds <- fds_http <- fds_proc <- integer()

      ## File desciptors to poll for HTTP
      if (num_http) {
        fds_http <- multi_fdset(pool = private$pool)
        fds <- c(fds, as.integer(fds_http$reads))
        if (length(fds_http$reads) && fds_http$timeout < timeout * 1000) {
          timeout <- fds_http$timeout / 1000.0
        }
      }

      ## File descriptors to poll for processes
      if (num_proc) {
        procs <- private$tasks[types == "process"]
        fds_proc <-
          lapply(procs, function(t) lapply(t$data$conns, conn_get_fileno))
        fds <- c(fds, unlist(fds_proc))
      }

      timeout <- if (is.finite(timeout)) timeout * 1000 else -1L

      ## Poll
      ready <- .Call(c_async_poll, fds, as.integer(timeout))

      if (num_http) {
        multi_run(timeout = 0L, poll = TRUE, pool = private$pool)
      }

      ## Processes ready
      if (num_proc) {
        ready_proc_fds <- fds[ready & fds %in% unlist(fds_proc)]
        ready_proc <- procs[vlapply(fds_proc, function(x) any(x %in% ready_proc_fds))]
        for (p in ready_proc)  {
          private$tasks[[p$id]] <- NULL
          p$data$process$wait(1000)
          p$data$process$kill()
          res <- list(
            status = p$data$process$get_exit_status(),
            stdout = read_all(p$data$stdout, p$data$encoding),
            stderr = read_all(p$data$stderr, p$data$encoding),
            timeout = FALSE
          )
          unlink(c(p$data$stdout, p$data$stderr))

          if (p$data$error_on_status && res$status != 0) {
            err <- make_error("process exited with non-zero status")
            err$data <- res
            res <- NULL
          } else {
            err <- NULL
          }
          p$callback(err, res)
        }
      }

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
    call_with_callback(task$data$func, task$callback,
                       info = task$data$error_info)
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
    if (private$tasks[[id]]$data$rep) {
      ## If it is repeated, then re-init
      private$timers[id] <-
        private$time + as.difftime(task$data$delay, units = "secs")
    } else {
      ## Otherwise remove
      private$tasks[[id]] <- NULL
      private$timers <- private$timers[setdiff(names(private$timers), id)]
    }
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

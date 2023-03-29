
#' @importFrom R6 R6Class

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function()
      el_init(self, private),

    add_http = function(handle, callback, file = NULL, progress = NULL,
                        data = NULL)
      el_add_http(self, private, handle, callback, file, progress, data),
    http_setopt = function(total_con = NULL, host_con = NULL, multiplex = NULL)
      el_http_setopt(self, private, total_con, host_con, multiplex),

    add_process = function(conns, callback, data)
      el_add_process(self, private, conns, callback, data),
    add_r_process = function(conns, callback, data)
      el_add_r_process(self, private, conns, callback, data),
    add_pool_task = function(callback, data)
      el_add_pool_task(self, private, callback, data),
    add_delayed = function(delay, func, callback, rep = FALSE)
      el_add_delayed(self, private, delay, func, callback, rep),
    add_next_tick = function(func, callback, data = NULL)
      el_add_next_tick(self, private, func, callback, data),

    cancel = function(id)
      el_cancel(self, private, id),
    cancel_all = function()
      el_cancel_all(self, private),

    run = function(mode = c("default", "nowait", "once"))
      el_run(self, private, mode = match.arg(mode)),

    suspend = function()
      el_suspend(self, private),
    wakeup = function()
      el_wakeup(self, private)
  ),

  private = list(
    create_task = function(callback, ..., id =  NULL, type = "foobar")
      el__create_task(self, private, callback, ..., id = id, type = type),
    ensure_pool = function()
      el__ensure_pool(self, private),
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
    io_poll = function(timeout)
      el__io_poll(self, private, timeout),
    update_curl_data = function()
      el__update_curl_data(self, private),

    id = NULL,
    time = Sys.time(),
    stop_flag = FALSE,
    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL,
    curl_fdset = NULL,                 # return value of multi_fdset()
    curl_poll = TRUE,                  # should we poll for curl sockets?
    curl_timer = NULL,                 # call multi_run() before this
    next_ticks = character(),
    worker_pool = NULL,
    http_opts = NULL
  )
)

el_init <- function(self, private) {
  private$id <- new_event_loop_id()
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

el_add_process <- function(self, private, conns, callback, data) {
  self; private; conns; callback; data
  data$conns <- conns
  private$create_task(callback, data, type = "process")
}

el_add_r_process <- function(self, private, conns, callback, data) {
  self; private; conns; callback; data
  data$conns <- conns
  private$create_task(callback, data, type = "r-process")
}

el_add_pool_task <- function(self, private, callback, data) {
  self; private; callback; data
  id <- private$create_task(callback, data, type = "pool-task")
  if (is.null(async_env$worker_pool)) {
    async_env$worker_pool <- worker_pool$new()
  }
  async_env$worker_pool$add_task(data$func, data$args, id, private$id)
  id
}

el_add_delayed <- function(self, private, delay, func, callback, rep) {
  force(self); force(private); force(delay); force(func); force(callback)
  force(rep)
  id <- private$create_task(
    callback,
    data = list(delay = delay, func = func, rep = rep),
    type = "delayed"
  )
  # This has to be real time, because our event loop time might
  # be very much in the past when his is called.
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
             private$tasks[[id]]$type %in% c("process", "r-process")) {
    private$tasks[[id]]$data$process$kill()
  } else if (id %in% names(private$tasks) &&
             private$tasks[[id]]$type == "pool-task") {
    async_env$worker_pool$cancel_task(id)
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

  ## Need to cancel pool tasks, these are interrupts for the workers
  types <- vcapply(private$tasks, "[[", "type")
  ids <- vcapply(private$tasks, "[[", "id")
  for (id in ids[types == "pool-task"]) {
    self$cancel(id)
  }

  private$tasks <-  list()
  invisible(self)
}

el_run <- function(self, private, mode) {

  ## This is closely modeled after the libuv event loop, on purpose,
  ## because some time we might switch to that.

  alive <- private$is_alive()
  if (! alive) private$update_time()

  while (alive && !private$stop_flag) {
    private$update_time()
    private$update_curl_data()
    private$run_timers()
    ran_pending <- private$run_pending()
    ## private$run_idle()
    ## private$run_prepare()

    timeout <- 0
    if ((mode == "once" && !ran_pending) || mode == "default") {
      timeout <- private$get_poll_timeout()
    }

    private$io_poll(timeout)
    ## private$run_check()
    ## private$run_closing_handles()

    if (mode == "once") {
      ## If io_poll returned without doing anything, that means that
      ## we have some timers that are due, so run those.
      ## At this point we have surely made progress
      private$update_time()
      private$run_timers()
    }

    alive <- private$is_alive()
    if (mode == "once" || mode == "nowait") break
  }

  private$stop_flag <- FALSE

  alive
}

el_suspend <- function(self, private) {
  ## TODO
}

el_wakeup <- function(self, private) {
  ## TODO
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

  ## Check for workers from the pool finished before, while another
  ## event loop was active
  finished_pool <- FALSE
  pool <- async_env$worker_pool
  if (!is.null(pool)) {
    done_pool <- pool$list_tasks(event_loop = private$id, status = "done")
    finished_pool <- nrow(done_pool) > 0
    for (tid in done_pool$id) {
      task <- private$tasks[[tid]]
      private$tasks[[tid]] <- NULL
      res <- pool$get_result(tid)
      err <- res$error
      res <- res[c("result", "stdout", "stderr")]
      task$callback(err, res)
    }
  }

  length(next_ticks) > 0 || finished_pool
}

#' @importFrom curl multi_run multi_fdset

el__io_poll <- function(self, private, timeout) {

  types <- vcapply(private$tasks, "[[", "type")

  ## The things we need to poll, and their types
  ## We put the result here as well
  pollables <- data.frame(
    stringsAsFactors = FALSE,
    id = character(),
    pollable = I(list()),
    type = character(),
    ready = character()
  )

  ## HTTP.
  if (private$curl_poll) {
    curl_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = "curl",
      pollable = I(list(processx::curl_fds(private$curl_fdset))),
      type = "curl",
      ready = "silent")
    pollables <- rbind(pollables, curl_pollables)
  }

  ## Processes
  proc <- types %in% c("process", "r-process")
  if (sum(proc)) {
    proc_conns <- lapply(private$tasks[proc], function(t) t$data$conns)
    proc_conns <- map2(proc_conns, names(proc_conns), function(c, id) {
      data.frame(id = id, pollable = I(unname(c)), type = names(c))
    })

    proc_pollables <- do.call(rbind, proc_conns)
    proc_pollables$ready <- "silent"

    pollables <- rbind(pollables, proc_pollables)
  }

  ## Pool
  px_pool <- if (!is.null(async_env$worker_pool)) {
    async_env$worker_pool$get_poll_connections()
  }
  if (length(px_pool)) {
    pool_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = names(px_pool),
      pollable = I(px_pool),
      type = rep("pool", length(px_pool)),
      ready = rep("silent", length(px_pool)))
    pollables <- rbind(pollables, pool_pollables)
  }

  if (!is.null(private$curl_timer) && private$curl_timer <= private$time) {
    multi_run(timeout = 0L, poll = TRUE, pool = private$pool)
    private$curl_timer <- NULL
  }

  if (nrow(pollables)) {

    ## OK, ready to poll
    pollables$ready <- unlist(processx::poll(pollables$pollable, timeout))

    ## Any HTTP?
    if (private$curl_poll &&
        pollables$ready[match("curl", pollables$type)] == "event") {
      multi_run(timeout = 0L, poll = TRUE, pool = private$pool)
    }

    ## Any process outputs
    proc_output <- pollables$type %in% c("stdout", "stderr") &
      pollables$ready == "ready"

    for (i in which(proc_output)) {
      pollable <- pollables[i, ]
      px <- private$tasks[[pollable$id]]$data$process
      buffers <- private$tasks[[pollable$id]]$data$buffers

      if (pollable$type == "stdout") {
        buffers$stdout$push(px$read_output())
      } else {
        buffers$stderr$push(px$read_error())
      }
    }

    ## Any processes
    proc_ready <- pollables$type %in% c("process", "r-process") &
      pollables$ready == "ready"
    for (id in pollables$id[proc_ready]) {
      p <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      ## TODO: this should be async
      p$data$process$wait(1000)
      encoding <- p$data$encoding

      stdout <- switch(
        px_file_type(p$data$stdout),
        conn = p$data$buffers$stdout$read(),
        file = read_all(p$data$stdout, encoding),
        NULL
      )
      stderr <- switch(
        px_file_type(p$data$stderr),
        conn = p$data$buffers$stderr$read(),
        file = read_all(p$data$stderr, encoding),
        NULL
      )

      res <- list(
        status = p$data$process$get_exit_status(),
        stdout = stdout,
        stderr = stderr,
        timeout = FALSE
      )

      p$data$process$kill()
      for (b in p$data$buffers) b$done()

      error <- FALSE
      if (p$type == "r-process") {
        res$result <- tryCatch({
          p$data$process$get_result()
        }, error = function(e) { error <<- TRUE; e })
      }

      unlink(c(p$data$stdout, p$data$stderr))

      if (p$data$error_on_status && (error || res$status != 0)) {
        err <- make_error("process exited with non-zero status")
        err$data <- res
        res <- NULL
      } else {
        err <- NULL
      }
      p$callback(err, res)
    }

    ## Worker pool
    pool_ready <- pollables$type == "pool" & pollables$ready == "ready"
    if (sum(pool_ready)) {
      pool <- async_env$worker_pool
      done <- pool$notify_event(as.integer(pollables$id[pool_ready]),
                                event_loop = private$id)
      mine <- intersect(done, names(private$tasks))
      for (tid in mine) {
        task <- private$tasks[[tid]]
        private$tasks[[tid]] <- NULL
        res <- pool$get_result(tid)
        err <- res$error
        res <- res[c("result", "stdout", "stderr")]
        task$callback(err, res)
      }
    }

  } else if (length(private$timers) || !is.null(private$curl_timer)) {
    Sys.sleep(timeout / 1000)
  }
}

el__create_task <- function(self, private, callback, data, ..., id, type) {
  id <- id %||% get_uuid()
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

el__ensure_pool <- function(self, private) {
  getopt <- function(nm) {
    anm <- paste0("async_http_", nm)
    if (!is.null(v <- getOption(anm))) return(v)
    if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) return(v)
    NULL
  }
  if (is.null(private$pool)) {
    private$http_opts <- list(
      total_con = getopt("total_con") %||% 100,
      host_con = getopt("host_con") %||%  6,
      multiplex  = getopt("multiplex") %||% TRUE
    )
    private$pool <- new_pool(
      total_con = private$http_opts$total_con,
      host_con =  private$http_opts$host_con,
      multiplex = private$http_opts$multiplex
    )
  }
}

#' @importFrom curl multi_set

el_http_setopt <- function(self, private, total_con, host_con, multiplex) {
  private$ensure_pool()
  if (!is.null(total_con)) private$http_opts$total_con <- total_con
  if (!is.null(host_con))  private$http_opts$host_con  <- host_con
  if (!is.null(multiplex)) private$http_opts$multiplex <- multiplex
  multi_set(
    pool = private$pool,
    total_con = private$http_opts$total_con,
    host_con = private$http_opts$host_con,
    multiplex = private$http_opts$multiplex
  )
}

el__get_poll_timeout <- function(self, private) {
  t <- if (length(private$next_ticks)) {
    ## TODO: can this happen at all? Probably not, but it does not hurt...
    0 # nocov
  } else {
    max(0, min(Inf, private$timers - private$time))
  }

  if (!is.null(private$curl_timer)) {
    t <- min(t, private$curl_timer - private$time)
  }

  t <- max(t, 0)

  if (is.finite(t)) as.integer(t * 1000) else -1L
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

#' @importFrom curl multi_fdset
#'
el__update_curl_data <- function(self, private) {
  private$curl_fdset <- multi_fdset(private$pool)
  num_fds <- length(unique(unlist(private$curl_fdset[1:3])))
  private$curl_poll <- num_fds > 0
  private$curl_timer <- if ((t <- private$curl_fdset$timeout) != -1) {
    private$time + as.difftime(t / 1000.0, units = "secs")
  }
}


#' Worker pool
#'
#' The worker pool functions are independent of the event loop, to allow
#' independent testing.
#'
#' @family worker pool functions
#' @name worker_pool
#' @keywords internal
#' @importFrom R6 R6Class
NULL

worker_pool <- R6Class(
  public = list(
    initialize = function()
      wp_init(self, private),
    add_task = function(func, args, id, event_loop)
      wp_add_task(self, private, func, args, id, event_loop),
    get_fds = function()
      wp_get_fds(self, private),
    get_pids = function()
      wp_get_pids(self, private),
    get_poll_connections = function()
      wp_get_poll_connections(self, private),
    notify_event = function(pids, event_loop)
      wp_notify_event(self, private, pids, event_loop),
    start_workers = function()
      wp_start_workers(self, private),
    kill_workers = function()
      wp_kill_workers(self, private),
    cancel_task = function(id)
      wp_cancel_task(self, private, id),
    cancel_all_tasks = function()
      wp_cancel_all_tasks(self, private),
    get_result = function(id)
      wp_get_result(self, private, id),
    list_workers = function()
      wp_list_workers(self, private),
    list_tasks = function(event_loop = NULL, status = NULL)
      wp_list_tasks(self, private, event_loop, status),
    finalize = function() self$kill_workers()
  ),

  private = list(
    workers = list(),
    tasks = list(),

    try_start = function()
      wp__try_start(self, private),
    interrupt_worker = function(pid)
      wp__interrupt_worker(self, private, pid)
  )
)

wp_init <- function(self, private) {
  self$start_workers()
  invisible(self)
}

wp_start_workers <- function(self, private) {
  num <- worker_pool_size()

  ## See if we need to start more
  if (NROW(private$workers) >= num) return(invisible())

  ## Yeah, start some more
  to_start <- num - NROW(private$workers)
  sess <- lapply(1:to_start, function(x) callr::r_session$new(wait = FALSE))
  fd <- viapply(sess, function(x) processx::conn_get_fileno(x$get_poll_connection()))
  new_workers <- data.frame(
    stringsAsFactors = FALSE,
    session = I(sess),
    task = NA_character_,
    pid = viapply(sess, function(x) x$get_pid()),
    fd = fd,
    event_loop = NA_integer_
  )

  private$workers <- rbind(private$workers, new_workers)
  invisible()
}

wp_add_task <- function(self, private, func, args, id, event_loop) {
  private$tasks <- rbind(
    private$tasks,
    data.frame(
      stringsAsFactors = FALSE,
      event_loop = event_loop, id = id, func = I(list(func)),
      args = I(list(args)), status = "waiting", result = I(list(NULL)))
  )

  private$try_start()
  invisible()
}

## We only need to poll the sessions that actually do something...

wp_get_fds <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  private$workers$fd[sts %in% c("starting", "busy")]
}

wp_get_pids <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  private$workers$pid[sts %in% c("starting", "busy")]
}

wp_get_poll_connections <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  busy <- sts %in% c("starting", "busy")
  structure(
    lapply(private$workers$session[busy],
           function(x) x$get_poll_connection()),
    names = private$workers$pid[busy])
}

wp_notify_event <- function(self, private, pids, event_loop) {
  done <- NULL
  dead <- integer()
  which <- match(pids, private$workers$pid)
  for (w in which) {
    msg <- private$workers$session[[w]]$read()
    if (is.null(msg)) next
    if (msg$code == 200 || (msg$code >= 500 && msg$code < 600)) {
      if (msg$code >= 500 && msg$code < 600) dead <- c(dead, w)
      wt <- match(private$workers$task[[w]], private$tasks$id)
      if (is.na(wt)) stop("Internal error, no such task")
      private$tasks$result[[wt]] <- msg
      private$tasks$status[[wt]] <- "done"
      private$workers$task[[w]] <- NA_character_
      done <- c(done, private$tasks$id[[wt]])
    }
  }
  if (length(dead)) {
    private$workers <- private$workers[-dead,]
    self$start_workers()
  }

  private$try_start()

  done
}

worker_pool_size <- function() {
  getOption("async.worker_pool_size") %||%
    as.integer(Sys.getenv("ASYNC_WORKER_POOL_SIZE", 4))
}

wp_kill_workers <- function(self, private) {
  lapply(private$workers$session, function(x) x$kill())
  private$workers <- NULL
  invisible()
}

wp_cancel_task <- function(self, private, id) {
  wt <- match(id, private$tasks$id)
  if (is.na(wt)) stop("Unknown task")

  if (private$tasks$status[[wt]] == "running") {
    wk <- match(id, private$workers$task)
    if (!is.na(wk)) private$interrupt_worker(private$workers$pid[wk])
  }
  private$tasks <- private$tasks[-wt, ]
  invisible()
}

wp_cancel_all_tasks <- function(self, private) {
  stop("`cancel_all_tasks` method is not implemented yet")
}

wp_get_result <- function(self, private, id) {
  wt <- match(id, private$tasks$id)
  if (is.na(wt)) stop("Unknown task")

  if (private$tasks$status[[wt]] != "done") stop("Task not done yet")
  result <- private$tasks$result[[wt]]
  private$tasks <- private$tasks[-wt, ]
  result
}

wp_list_workers <- function(self, private) {
  private$workers[, setdiff(colnames(private$workers), "session")]
}

wp_list_tasks <- function(self, private, event_loop, status) {
  dont_show <- c("func", "args", "result")
  ret <- private$tasks
  if (!is.null(event_loop)) ret <- ret[ret$event_loop %in% event_loop, ]
  if (!is.null(status)) ret <- ret[ret$status %in% status, ]
  ret[, setdiff(colnames(private$tasks), dont_show)]
}

## Internals -------------------------------------------------------------

#' @importFrom utils head

wp__try_start <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  if (all(sts != "idle")) return()
  can_work <- sts == "idle"

  can_run <- private$tasks$status == "waiting"
  num_start <- min(sum(can_work), sum(can_run))
  will_run <- head(which(can_run), num_start)
  will_work <- head(which(can_work), num_start)

  for (i in seq_along(will_run)) {
    wt <- will_run[[i]]
    ww <- will_work[[i]]
    func <- private$tasks$func[[wt]]
    args <- private$tasks$args[[wt]]
    private$workers$session[[ww]]$call(func, args)
    private$tasks$status[[wt]] <- "running"
    private$workers$task[[ww]] <- private$tasks$id[[wt]]
  }

  invisible()
}

#' Interrupt a worker process
#'
#' We need to make sure that the worker is in a usable state after this.
#'
#' For speed, we try to interrupt with a SIGINT first, and if that does
#' not work, then we kill the worker and start a new one.
#'
#' When we interrupt with a SIGINT a number of things can happen:
#' 1. we successfully interrupted a computation, then
#'    we'll just poll_io(), and read() and we'll get back an
#'    interrupt error.
#' 2. The computation has finished, so we did not interrupt it.
#'    In this case the background R process will apply the interrupt
#'    to the next computation (at least on Unix) so the bg process
#'    needs to run a quick harmless call to absorb the interrupt.
#'    We can use `Sys.sleep()` for this, and `write_input()` directly
#'    for speed and simplicity.
#' 3. The process has crashed already, in this case `interrupt()` will
#'    return `FALSE`. `poll_io()` will return with "ready" immediately,
#'    `read()` will return with an error, and `write_input()` throws
#'    an error.
#' 4. We could not interrupt the process, because it was in a
#'    non-interruptable state. In this case we kill it, and start a
#'    new process. `poll_io()` will not return with "ready" in this case.
#'
#' @param self self
#' @param private private self
#' @param pid pid of process
#' @noRd

wp__interrupt_worker <- function(self, private, pid) {
  ww <- match(pid, private$workers$pid)
  if (is.na(ww)) stop("Unknown task in interrupt_worker() method")

  kill <- FALSE
  sess <- private$workers$session[[ww]]
  int <- sess$interrupt()
  pr <- sess$poll_io(100)["process"]

  if (pr == "ready") {
    msg <- sess$read()
    if (! inherits(msg, "interrupt")) {
      tryCatch({
        sess$write_input("base::Sys.sleep(0)\n")
        sess$read_output()
        sess$read_error()
      }, error = function(e) kill <<- TRUE)
    }
    private$workers$task[[ww]] <- NA_character_
  } else {
    kill <- TRUE
  }

  if (kill) {
    sess$close()
    private$workers <- private$workers[-ww, ]
    ## Make sure that we have enough workers running
    self$start_workers()
  }

  invisible()
}

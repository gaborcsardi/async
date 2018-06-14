
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
    notify_event = function(fds, event_loop)
      wp_notify_event(self, private, fds, event_loop),
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
    list_tasks = function()
      wp_list_tasks(self, private),
    finalize = function() self$kill_workers()
  ),

  private = list(
    workers = list(),
    tasks = list(),

    try_start = function(event_loop)
      wp__try_start(self, private, event_loop),
    interrupt_worker = function(pid)
      wp__interrupt_worker(self, private, pid)
  )
)

wp_init <- function(self, private) {
  self$start_workers()
  invisible(self)
}

#' @importFrom callr r_session
#' @importFrom processx conn_get_fileno

wp_start_workers <- function(self, private) {
  num <- worker_pool_size()

  ## See if we need to start more
  if (NROW(private$workers) >= num) return(invisible())

  ## Yeah, start some more
  to_start <- num - NROW(private$workers)
  sess <- lapply(1:to_start, function(x) r_session$new(wait = FALSE))
  fd <- viapply(sess, function(x) conn_get_fileno(x$get_poll_connection()))
  new_workers <- data.frame(
    stringsAsFactors = FALSE,
    session = I(sess),
    task = NA_character_,
    pid = viapply(sess, function(x) x$get_pid()),
    fd = fd
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

  private$try_start(event_loop)
  invisible()
}

## We only need to poll the sessions that actually do something...

wp_get_fds <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  private$workers$fd[sts %in% c("starting", "busy")]
}

wp_notify_event <- function(self, private, fds, event_loop) {
  wfds <- private$workers$fd
  if (any(! fds %in% wfds)) {
    warning("Internal error, unknown worker fds ignored")
    fds <- intersect(wfds, fds)
  }

  done <- NULL
  dead <- integer()
  wh <- match(fds, wfds)
  for (w in wh) {
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

  private$try_start(event_loop)

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

wp_list_tasks <- function(self, private) {
  dont_show <- c("func", "args", "result")
  private$tasks[, setdiff(colnames(private$tasks), dont_show)]
}

## Internals -------------------------------------------------------------

#' @importFrom utils head

wp__try_start <- function(self, private, event_loop) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  if (all(sts != "idle")) return()
  can_work <- sts == "idle"

  can_run <- private$tasks$status == "waiting" &
    private$tasks$event_loop == event_loop
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

wp__interrupt_worker <- function(self, private, pid) {
  ww <- match(pid, private$workers$pid)
  if (is.na(ww)) stop("Unknown task in interrupt_worker() method")

  sess <- private$workers$session[[ww]]
  sess$interrupt()
  pr <- sess$poll_io(300)["process"]
  if (pr == "ready") {
    sess$read()
  } else {
    sess$close()
  }

  private$workers$task[[ww]] <- NA_character_

  ## Make sure that we have enough workers running
  self$start_workers()

  invisible()
}

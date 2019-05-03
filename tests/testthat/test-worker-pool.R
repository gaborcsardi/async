
context("worker pool")

test_that("simple CRUD", {
  skip_on_cran()

  withr::with_options(c(async.worker_pool_size = 4), {
    wp <- worker_pool$new()
    on.exit(wp$kill_workers())
    wl <- wp$list_workers()
    expect_equal(nrow(wl), 4)

    ## Schedule some work, it will not start until a process is ready
    for (i in 1:10) {
      wp$add_task(function() Sys.getpid(), list(), as.character(i), 42)
    }

    ## Wait until they have started
    conns <- wp$get_poll_connections()
    expect_equal(length(conns), 4)

    ready <- NULL
    while (!identical(unlist(ready, use.names = FALSE), rep("ready", 4))) {
      ready <- processx::poll(conns, 1000L)
    }

    ## Let the queue know that they have started
    ## Returns NULL because nothing has finished
    expect_null(wp$notify_event(names(conns), event_loop = 42))

    ## Now four workers must be busy
    expect_equal(is.na(wp$list_workers()$task), rep(FALSE, 4))

    ## Check for result
    pids <- wp$get_pids()
    expect_equal(length(pids), 4)
    ready <- NULL
    while (!identical(unlist(ready, use.names = FALSE), rep("ready", 4))) {
      ready <- processx::poll(conns, 1000L)
    }

    ## Results are in, four more tasks should be queued
    expect_equal(
      sort(wp$notify_event(pids = pids, event_loop = 42)),
      as.character(1:4))
    res <- lapply(as.character(1:4), function(i) wp$get_result(i))
    pids <- viapply(res, "[[", "result")
    expect_equal(pids, wp$list_workers()$pid)
    expect_equal(sum(wp$list_tasks()$status == "running"), 4)

    ## Cancel the rest
    for (i in 5:10) wp$cancel_task(as.character(i))

    ## Workers are idle, no tasks
    expect_equal(is.na(wp$list_workers()$task), rep(TRUE, 4))
    expect_equal(NROW(wp$list_tasks()), 0)

    wp$kill_workers()
    expect_equal(NROW(wp$list_workers()), 0)
  })
})

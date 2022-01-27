
context("timer")

test_that("keeps event loop alive", {

  counter <- 0L
  do <- function() {
    cb <- function() {
      counter <<- counter + 1L
      if (counter == 3L) t$cancel()
    }
    t <- async_timer$new(1/100, cb)
    "done"
  }

  start <- Sys.time()
  res <- run_event_loop(do())
  end <- Sys.time()

  expect_null(res)
  expect_true(end - start >= as.difftime(3/100, units = "secs"))
  expect_true(end - start <= as.difftime(2, units = "secs"))
})

test_that("errors", {

  counter <- 0L
  do <- function() {
    cb <- function() {
      counter <<- counter + 1L
      if (counter == 2L) stop("foobar")
      if (counter == 3L) t$cancel()
    }
    t <-  async_timer$new(1/100, cb)
  }

  expect_error(run_event_loop(do()), "foobar")
  expect_equal(counter, 2L)

  counter <- 0L
  error <- NULL
  do <- function() {
    cb <- function() {
      counter <<- counter + 1L
      if (counter == 2L) stop("foobar")
      if (counter == 3L) t$cancel()
    }
    t <-  async_timer$new(1/100, cb)
    t$listen_on("error", function(err) error <<- err)
  }

  expect_silent(run_event_loop(do()))
  expect_equal(counter, 3L)
  expect_s3_class(error, "error")
  expect_equal(conditionMessage(error), "foobar")
})

test_that("mixing deferred and timers", {

  counter <- 0L
  do <- function(s) {
    counter <<- 0L
    cb <- function() {
      counter <<- counter + 1L
      if (counter == 3L) t$cancel()
    }
    t <- async_timer$new(.6, cb)
    delay(s)$then(function() "OK")
  }

  ## Once we have the output, we quit
  start <- Sys.time()
  res <- synchronise(do(1))
  end <-  Sys.time()

  expect_equal(res, "OK")
  expect_true(end - start >= as.difftime(1, units = "secs"))
  expect_true(end - start <= as.difftime(2, units = "secs"))
  expect_equal(counter, 1L)

  ## Run the timer to the end
  start <- Sys.time()
  res <- synchronise(do(3))
  end <-  Sys.time()

  expect_equal(res, "OK")
  expect_true(end - start >= as.difftime(1/5, units = "secs"))
  expect_true(end - start <= as.difftime(5, units = "secs"))
  expect_equal(counter, 3L)
})

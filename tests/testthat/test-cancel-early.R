
context("early cancellation")

test_that("auto-cancellation", {

  httpx <- NULL
  idx <- 0

  do <- async(function() {
    req_done <- 0L

    response_time <- async(function(url) {
      idx <<- idx + 1
      httpx[[idx]] <<- http_head(url)
      httpx[[idx]]$
        then(function(x) { req_done <<- req_done + 1L ; x })$
        then(http_stop_for_status)$
        then(~ setNames(.[["times"]][["total"]], url))$
        catch(~ setNames(Inf, url))
    })

    urls <- http$url(c("/delay/5", "/get"))
    reqs <- lapply(urls, response_time)
    when_any(.list = reqs)
  })

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()
  expect_true(toc - tic < as.difftime(2, units = "secs"))
  expect_true(get_private(httpx[[1]])$cancelled)
})

test_that("detect, if one is done", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ TRUE)
      dx <<- c(dx, list(nx))
      nx
    }
    async_detect(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  synchronise(do())
  toc <- Sys.time()

  expect_equal(pinged, 1L)
  expect_true(get_private(dx[[2]])$cancelled)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("detect, if one errors",  {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ stop("foobar"))
      dx <<- c(dx, list(nx))
      nx
    }
    async_detect(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_true(get_private(dx[[2]])$cancelled)
  expect_equal(conditionMessage(err), "foobar")
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("every, if one is FALSE", {
  pinged <- 0L
  dx <-  list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ FALSE)
      dx <<- c(dx, list(nx))
      nx
    }
    async_every(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  synchronise(do())
  toc <- Sys.time()
  expect_true(get_private(dx[[2]])$cancelled)
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("every, if one errors", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ stop("foobar"))
      dx <<- c(dx, list(nx))
      nx
    }
    async_every(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_true(get_private(dx[[2]])$cancelled)
  expect_equal(conditionMessage(err), "foobar")
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("filter, if one errors", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ stop("foobar"))
      dx <<- c(dx, list(nx))
      nx
    }
    async_filter(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_true(get_private(dx[[2]])$cancelled)
  expect_equal(conditionMessage(err), "foobar")
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("map, if one errors", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ stop("foobar"))
      dx <<- c(dx, list(nx))
      nx
    }
    async_map(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_true(get_private(dx[[2]])$cancelled)
  expect_equal(conditionMessage(err), "foobar")
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("some, if one is TRUE", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ TRUE)
      dx <<- c(dx, list(nx))
      nx
    }
    async_some(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  synchronise(do())
  toc <- Sys.time()
  expect_equal(pinged, 1L)
  expect_true(get_private(dx[[2]])$cancelled)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("every, if one errors", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ stop("foobar"))
      dx <<- c(dx, list(nx))
      nx
    }
    async_some(c(1/1000, 5), f)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_true(get_private(dx[[2]])$cancelled)
  expect_equal(conditionMessage(err), "foobar")
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("when_all, if one errors", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ stop("foobar"))
      dx <<- c(dx, list(nx))
      nx
    }
    defs <- lapply(c(1/1000, 5, 5), f)
    when_all(.list = defs)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_true(get_private(dx[[2]])$cancelled)
  expect_true(get_private(dx[[3]])$cancelled)
  expect_equal(conditionMessage(err), "foobar")
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("when_some, if enough are done", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ "yep")
      dx <<- c(dx, list(nx))
      nx
    }
    defs <- lapply(c(1/1000, 5, 1/1000, 5), f)
    when_some(2, .list = defs)
  }

  tic <- Sys.time()
  res <- synchronise(do())
  toc <- Sys.time()

  expect_equal(res, list("yep", "yep"))
  expect_true(get_private(dx[[2]])$cancelled)
  expect_true(get_private(dx[[4]])$cancelled)
  expect_equal(pinged, 2L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("when_some, if some error", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ stop("foobar"))
      dx <<- c(dx, list(nx))
      nx
    }
    defs <- lapply(c(5, 1/1000, 1/1000, 5), f)
    when_some(3, .list = defs)
  }

  tic <- Sys.time()
  err <- tryCatch(synchronise(do()), error = identity)
  toc <- Sys.time()

  expect_s3_class(err, "async_rejected")
  expect_true(get_private(dx[[1]])$cancelled)
  expect_true(get_private(dx[[4]])$cancelled)
  expect_equal(conditionMessage(err), "when_some / when_any failed")
  expect_equal(length(err$errors), 2)
  expect_equal(conditionMessage(err$errors[[1]]), "foobar")
  expect_equal(conditionMessage(err$errors[[2]]), "foobar")
  expect_equal(pinged, 2L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

test_that("when_any, if one is done", {
  pinged <- 0L
  dx <- list()
  do <- function() {
    f <- function(n) {
      force(n)
      nx <- delay(n)$
        then(function() pinged <<- pinged + 1)$
        then(~ "yep")
      dx <<- c(dx, list(nx))
      nx
    }
    defs <- lapply(c(5, 5, 1/1000, 5), f)
    when_any(.list = defs)
  }

  tic <- Sys.time()
  res <- synchronise(do())
  toc <- Sys.time()

  expect_equal(res, "yep")
  expect_true(get_private(dx[[1]])$cancelled)
  expect_true(get_private(dx[[2]])$cancelled)
  expect_true(get_private(dx[[4]])$cancelled)
  expect_equal(pinged, 1L)
  expect_true(toc - tic < as.difftime(4.5, units = "secs"))
})

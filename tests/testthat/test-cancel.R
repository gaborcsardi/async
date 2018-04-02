
context("cancellation")

test_that("on_cancel callback is called", {

  dx <- NULL
  cancelled <- FALSE
  cancel_msg <- NULL
  do <- async(function() {
    dx <<- deferred$new(
      action = function(resolve) { },
      on_cancel = function(msg) {
        cancelled <<- TRUE
        cancel_msg <<- msg
      })
    dx$cancel("changed my mind")
  })

  err <- tryCatch(synchronise(do()), error = function(e) e)
  expect_match(conditionMessage(err), "changed my mind")
  expect_s3_class(err, "async_cancelled")
  expect_equal(get_private(dx)$state, "rejected")
  expect_true(cancelled)
  expect_match(cancel_msg, "changed my mind")
})

test_that("then() is also rejected on cancel", {

  dx <- dx2 <- NULL
  do <- async(function() {
    dx <<- deferred$new(action = function(resolve) { })
    dx2 <<- dx$then(function() "not this far")
    dx$cancel("changed my mind")
    dx2
  })

  err <- tryCatch(synchronise(do()), error = function(e) e)
  expect_match(conditionMessage(get_private(dx)$value), "changed my mind")
  expect_match(conditionMessage(get_private(dx2)$value), "changed my mind")
  expect_s3_class(get_private(dx)$value, "async_cancelled")
  expect_s3_class(get_private(dx2)$value, "async_cancelled")
  expect_equal(get_private(dx2)$state, "rejected")
  expect_equal(get_private(dx)$state, "rejected")
})

test_that("can catch and handle cancellation", {

  err <- NULL
  do <- async(function() {
    dx <- deferred$new(action = function(resolve) { })
    dx2 <- dx$catch(error = function(e) err <<- e)
    dx$cancel("changed my mind")
    dx2
  })

  synchronise(do())
  expect_s3_class(err, "async_cancelled")
  expect_match(conditionMessage(err), "changed my mind")
})

test_that("cancel delay", {

  do <- function() {
    d1 <- delay(60)
    d1$cancel()
  }
  tic <- Sys.time()
  expect_error(synchronise(do()), "Cancelled")
  tac <- Sys.time()
  expect_true(tac - tic < as.difftime(30, units  =  "secs"))
})

test_that("cancel delay after it has started", {

  cancelled <- NULL
  do <- function() {
    d1 <- delay(5)
    d1x <- d1$catch(error = identity)
    d2 <- delay(1/100)$
      then(function() { d1$cancel("nope"); "OK" })
    when_all(d1x, d2)
  }

  tic <- Sys.time()
  res <- synchronise(do())
  tac <- Sys.time()

  expect_s3_class(res[[1]], "async_cancelled")
  expect_equal(conditionMessage(res[[1]]), "nope")
  expect_equal(res[[2]], "OK")
  expect_true(tac - tic < as.difftime(4, units  =  "secs"))
})

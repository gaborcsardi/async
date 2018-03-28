
context("cancellation")

test_that("on_cancel callback is called", {
  do <- async(function() {
    cancelled <- FALSE
    cancel_msg <- NULL
    dx <- deferred$new(
      action = function(resolve, reject) { },
      on_cancel = function(msg) {
        cancelled <<- TRUE
        cancel_msg <<- msg
      }
    )

    all <- when_all(
      dx$catch(function(e) expect_match(e$message, "changed my mind")),
      dx$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx$catch(~ expect_equal(get_private(dx)$state, "rejected")),
      dx$catch(~ expect_true(cancelled)),
      dx$catch(~ expect_match(cancel_msg, "changed my mind"))
    )

    dx$cancel("changed my mind")

    all
  })

  synchronise(do())
})

test_that("then() is also rejected on cancel", {
  do <- async(function() {
    dx <- deferred$new(action = function(resolve, reject) { })
    dx2 <- dx$then(function() "not this far")

    all <- when_all(
      dx2$catch(function(e) expect_match(e$message, "changed my mind")),
      dx$catch(function(e) expect_match(e$message, "changed my mind")),
      dx2$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx2$catch(~ expect_equal(get_private(dx2)$state, "rejected")),
      dx$catch(~ expect_equal(get_private(dx)$state, "rejected"))
    )

    dx$cancel("changed my mind")

    all
  })
  synchronise(do())
})

test_that("can catch and handle cancellation", {

  do <- async(function() {
    dx <- deferred$new(action = function(resolve, reject) { })
    all <- when_all(
      dx$catch(function(e) expect_s3_class(e, "async_cancelled")),
      dx$catch(function(e) expect_match(e$message, "changed my mind"))
    )

    dx$cancel("changed my mind")

    all
  })
  synchronise(do())
})

test_that("cancel delay", {

  do <- function() {
    d1 <- delay(60)
    d1$cancel()
  }
  tic <- Sys.time()
  synchronise(do())
  tac <- Sys.time()
  expect_true(tac - tic < as.difftime(30, units  =  "secs"))
})

test_that("cancel delay after it has started", {

  cancelled <- NULL
  do <- function() {
    d1 <- delay(5)
    d1x <- d1$catch(identity)
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


context("errors")

test_that("rejection", {

  do <- async(function() {
    dx <- delay(1/10000)$
      then(~ stop("ohno!"))$
      catch(~ expect_match(.$message, "ohno!"))
  })
  synchronise(do())
})

test_that("error propagates", {

  do <- async(function() {
    called <- FALSE
    dx <- delay(1/10000)$
      then(~ .)$
      then(~ stop("ohno!"))$
      then(function(x) called <<- TRUE)

    dx$
      catch(~ expect_match(.$message, "ohno!"))$
      then(function(x) expect_false(called))
  })
  synchronise(do())
})

test_that("handled error is not an error any more", {

  do <- async(function() {
    delay(1/10000)$
      then(function(x) stop("ohno!"))$
      catch(function(x) "OK")$
      then(~ expect_equal(., "OK"))$
      catch(~ stop("not called"))
  })
  synchronise(do())
})

test_that("catch", {
  do <- async(function() {
    dx <- delay(1/1000)$
      then(~ .)$
      then(~ stop("ooops"))$
      then(~ "not this one")$
      catch(~ "nothing to see here")
  })
  expect_equal(
    synchronise(do()),
    "nothing to see here"
  )
})

test_that("finally", {
  called <- FALSE
  do <- async(function() {
    delay(1/1000)$
      then(~ .)$
      then(~ stop("oops"))$
      then(~ "not this one")$
      finally(function() called <<- TRUE)
  })
  expect_error(synchronise(do()), "oops")
  expect_true(called)

  called <- FALSE
  do <- async(function() {
    delay(1/1000)$
      then(~ .)$
      then(~ "this one")$
      finally(function() called <<- TRUE)
  })
  expect_equal(synchronise(do()), "this one")
  expect_true(called)
})

test_that("error in action, lazy", {
  do <- function() {
    deferred$new(lazy = TRUE, function(resolve, reject) stop("foobar"))
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("error in action, eager", {
  do <- function() {
    deferred$new(lazy = FALSE, function(resolve, reject) stop("foobar"))
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("error in then function", {
  do <- function() {
    delay(1/100)$then(function(x) stop("foobar"))
  }

  err <- tryCatch(synchronise(do()), error = identity)
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("can catch error in action, lazy", {
  do <- function() {
    deferred$new(lazy = TRUE, function(resolve, reject) stop("foobar"))$
      catch(function(e) e)
  }

  err  <- synchronise(do())
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("can catch error in action, eager", {
  do <- function() {
    deferred$new(lazy = FALSE, function(resolve, reject) stop("foobar"))$
      catch(function(e) e)
  }

  err  <- synchronise(do())
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

test_that("can catch error in then function", {
  do <- function() {
    delay(1/100)$
      then(function(x) stop("foobar"))$
      catch(function(e) e)
  }

  err <- synchronise(do())
  expect_s3_class(err, "async_rejected")
  expect_match(conditionMessage(err), "foobar")
})

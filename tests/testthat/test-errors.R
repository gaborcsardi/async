
context("errors")

test_that("rejection", {

  dx <- delay(1/10000)$
    then(function(value) stop("ohno!"))

  expect_error(wait_for(dx), "ohno!")
})

test_that("error propagates", {
  called <- FALSE
  dx <- delay(1/10000)$
    then(function(x) x)$
    then(function(x) stop("ohno!"))$
    then(function(x) called <<- TRUE)

  expect_error(wait_for(dx, "ohno!"))
  expect_false(called)
})

test_that("handled error is not an error any more", {

  dx <- delay(1/10000)$
    then(function(x) stop("ohno!"))$
    then(NULL, function(x) "OK")

  expect_silent(wait_for(dx))
  expect_equal(wait_for(dx), "OK")
})

test_that("catch", {
  dx <- delay(1/1000)$
    then(~ .)$
    then(~ stop("ooops"))$
    then(~ "not this one")$
    catch(~ "nothing to see here")

  expect_equal(wait_for(dx), "nothing to see here")
})

test_that("finally", {
  called <- FALSE
  dx <- delay(1/1000)$
    then(~ .)$
    then(~ stop("oops"))$
    then(~ "not this one")$
    finally(function() called <<- TRUE)

  expect_error(wait_for(dx), "oops")
  expect_true(called)

  called <- FALSE
  dx <- delay(1/1000)$
    then(~ .)$
    then(~ "this one")$
    finally(function() called <<- TRUE)

  expect_equal(wait_for(dx), "this one")
  expect_true(called)
})

test_that("errors from other resolutions are not reported", {

  dx1 <- delay(1/10000)$then(~ stop("wrong"))
  dx2 <- delay(1/10)$then(~ "OK")

  expect_equal(wait_for(dx2), "OK")
  expect_equal(dx1$get_state(), "rejected")
  expect_error(wait_for(dx1), "wrong")

  dx1 <- delay(1/10000)$then(~ stop("wrong"))
  dx2 <- delay(1/10)$then(~ stop("oops"))

  expect_error(wait_for(dx2), "oops")
  expect_equal(dx1$get_state(), "rejected")
  expect_error(wait_for(dx1), "wrong")
})
